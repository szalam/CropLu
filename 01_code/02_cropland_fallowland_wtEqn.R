rm(list=ls())

library(ggplot2)

wd = list()
wd$data = 'D:/CropLu/02_data/'
wd$figure3 = 'D:/CropLu/03_figure/'

setwd(wd$data)

df = read.csv('fallow_and_cropland.csv')
head(df)
colnames(df) = c('Region' ,'Year'   ,'fallow_c' ,'total_c')

df$fallow_c = df$fallow_c/1000/2.4711
df$total_c = df$total_c/1000/2.4711

summary(df)

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  # eq <- substitute(italic(y) == a + (b) %.% italic(x)*","~~italic(R)^2~"="~r2, 
  if(coef(m)[2]<0){
    eq <- substitute(italic(y) == a - b %.% italic(x), 
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]*(-1)), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 2)))
    return(as.character(as.expression(eq)))
  }
  if(coef(m)[2]>=0){
    eq <- substitute(italic(y) == a + b %.% italic(x), 
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 2)))
    return(as.character(as.expression(eq)))
  }
}
lm_eqn2 <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(R)^2~"="~r2, 
                   list(r2 = format(round(summary(m)$r.squared,2), nsmall = 2)))
  return(as.character(as.expression(eq)))
}

reg.uniq = unique(df$Region)

for(i in 1:length(reg.uniq)){
  
  df.tmp = df[df$Region==reg.uniq[i],]
  p = ggplot(data=df.tmp, aes(x=Year, y=total_c)) +
    geom_point(aes(color=Region),size=2)+
    geom_line(aes(color=Region),size=.5)+
    theme_bw()+
    scale_y_continuous(breaks = seq(0, 8000, by = 1000))+
    scale_x_continuous(breaks = seq(2007, 2016, by = 2))+
    theme(text = element_text(size=16),
          axis.text.x = element_text(angle=0, hjust=.5),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))+
    ylab('Active cropland (x1000 hectares)')+ xlab('Year')+
    geom_smooth(aes(color = Region),method='lm',se=F,linetype='dashed')+
    scale_color_manual(values = c("black", "blue", "red"))
  # theme(axis.text.x=element_blank())+ 
  # theme(axis.title.x = element_blank(),
  #       axis.title.y = element_blank())
  
  max_min_diff = (max(df.tmp$total_c) - min(df.tmp$total_c))/8
  df.tmp2 = data.frame(x=df.tmp$Year,y=(df.tmp$total_c*1000))
  p=p + geom_text(x = 2010, y = (max(df.tmp$total_c)*.9), label = lm_eqn(df.tmp2), parse = TRUE,size = 2.3)+
    geom_text(x = 2010, y = (max(df.tmp$total_c)*1.04-max_min_diff), label = lm_eqn2(df.tmp2), parse = TRUE,size = 2.3)
  
  
  p
  
  ggsave(p,filename=paste0(wd$figure3,"total_crop_series_",reg.uniq[i],".png",sep=""),
         width = 20, height = 10, units = "cm")
}

for(i in 1:length(reg.uniq)){
  
  df.tmp = df[df$Region==reg.uniq[i],]
  p=ggplot(data=df.tmp, aes(x=Year, y=fallow_c)) +
    geom_point(aes(color=Region),size=2)+
    geom_line(aes(color=Region),size=.5)+
    theme_bw()+
    # scale_y_continuous(breaks = seq(200, 800, by = 100))+
    scale_x_continuous(breaks = seq(2007, 2016, by = 2))+
    theme(text = element_text(size=16),
          axis.text.x = element_text(angle=0, hjust=.5),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))+
    ylab('Fallowed area (x1000 hectares)')+ xlab('Year')+
    geom_smooth(aes(color = Region),method='lm',se=F,linetype='dashed')+
    scale_color_manual(values = c("black", "blue", "red"))
  # theme(axis.text.x=element_blank())+ 
  # theme(axis.title.x = element_blank(),
  #       axis.title.y = element_blank())
  
  max_min_diff = (max(df.tmp$fallow_c) - min(df.tmp$fallow_c))/8
  df.tmp2 = data.frame(x=df.tmp$Year,y=(df.tmp$fallow_c*1000))
  p=p + geom_text(x = 2010, y = (max(df.tmp$fallow_c)*.9), label = lm_eqn(df.tmp2), parse = TRUE,size = 2.3)+
    geom_text(x = 2010, y = (max(df.tmp$fallow_c)*1.04-max_min_diff), label = lm_eqn2(df.tmp2), parse = TRUE,size = 2.3)
  
  
  p
  
  ggsave(p,filename=paste0(wd$figure3,"fallow_crop_series_",reg.uniq[i],".png",sep=""),
         width = 20, height = 10, units = "cm")
}
