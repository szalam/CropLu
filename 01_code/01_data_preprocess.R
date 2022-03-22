rm(list=ls())

library(ggplot2)

wd = list()
wd$data = 'D:/CropLu/02_data/'
wd$figure = 'D:/CropLu/02_data/figure/'

setwd(wd$data)

df = read.csv('data-for-plotting2.csv')
head(df)
colnames(df) = c('Crop' ,'Year'   ,'Acreage' ,'Yield' ,'Price')

df$Acreage = df$Acreage/1000/2.4711
df$Price = df$Price/0.907185
df$Yield = df$Yield * 2.4711*0.907185
summary(df)




p = ggplot(data=df, aes(x=Year, y=Price, group=Crop)) +
  geom_point(aes(color=Crop),size=2)+
  geom_line(aes(color=Crop),size=.5)+
  theme_bw()+
  # scale_y_continuous(breaks = seq(200, 800, by = 100))+
  scale_x_continuous(breaks = seq(2007, 2016, by = 1))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab('Price ($/Metric Ton)')+ xlab('Year')+
  geom_smooth(aes(color = Crop),method='lm',se=F,linetype='dashed')+
  theme(axis.text.x=element_blank())+ 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


p

ggsave(p,filename=paste0(wd$figure,"Price_series",".png",sep=""),
       width = 20, height = 15, units = "cm")


p2 = ggplot(data=df, aes(x=Year, y=Yield, group=Crop)) +
  geom_point(aes(color=Crop),size=2)+
  geom_line(aes(color=Crop),size=.5)+
  theme_bw()+
  # scale_y_continuous(breaks = seq(200, 800, by = 100))+
  scale_x_continuous(breaks = seq(2007, 2016, by = 1))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab('Yield (Metric Ton/Hectare )')+ xlab('Year')+
  geom_smooth(aes(color = Crop),method='lm',se=F,linetype='dashed')+
  theme(axis.text.x=element_blank())+ 
  theme(axis.title.x = element_blank(),
    axis.title.y = element_blank())


ggsave(p2,filename=paste0(wd$figure,"Yield_series",".png",sep=""),
       width = 20, height = 15, units = "cm")


crop.uniq = unique(df$Crop)
#yield plot for separate crops

lm_eqn <- function(df){
  df = df[!is.na(df$y),]
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

for(i in 1:length(crop.uniq)){
  df.tmp = df[df$Crop==crop.uniq[i],]
  df.bt = data.frame(x=df.tmp$Year,y=df.tmp$Yield)
  
  p2 = ggplot(data=df.tmp, aes(x=Year, y=Yield)) +
    geom_point(color ='black',size=1.2)+
    geom_line(color ='black',size=.5)+
    theme_bw()+
    # scale_y_continuous(breaks = seq(200, 800, by = 100))+
    scale_x_continuous(breaks = seq(2007, 2016, by = 1))+
    theme(text = element_text(size=9),
          axis.text.x = element_text(angle=0, hjust=.5),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))+
    ylab('Yield (Metric Ton/Hectare)')+ xlab('Year')+
    geom_smooth(color ='black',method='lm',se=F,linetype='dashed',size=.5)+
    ylim(min(df.bt$y),(max(df.bt$y)*1.05))+
    theme(axis.text.x=element_blank())+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  
  max_min_diff = (max(df.bt$y) - min(df.bt$y))/8
  p2=p2 + geom_text(x = 2011, y = (max(df.bt$y)*1.04), label = lm_eqn(df.bt), parse = TRUE,size = 2.8)+
    geom_text(x = 2011, y = (max(df.bt$y)*1.04-max_min_diff), label = lm_eqn2(df.bt), parse = TRUE,size = 2.8)
  
  if(max(df.bt$y)>=0 & max(df.bt$y)<10){
    ggsave(p2,filename=paste0(wd$figure,"yield_cropwise/Yield_series_",crop.uniq[i],".png"),
           width =5, height = 4, units = "cm")}
  if(max(df.bt$y)>10 & max(df.bt$y)<100){
    ggsave(p2,filename=paste0(wd$figure,"yield_cropwise/Yield_series_",crop.uniq[i],".png"),
           width =5, height = 4, units = "cm")}
  if(max(df.bt$y)>=100 & max(df.bt$y)<1000){
    ggsave(p2,filename=paste0(wd$figure,"yield_cropwise/Yield_series_",crop.uniq[i],".png"),
           width =5.06, height = 4, units = "cm")}
  if(max(df.bt$y)>=1000 & max(df.bt$y)<10000){
    ggsave(p2,filename=paste0(wd$figure,"yield_cropwise/Yield_series_",crop.uniq[i],".png"),
           width =5.24, height = 4, units = "cm")}
  if(max(df.bt$y)>=10000 & max(df.bt$y)<100000){
    ggsave(p2,filename=paste0(wd$figure,"yield_cropwise/Yield_series_",crop.uniq[i],".png"),
           width =5.3, height = 4, units = "cm")}
  
}


for(i in 1:length(crop.uniq)){
  df.tmp = df[df$Crop==crop.uniq[i],]
  df.bt = data.frame(x=df.tmp$Year,y=df.tmp$Price)
  
  p2 = ggplot(data=df.tmp, aes(x=Year, y=Price)) +
    geom_point(color ='black',size=1.2)+
    geom_line(color ='black',size=.5)+
    theme_bw()+
    # scale_y_continuous(breaks = seq(200, 800, by = 100))+
    scale_x_continuous(breaks = seq(2007, 2016, by = 1))+
    theme(text = element_text(size=9),
          axis.text.x = element_text(angle=0, hjust=.5),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))+
    ylab('Price ($/Metric Ton)')+ xlab('Year')+
    geom_smooth(color ='black',method='lm',se=F,linetype='dashed',size=.5)+
    ylim(min(df.bt$y),(max(df.bt$y)*1.05))+
    theme(axis.text.x=element_blank())+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  
  max_min_diff = (max(df.bt$y,na.rm=T) - min(df.bt$y,na.rm=T))/8
  if(crop.uniq[i]=="Cotton"){
    p2=p2 + geom_text(x = 2011, y = (max(df.bt$y,na.rm=T)*1), label = lm_eqn(df.bt), parse = TRUE,size = 2.8)+
      geom_text(x = 2011, y = (max(df.bt$y,na.rm=T)*1-max_min_diff), label = lm_eqn2(df.bt), parse = TRUE,size = 2.8)
  }else{
  p2=p2 + geom_text(x = 2011, y = (max(df.bt$y,na.rm=T)*1.04), label = lm_eqn(df.bt), parse = TRUE,size = 2.8)+
    geom_text(x = 2011, y = (max(df.bt$y,na.rm=T)*1.04-max_min_diff), label = lm_eqn2(df.bt), parse = TRUE,size = 2.8)
  }
  
  if(max(df.bt$y,na.rm=T)>=0 & max(df.bt$y,na.rm=T)<10){
    ggsave(p2,filename=paste0(wd$figure,"price_cropwise/Price_series_",crop.uniq[i],".png"),
           width =4.8, height = 4, units = "cm")}
  if(max(df.bt$y,na.rm=T)>=10 & max(df.bt$y,na.rm=T)<100){
    ggsave(p2,filename=paste0(wd$figure,"price_cropwise/Price_series_",crop.uniq[i],".png"),
           width =5, height = 4, units = "cm")}
  if(max(df.bt$y,na.rm=T)>=100 & max(df.bt$y,na.rm=T)<1000){
    ggsave(p2,filename=paste0(wd$figure,"price_cropwise/Price_series_",crop.uniq[i],".png"),
           width =5.06, height = 4, units = "cm")}
  if(max(df.bt$y,na.rm=T)>=1000 & max(df.bt$y,na.rm=T)<10000){
    ggsave(p2,filename=paste0(wd$figure,"price_cropwise/Price_series_",crop.uniq[i],".png"),
           width =5.24, height = 4, units = "cm")}
  if(max(df.bt$y,na.rm=T)>=10000 & max(df.bt$y,na.rm=T)<100000){
    ggsave(p2,filename=paste0(wd$figure,"price_cropwise/Price_series_",crop.uniq[i],".png"),
           width =5.3, height = 4, units = "cm")}
  
}



for(i in 1:length(crop.uniq)){
  df.tmp = df[df$Crop==crop.uniq[i],]
  df.bt = data.frame(x=df.tmp$Year,y=df.tmp$Acreage)
  
  
  p2 = ggplot(data=df.tmp, aes(x=Year, y=Acreage)) +
    geom_point(color ='black',size=1.2)+
    geom_line(color ='black',size=.5)+
    theme_bw()+
    # scale_y_continuous(breaks = seq(200, 800, by = 100))+
    scale_x_continuous(breaks = seq(2007, 2016, by = 1))+
    theme(text = element_text(size=9),
          axis.text.x = element_text(angle=0, hjust=.5),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))+
    ylab('Area(Hectare)')+ xlab('Year')+
    geom_smooth(color ='black',method='lm',se=F,linetype='dashed',size=.5)+
    ylim(min(df.bt$y),(max(df.bt$y)*1.05))+
    theme(axis.text.x=element_blank())+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  max_min_diff = (max(df.bt$y) - min(df.bt$y))/8
  df.tmp = df.bt
  df.tmp[,2] = df.tmp[,2]*1000
  p2=p2 + geom_text(x = 2011, y = (max(df.bt$y)*1.04), label = lm_eqn(df.tmp), parse = TRUE,size = 2.8)+
    geom_text(x = 2011, y = (max(df.bt$y)*1.04-max_min_diff), label = lm_eqn2(df.tmp), parse = TRUE,size = 2.8)
  
  if(max(df.bt$y)>=0 & max(df.bt$y)<10){
    ggsave(p2,filename=paste0(wd$figure,"acre_cropwise/acre_series_",crop.uniq[i],".png"),
           width =4.8, height = 4, units = "cm")}
  if(max(df.bt$y)>=10 & max(df.bt$y)<100){
  ggsave(p2,filename=paste0(wd$figure,"acre_cropwise/acre_series_",crop.uniq[i],".png"),
         width =5, height = 4, units = "cm")}
  if(max(df.bt$y)>=100 & max(df.bt$y)<1000){
    ggsave(p2,filename=paste0(wd$figure,"acre_cropwise/acre_series_",crop.uniq[i],".png"),
           width =5.06, height = 4, units = "cm")}
  if(max(df.bt$y)>=1000 & max(df.bt$y)<10000){
    ggsave(p2,filename=paste0(wd$figure,"acre_cropwise/acre_series_",crop.uniq[i],".png"),
           width =5.24, height = 4, units = "cm")}
  if(max(df.bt$y)>=10000 & max(df.bt$y)<100000){
    ggsave(p2,filename=paste0(wd$figure,"acre_cropwise/acre_series_",crop.uniq[i],".png"),
           width =5.3, height = 4, units = "cm")}
  
}
