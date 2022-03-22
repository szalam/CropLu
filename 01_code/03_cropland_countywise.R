rm(list=ls())

library(ggplot2)
library(reshape2)

wd = list()
wd$data = 'D:/CropLu/02_data/'
wd$figure3 = 'D:/CropLu/02_data/3_cropland_countywide_figure/'

setwd(wd$data)

df = read.csv('Acreage_by_county_cropwise.csv')
head(df)
colnames(df)[1] = c('Year')

# df[,2:(ncol(df)-1)]= df[,2:(ncol(df)-1)]/1000

summary(df)

mdat = melt(df, id.var = c('Year','Type'))
head(mdat)
mdat$value = mdat$value/1000
colnames(mdat) = c('Year','C_type','County','Area')

# mdat = mdat[!is.na(mdat$Area),]
# mdat$County = as.character(mdat$County)
# mdat = mdat[order(mdat$County),]
# mdat$County = as.factor(mdat$County)

y= c('CROPLAND','FALLOW LAND','ALMONDS','ALFALFA','GRAPES','RICE','WINTER WHEAT','WALNUTS','COTTON','CORN','TOMATOES',
'PISTACHIOS','ORANGES','OATS')
mdat = mdat[order(match(mdat$C_type, y)),]
mdat$C_type = factor(as.character(mdat$C_type), levels = unique(mdat$C_type))


p = ggplot(data=mdat, aes(x=Year, y=Area, group=County)) +
  geom_point(aes(color=County),size=2)+
  geom_line(aes(color=County),size=.5)+
  theme_bw()+
  # scale_y_continuous(breaks = seq(0, 8000, by = 1000))+
  scale_x_continuous(breaks = seq(2007, 2016, by = 4))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
   facet_wrap(~C_type, nrow = 5,scales = 'free_y')+
   theme(strip.background = element_rect(fill="white"))
  # ylab('Total cropland (x1000 acre)')+ xlab('Year')+
  # geom_smooth(aes(color = County),method='lm',se=F,linetype='dashed')+
  # scale_color_manual(values = c("black", "blue", "red"))
# theme(axis.text.x=element_blank())+ 
# theme(axis.title.x = element_blank(),
#       axis.title.y = element_blank())


p

ggsave(p,filename=paste0(wd$figure3,"area_county_series",".png",sep=""),
       width = 25, height = 22, units = "cm")
