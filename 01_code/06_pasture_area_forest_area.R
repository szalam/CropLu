rm(list=ls())

library(ggplot2)

wd = list()
wd$data = 'D:/CropLu/02_data/'
wd$figure3 = 'D:/CropLu/03_figure/'

setwd(wd$data)

df = read.csv('pasture_land_fallow_land.csv')
head(df)
colnames(df) = c('Year'   ,'c.area' ,'type')

df$c.area = df$c.area/1000#/2.4711
# df$past.grass = df$past.grass/1000

summary(df)



p = ggplot(data=df, aes(x=Year, y=c.area, group=type)) +
  geom_point(aes(color=type),size=2)+
  geom_line(aes(color=type),size=.5)+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 6000, by = 500))+
  scale_x_continuous(breaks = seq(2007, 2016, by = 2))+
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab('Area (x1000 hectares)')+ xlab('Year')+
  geom_smooth(aes(color = type),method='lm',se=F,linetype='dashed')+
  scale_color_manual(values = c("darkgreen", "purple"))
# theme(axis.text.x=element_blank())+ 
# theme(axis.title.x = element_blank(),
#       axis.title.y = element_blank())


p

ggsave(p,filename=paste0(wd$figure3,"pasture_forest_area",".png",sep=""),
       width = 19.15, height = 10, units = "cm")
