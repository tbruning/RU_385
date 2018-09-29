library(ggplot2)
library(datasauRus)
library(dplyr)
library(tidyr)
ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
  geom_point()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=3)
library(tibble)
dt <- as_tibble(datasaurus_dozen)
dt_grp <- group_by(datasaurus_dozen,dataset)

dt_meanx <- summarize(dt_grp,mx=mean(x))
dt_meany <- summarize(dt_grp,my=mean(y))
dt_sdx <- summarize(dt_grp,sdx=sd(x))
dt_sdy <- summarize(dt_grp,sdy=sd(y))
dt_corr <- summarize(dt_grp,cor=cor(x,y))
dt_meanx$mx <- round(dt_meanx$mx,2)
dt_meany$my <- round(dt_meany$my,2)
dt_sdx$sdx <- round(dt_sdx$sdx,2)
dt_sdy$sdy <- round(dt_sdy$sdy,2)
dt_corr$cor <- round(dt_corr$cor,2)
dt_join <- right_join(dt_meanx, dt_meany, by = "dataset")
dt_join <- right_join(dt_join, dt_meany, by = "dataset")
dt_join <- right_join(dt_join, dt_sdx, by = "dataset")
dt_join <- right_join(dt_join, dt_sdy, by = "dataset")
dt_join <- right_join(dt_join, dt_corr, by = "dataset")
glimpse(dt_join)
filter(datasaurus_dozen,dataset=="dino") %>% 
ggplot( aes(x=x, y=y))+
  geom_point(colour="blue", size=2)+
  theme_void()+
  theme(legend.position = "none")
