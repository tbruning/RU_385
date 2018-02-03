library(tufte)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggthemes)
require(knitr)
require(tibble)
library(RColorBrewer)
a <- tibble(a=c(490,580,440,580,430))
b <- tibble(c(310,590,730,710,540))
c <- tibble(c(500,450,510,570,610))
d <- tibble(c(450,590,710,240,510))
e <- tibble(c(420,640,470,530,640))
f <- tibble(c(450,670,390,500,470))
g <- tibble(c(490,450,590,640,650))
h <- tibble(c(670,610,550,540,540))

i <- bind_cols(a,b,c,d,e,f,g,h)
kable(i, col.names = c(1,2,3,4,5,6,7,8),caption = "Random sample from GMAT Scores Population")

a <- tibble(x="1",y=c(490,580,440,580,430),z="x")
b <- tibble(x="2",y=c(310,590,730,710,540),z="x")
c <- tibble(x="3",y=c(500,450,510,570,610),z="x")
d <- tibble(x="4",y=c(450,590,710,240,510),z="x")
e <- tibble(x="5",y=c(420,640,470,530,640),z="x")
f <- tibble(x="6",y=c(450,670,390,500,470),z="x")
g <- tibble(x="7",y=c(490,450,590,640,650),z="x")
h <- tibble(x="8",y=c(670,610,550,540,540),z="x")

i <- bind_rows(a,b,c,d,e,f,g,h)

k <- group_by(i,x)

l <- summarize(k,y=mean(y))
# l <- tibble(l)
z1 <- tibble((rep("Sample Mean",8)))
colnames(z1) <- "z"
l <- bind_cols(l,z1)
i <- bind_rows(i, l)
i$z <- as.factor(i$z)


ggplot(i, aes(x=x, y=y, group=z))+ 
  geom_point(aes(colour=z, fill=z,shape=z), size=3) +  
  scale_colour_manual(values = c("red","blue")) +
  geom_hline(yintercept=mean(i$y),linetype="dashed") + 
  scale_shape_manual(values=c(10,1)) + theme_tufte() +
  xlab("Samples") + ylab("Scores") + 
  theme(legend.title=element_blank()) 
g <- tibble(x=c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3))
h <- tibble(y=c(0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3))
i <- bind_cols(g,h)
colnames(i) <- c("g","h")
i <- i %>% mutate(m=(g+h)/2)

ggplot(i, aes(x=g)) + geom_histogram() + theme_tufte() + ylab(" ") + xlab(" ") + ggtitle("Population") 

ggplot(i, aes(x=m)) + geom_histogram()+ theme_tufte() + ylab(" ") + xlab(" ") + ggtitle("Sample Means (n=2)") 
