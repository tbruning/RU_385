require(tibble)
require(ggthemes)
require(ggplot2)
require(tidyr)
require(dplyr)
require(grid)



xstart <- NULL
df <- tibble(z = seq(-4, 4, length = 100))
df1 <- tibble(y=dnorm(df$z))
dffinal <- bind_cols(df,df1)
xstart[2] <- dffinal[75,1]
xstart[1] <- dffinal[26,1]
x1 <- xstart[1]
x2 <- xstart[2]
label <-  expression(paste("Plot of ", alpha^beta, " versus ", hat(mu)[0]))
base <- ggplot(dffinal, aes(x=z, y=y)) + geom_area(fill="white",color="blue") +
  theme_tufte() +
  geom_area(data = subset(dffinal, z < qnorm(0.03)), fill="red") + 
  geom_area(data = subset(dffinal, z > qnorm(0.97)), fill = "red") + theme_tufte() +
  annotate("text", x = c(.5), y = c(.08), adj=1,  family="serif",
           label = "Accept") +
  annotate("text", x = c(-2.5), y = c(.08), adj=1,  family="serif", color = "red",     label = "Reject") + 
annotate("text", x = c(3), y = c(.08), adj=1,  family="serif", color = "red",     label = "Reject") +
geom_segment(aes(x = -1.9,y = .06,xend = 1.9,yend = .06),arrow=arrow(ends = "both", type = "closed", angle = 20)) +

geom_segment(aes(x = -1.9,y = .06,xend = -4,yend = .06),color="red", arrow=arrow(ends = "last", type = "closed", angle = 20)) +
geom_segment(aes(x = 1.9,y = .06,xend = 4,yend = .06),color="red", arrow=arrow(ends = "last", type = "closed", angle = 20))

base


base1 <- ggplot(dffinal, aes(x=z, y=y)) + geom_area(fill="white",color="blue") +
  theme_tufte() +
  geom_area(data = subset(dffinal, z < qnorm(0.06)), fill="red") + 
theme_tufte() +
  annotate("text", x = c(.5), y = c(.08), adj=1,  family="serif",
           label = "Accept") +
  annotate("text", x = c(-2.5), y = c(.08), adj=1,  family="serif", color = "red",     label = "Reject") + 

  geom_segment(aes(x = -1.6,y = .12,xend = 4,yend = .12),arrow=arrow(ends = "both", type = "closed", angle = 20)) +
  
  geom_segment(aes(x = -1.6,y = .12,xend = -4,yend = .12), color="red",arrow=arrow(ends = "last", type = "closed", angle = 20)) 

base1

base2 <- ggplot(dffinal, aes(x=z, y=y)) + geom_area(fill="white",color="blue") +
  theme_tufte() +
  geom_area(data = subset(dffinal, z < qnorm(0.06)), fill="orange") + 
  geom_area(data = subset(dffinal, z < qnorm(0.03)), fill="red", alpha=.3) + 
  theme_tufte() +
  annotate("text", x = c(.5), y = c(.08), adj=1,  family="serif",
           label = "Accept") +
  annotate("text", x = c(-2.5), y = c(.08), adj=1,  family="serif", color = "red",     label = "Reject") 
base2

