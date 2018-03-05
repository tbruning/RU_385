
library(tufte)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggthemes)
require(knitr)
require(tibble)

a <- tibble(c(245,312,279,308,199,219,405,324,319,255))
b <- tibble(c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700))
c <- as.data.frame(bind_cols(a,b))
colnames(c) <- c("House Price", "Sq. Ft.")
ggplot(c, (aes(x=b, y=a))) + geom_point() + 
  xlab("Square Feet") + ylab("House Price ($1,000") + 
  theme_tufte() + geom_rug() +
  stat_smooth(method = "lm", col = "red", se=FALSE) +xlim(0,3000) +ylim(0,450)

df <- tibble(a=c(0,1,2,3,4,5), b=c(000,100,250,90,50,300))
dfmean <- mean(df$b) 
dfxmean <- mean(df$a)
temp<- expression(paste( hat(Y[i]), " = ", beta[0], " + ", 
                         beta[1], X[1], " + ",epsilon[i]))
lm1 <- lm(df$b ~ df$a)
yend <- 2*34 + 46.67
ggplot(df, (aes(x=a, y=b))) + geom_point(size=3) + 
  xlab(" ") + ylab(" ") + ylim(0,325) + 
  theme_tufte() + geom_rug() +
  geom_hline(yintercept = dfmean, col="blue") +
  geom_vline(xintercept = dfxmean, col="blue") +
  stat_smooth(method = "lm", col = "red", se=FALSE) +
  geom_segment(aes(x=2,xend=2, y=250, yend=yend))+
  geom_segment(aes(x=1.4,xend=1.9, y=250, yend=250), arrow=arrow())+  
  annotate("text", x = 1, y = 250, label = "Observed value \nof Y for x=2") +
  annotate("text", x = 1, y = 180,
    label = c("Predicted value \nof Y for x=2")) +
  annotate("text", x = 3.5, y = 240,
         label = "Random~error~ (~epsilon)",parse = TRUE)+
  geom_segment(aes(x=1.5,xend=1.99, y=165, yend=yend +1), arrow=arrow()) +
geom_segment(aes(x=3,xend=2, y=225, yend=200), arrow=arrow())+
  annotate("text", x = 2.5, y = 320,
           label = as.character(temp), parse=TRUE, size=7)

require(graphics)

## 
colnames(c) <- c("a", "b")
x <- c$b
y <- c$a
predict(lm(y ~ x))
new <- data.frame(x = seq(100, 3000, 100))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- as.tibble(predict(lm(y ~ x), new, interval = "prediction"))
pred.w.clim <- as.tibble(predict(lm(y ~ x), new, interval = "confidence"))
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
  
d <- cbind(pred.w.clim, pred.w.plim[,-1])
matplot(new$x, pred.w.plim[,-1],
        lty = c(3,2,2,3,3), type = "l", ylab = "predicted y")           
ggplot() + geom_line(aes(x=new$x, y= pred.w.plim$lwr), col="red") +

  geom_line(aes(x=new$x, y= pred.w.plim$upr), col="red") + 
  geom_line(aes(x=new$x, y= pred.w.clim$lwr), col="blue") +
  geom_line(aes(x=new$x, y= pred.w.clim$upr), col="blue") +
  geom_line(aes(x=new$x, y= pred.w.clim$fit), col="black") +
  theme_tufte() + ylim(0,700) + xlim(0,3000) +
  ylab(" ") + xlab(" ") +
  annotate("text", x=700, y=575, 
           label = "Prediction inteval (PI)", color="red")+
  annotate("text", x=700, y=540, label= "vs.") +
  annotate("text", x=700, y=500, 
           label = "Confidence inteval (CI)", color="blue") +
  geom_point(data=c, (aes(x=b, y=a))) 



df <- tibble(a=c(0,1,2,3,4,5), b=c(000,100,250,90,50,300))
dfmean <- mean(df$b) 
dfxmean <- mean(df$a)
temp<- expression(paste( hat(Y[i]), " = ", beta[0], " + ", 
                         beta[1], X[1], " + ",epsilon[i]))
lm1 <- lm(df$b ~ df$a)
yend <- 2*34 + 46.67
ggplot(df, (aes(x=a, y=b))) + geom_point(size=3) + 
  xlab(" ") + ylab(" ") + ylim(0,325) + 
  theme_tufte() + geom_rug() +
  stat_smooth(method = "lm", col = "red", se=FALSE) +
  geom_segment(aes(x=2,xend=2, y=250, yend=yend))+
  geom_segment(aes(x=1.4,xend=1.9, y=250, yend=250), arrow=arrow())+  
  annotate("text", x = 1, y = 250, label = "Observed value \nof Y for x=2") +
  annotate("text", x = 1, y = 180,
           label = c("Predicted value \nof Y for x=2")) +
  annotate("text", x = 3.5, y = 240,
           label = "Random~error~ (~epsilon)",parse = TRUE)+
  geom_segment(aes(x=1.5,xend=1.99, y=165, yend=yend +1), arrow=arrow()) +
  geom_segment(aes(x=3,xend=2, y=225, yend=200), arrow=arrow())+
  annotate("text", x = 2.5, y = 320,
           label = as.character(temp), parse=TRUE, size=7)

mydata=with(anscombe,data.frame(xVal=c(x1,x2,x3,x4), yVal=c(y1,y2,y3,y4), mygroup=gl(4,nrow(anscombe))))
# aggregate(.~mygroup,data=mydata,mean)
# aggregate(.~mygroup,data=mydata,sd)
# aggregate(.~mygroup,data=mydata,var)
# aggregate(.~mygroup,data=mydata,cor(xVal,yVal))
ds1 <- select(anscombe,contains("1"))
ds2 <- select(anscombe,contains("2"))
ds3 <- select(anscombe,contains("3"))
ds4 <- select(anscombe,contains("4"))
ds <- bind_cols(ds1,ds2,ds3,ds4)
colnames(ds) <- c("DS1_x", "DS1_y","DS2_x", "DS2_y","DS3_x", "DS3_y","DS4_x", "DS4_y")
mygrp <- group_by(mydata, mygroup)
ds_x_means <- summarize(mygrp,mean(xVal) )
ds_y_means <- summarize(mygrp,mean(yVal) )
ds_x_var <- summarize(mygrp,var(xVal) )
ds_y_var <- summarize(mygrp,var(yVal) )

ds_cor <- summarize(mygrp, cor(xVal, yVal))
colnames(ds_cor) <- c("mygroup", "cor")
ds_cor <- as.data.frame(ds_cor)
ds_means <- right_join(ds_x_means,ds_y_means, by = "mygroup")
ds_var <- right_join(ds_x_var,ds_y_var, by = "mygroup")
ds_sum1 <- right_join( ds_means, ds_var, by = "mygroup")
ds_sum1 <- right_join(ds_sum1, ds_cor, by ='mygroup')
colnames(ds_sum1) <- c("Dataset", "Mean_x", "Mean_y", "Var_x", "Var_y", "Cor")
ds_sum1$Mean_y <- round(ds_sum1$Mean_y,2)
ds_sum1$Var_y <- round(ds_sum1$Var_y,2)
ds_sum1$Cor <- round(ds_sum1$Cor,2)
ggplot(mydata,aes(x=xVal, y=yVal)) + geom_point() + xlim(0,20) + ylim(0,14) + facet_wrap(~mygroup) +
  stat_smooth(method = "lm", col = "red", se=FALSE) + theme_tufte()

a <- tibble(c(245,312,279,308,199,219,405,324,319,255))
b <- tibble(c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700))
c <- as.data.frame(bind_cols(a,b))
colnames(c) <- c("House Price", "Sq. Ft.")
plot1 <- ggplot(c, (aes(x=b, y=a))) + geom_point() + xlab("Square Feet") + ylab("House Price ($1,000") + theme_tufte() +xlim(0,3000) +ylim(0,450) + geom_rug()
c <- as.data.frame(c)
colnames(c) <- c("a", "b")
total<-anova(lm(c$a ~ c$b, data=c))
coeff <- lm(c$a ~ c$b, data=c)
##grid.arrange(
##  plot1,
##  tableGrob(round(coeff$coefficients,4)),
##    tableGrob(round(total,3)),a
##  nrow = 3)

ggplot(c, (aes(x=b, y=a))) + geom_point() + xlab("Square Feet") + 
  geom_rug() + ylab("House Price ($1,000")+ theme_tufte() +
  stat_smooth(method = "lm", col = "red", se=FALSE) + 
  xlim(0,3000) +ylim(0,450) +
  annotate("text", x = 1000, y = 400,
           label = "House Price Data: \n Scatterplot and Regression Line", family="seriff")

a <- tibble(a= c(245,312,279,308,199,219,405,324,319,255))
b <- tibble(b= c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700))
c <- bind_cols(a,b)
d <- c %>% 
  mutate(d=98.24833+.10977*b) %>% 
  mutate(e=a-d)

ggplot(d, aes(x=b,y=e)) + geom_point() + ylim(-60,80)+ geom_hline(yintercept = 0) + xlab("Sq. Ft.") + 
  ylab("Residual") + geom_rug() + theme_tufte() +
  annotate("text", x = 2200, y = 80,
           label = "Residual Analysis", family="seriff")

e <- c %>% 
  mutate(upper=232.07+.1858*b) %>% 
  mutate(lower=-35.58+.03374*b) %>% 
  mutate(d=98.24833+.10977*b)

ggplot(e) + geom_line(aes(x=b,y=d)) +
  geom_point(aes(x=b, y=a)) +
  geom_line(aes(x=b,y=upper), color="blue") +
  geom_line(aes(x=b,y=lower), color="darkgreen") +
  theme_tufte() + geom_rug() +
  xlim(0,2500) + ylim(-50,700) +
  xlab("Sq. Ft.") + ylab("$ in thousands") +
  annotate("text", x=910, y=min(e$d), label="Point Est.", family="serrif") +
  annotate("text", x=910, y=min(e$upper), label="Upper Est.", color="Blue", family="serrif") +
  annotate("text", x=910, y=min(e$lower), label="Lower Est.", color="darkgreen", family="serrif") +
  annotate("text", x=500, y=650, label= "Range of Estimated \n Housing Prices \n 95% Confidence Inteval", family="serrif")
  
  
