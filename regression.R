
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

df <- tibble(a=c(0,1,2,3,4,5)X, b=c(000,100,250,90,50,300))
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
new <- data.frame(x = seq(1000, 2500, 100))
predict(lm(y ~ x), new, se.fit = TRUE)
<<<<<<< HEAD
pred.w.plim <- as.tibble(predict(lm(y ~ x), new, interval = "prediction"))
=======
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
>>>>>>> a4c867f09da036bceabae19931ffd414c2a52ad5
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
  
d <- cbind(pred.w.clim, pred.w.plim[,-1])
<<<<<<< HEAD
matplot(new$x, pred.w.plim[,-1],
        lty = c(3,2,2,3,3), type = "l", ylab = "predicted y")           
ggplot() + geom_line(aes(x=new$x, y= pred.w.plim$lwr), col="red") +
  geom_line(aes(x=new$x, y= pred.w.plim$upr), col="red") + 
  theme_tufte() + ylim(0,500) + xlim(0,3000)
=======
           
           
>>>>>>> a4c867f09da036bceabae19931ffd414c2a52ad5
