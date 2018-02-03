library(tufte)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)
mean=100; sd=15
type=rep(1,100)
Mean <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(Mean,mean,sd)
d <- data.frame(Mean,hx,type)
sd2 <- 20
mean2 <- 100
type <- rep(2,100)
Mean <- seq(-4,4,length=100)*sd2 + mean2
hx <- dnorm(Mean,mean2,sd2)
d2 <- data.frame(Mean,hx,type)
mean3 <- 130
sd3 <- 30
type <- rep(3,100)
Mean <- seq(-4,4,length=100)*sd3 + mean3
hx <- dnorm(Mean,mean3,sd3)
d3 <- data.frame(Mean,hx,type)
d <- bind_rows(d,d2,d3)

d <- bind_rows(d,d2)


d$type <- as.factor(d$type)
ggplot(d) + geom_line(aes(x=Mean, y = hx,colour=type, 
  group=type), size=1.5)+
  theme_tufte() + ylab("") +  xlab(" ") +
theme( axis.text.x=element_blank(),
axis.ticks.x=element_blank(),
  axis.text.y=element_blank(),
   axis.ticks.y=element_blank())
ggplot() +
  geom_rect(aes(xmin=50, ymin=0, xmax=150, ymax=450), 
  colour="red",fill="white") + xlim(0,200) + ylim(0,500) + 
  geom_rangeframe() + theme_tufte() +
  
  theme(axis.title.x = element_text(vjust=-0.5),
        axis.title.y = element_text(vjust=1.5))

bayes <- function(prior, hypo_true, hypo_false, rnd=3) {
  post <- (prior * hypo_true)/ 
    ((prior * hypo_true) + hypo_false * (1-prior))
  #  print(round(post, rnd))
}
## prior is the base rate
## cond_probability is how likely is an event given that something related has occured. The opposite of the posteriori
## likely is 
## End of Bayes function ##############
require(dplyr)
require(ggplot2)
require(ggthemes)
require(tidyr)
par(mfrow=c(1,3), las=1)
set.seed(43)
x <- c(1,1,1,1,1,1,-1,-1,-1,-1)
cum_01 <- as.numeric(data.frame())
cum_50 <- as.numeric(data.frame())
cum_99 <- as.numeric(data.frame())
prior_01 <- .01
prior_50 <- .5
prior_99 <- .99
cum_01[1] <- prior_01
cum_50[1] <- prior_50
cum_99[1] <- prior_99
for(i in 1:26) {
  s <- sample(x, 1, replace = TRUE)
  if(s > 0) {
    hypo_true <- .75
    hypo_false <- .25
    
  } else {
    hypo_true <- .25
    hypo_false <- .75
    
  }
  z_01 <- bayes(cum_01[i], hypo_true,hypo_false, rnd = 5)
  if(z_01 > .999){
    z_01 <- .999
  }
  cum_01[i+1] <-  z_01
  z_50 <- bayes(cum_50[i], hypo_true,hypo_false, rnd = 5)
  if(z_50 > .999){
    z_50 <- .999
  }
  cum_50[i+1] <-  z_50
  z_99 <- bayes(cum_99[i], hypo_true,hypo_false, rnd = 5)
  if(z_99 > .999){
    z_99 <- .999
  }
  cum_99[i+1] <-  z_99
  
  
}
w <- c(0:26)
# prio_01 <-c(rep(1,27))
w <- as.data.frame(w)
cum_01 <- as.data.frame(cum_01)
prio_01 <- as.data.frame(c(rep(1,27)))
# prio_50 <-c(rep(2,27))
cum_50 <- as.data.frame(cum_50)
prio_50 <- as.data.frame(c(rep(2,27)))
# prio_99 <-c(rep(3,27))
cum_99 <- as.data.frame(cum_99)
prio_99 <- as.data.frame(c(rep(3,27)))
final_cumcum  <- bind_cols(w,prio_01,cum_01, prio_50, cum_50, prio_99, 
                cum_99)
x1 <- rep("01", 27)
x50 <- rep("50", 27)
x99 <- rep("99", 27)
x1 <- as.data.frame(x1)
x50 <- as.data.frame(x50)
x99 <- as.data.frame(x99)
cum01 <- bind_cols(w, x1, cum_01)
cum50 <- bind_cols(w, x50, cum_50)
cum99 <- bind_cols(w, x99, cum_99)


colnames(cum01) <- c("Week", "Prior", "pct")
colnames(cum50) <- c("Week", "Prior", "pct")
colnames(cum99) <- c("Week", "Prior", "pct")
finalcum <- bind_rows(cum01, cum50, cum99)
finalcum <- mutate(finalcum, Pct = round(pct * 100, 0))
p1 <- ggplot(data = finalcum) + aes(x=Week, y = Pct, group = Prior, 
                colour = Prior) + geom_line(aes(linetype=Prior)) + geom_point() +
  xlab("Week") + ylab("Pct Belief\n in Bull Mkt.") +
  ggtitle("Bayesian Revision\n of Belief of Bull Market\n (Bull Market)") + theme_tufte()



####################################################
## Bear Market
#################################################3#3
set.seed(43)
x <- c(-1,-1,-1,-1,-1,-1,1,1,1,1)
cum_01 <- as.numeric(data.frame())
cum_50 <- as.numeric(data.frame())
cum_99 <- as.numeric(data.frame())
prior_01 <- .01
prior_50 <- .5
prior_99 <- .99
cum_01[1] <- prior_01
cum_50[1] <- prior_50
cum_99[1] <- prior_99
for(i in 1:26) {
  s <- sample(x, 1, replace = TRUE)
  if(s > 0) {
    hypo_true <- .75
    hypo_false <- .25
    
  } else {
    hypo_true <- .25
    hypo_false <- .75
    
  }
  z_01 <- bayes(cum_01[i], hypo_true,hypo_false, rnd = 5)
  if(z_01 < .001){
    z_01 <- .001
  }
  cum_01[i+1] <-  z_01
  z_50 <- bayes(cum_50[i], hypo_true,hypo_false, rnd = 5)
  if(z_50 < .001){
    z_50 <- .001
  }
  cum_50[i+1] <-  z_50
  z_99 <- bayes(cum_99[i], hypo_true,hypo_false, rnd = 5)
  if(z_99 < .001){
    z_99 <- .001
  }
  cum_99[i+1] <-  z_99
  
  
}
w <- c(0:26)
prio_01 <-c(rep(1,27))
w <- as.data.frame(w)
cum_01 <- as.data.frame(cum_01)
prio_01 <- as.data.frame(prio_01)
prio_50 <-c(rep(2,27))
cum_50 <- as.data.frame(cum_50)
prio_50 <- as.data.frame(prio_50)
prio_99 <-c(rep(3,27))
cum_99 <- as.data.frame(cum_99)
prio_99 <- as.data.frame(prio_99)
final_cumcum  <- bind_cols(w,prio_01,cum_01, prio_50, cum_50, prio_99, cum_99)
x1 <- rep("01", 27)
x50 <- rep("50", 27)
x99 <- rep("99", 27)
x1 <- as.data.frame(x1)
x50 <- as.data.frame(x50)
x99 <- as.data.frame(x99)
cum01 <- bind_cols(w, x1, cum_01)
cum50 <- bind_cols(w, x50, cum_50)
cum99 <- bind_cols(w, x99, cum_99)


colnames(cum01) <- c("Week", "Prior", "pct")
colnames(cum50) <- c("Week", "Prior", "pct")
colnames(cum99) <- c("Week", "Prior", "pct")
finalcum <- bind_rows(cum01, cum50, cum99)
finalcum <- mutate(finalcum, Pct = round(pct * 100, 0))
p2 <- ggplot(data = finalcum) + aes(x=Week, y = Pct, group = Prior, colour = Prior) + geom_line(aes(linetype=Prior)) + geom_point() +
  xlab("Week") + ylab("Pct Belief\n in Bull Mkt.") +
  ggtitle("Bayesian Revision\n of Belief of Bull Market\n (Bear Market)") + theme_tufte()


## Same Bull market set to .75 up and .25 Down
set.seed(43)
x <- c(rep(1, 75), rep(-1, 25))
cum_01 <- as.numeric(data.frame())
cum_50 <- as.numeric(data.frame())
cum_99 <- as.numeric(data.frame())
prior_01 <- .01
prior_50 <- .5
prior_99 <- .99
cum_01[1] <- prior_01
cum_50[1] <- prior_50
cum_99[1] <- prior_99
for(i in 1:26) {
  s <- sample(x, 1, replace = TRUE)
  if(s > 0) {
    hypo_true <- .7
    hypo_false <- .2
    
  } else {
    hypo_true <- .2
    hypo_false <- .7
    
  }
  z_01 <- bayes(cum_01[i], hypo_true,hypo_false, rnd = 5)
  if(z_01 > .999){
    z_01 <- .999
  }
  cum_01[i+1] <-  z_01
  z_50 <- bayes(cum_50[i], hypo_true,hypo_false, rnd = 5)
  if(z_50 > .999){
    z_50 <- .999
  }
  cum_50[i+1] <-  z_50
  z_99 <- bayes(cum_99[i], hypo_true,hypo_false, rnd = 5)
  if(z_99 > .999){
    z_99 <- .999
  }
  cum_99[i+1] <-  z_99
  
  
}
w <- c(0:26)
prio_01 <-c(rep(1,27))
w <- as.data.frame(w)
cum_01 <- as.data.frame(cum_01)
prio_01 <- as.data.frame(prio_01)
prio_50 <-c(rep(2,27))
cum_50 <- as.data.frame(cum_50)
prio_50 <- as.data.frame(prio_50)
prio_99 <-c(rep(3,27))
cum_99 <- as.data.frame(cum_99)
prio_99 <- as.data.frame(prio_99)
final_cumcum  <- bind_cols(w,prio_01,cum_01, prio_50, cum_50, prio_99, cum_99)
x1 <- rep("01", 27)
x50 <- rep("50", 27)
x99 <- rep("99", 27)
x1 <- as.data.frame(x1)
x50 <- as.data.frame(x50)
x99 <- as.data.frame(x99)
cum01 <- bind_cols(w, x1, cum_01)
cum50 <- bind_cols(w, x50, cum_50)
cum99 <- bind_cols(w, x99, cum_99)


colnames(cum01) <- c("Week", "Prior", "pct")
colnames(cum50) <- c("Week", "Prior", "pct")
colnames(cum99) <- c("Week", "Prior", "pct")
finalcum <- bind_rows(cum01, cum50, cum99)
finalcum <- mutate(finalcum, Pct = round(pct * 100, 0))
p3 <- ggplot(data = finalcum) + aes(x=Week, y = Pct, group = Prior, colour = Prior) + geom_line(aes(linetype=Prior)) + geom_point() +
  xlab("Week") + ylab("Pct Belief\n in Bull Mkt.") +
  ggtitle("Bayesian Revision\n of Belief of Bull Market\n (Bull Market)") + theme_tufte()
par(mfrow=c(1,3), las=1)
p1
p2
p3

