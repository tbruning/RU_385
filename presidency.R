library(tufte)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggthemes)
require(knitr)
require(tibble)
a <- tibble(c(27,41,61,44,49,42,57,48,65,67,37,45,41))
b <- tibble(c(-45,-29,-4,-47,-15,-26,-8,-52,5,8,-30,-63,-13))
c <- bind_cols(a,b)
ggplot(c, aes(x=a, y=b)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  theme_tufte() +
  xlab("Pres. Approval Rating") + ylab("Midterm House Change") 
m <- lm(b ~ a, data = c) 
rpi <- cbind(c, predict(m, interval = "prediction")) 
summary(m)
m <- lm(wt ~ qsec, data = mtcars) 
# cbind the predictions to mtcars 
mpi <- cbind(mtcars, predict(m, interval = "prediction")) 

library(ggplot2) 
ggplot(rpi, aes(x = a)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), 
              fill = "blue", alpha = 0.1) + 
  geom_point(aes(y = b)) + 
  geom_line(aes(y = fit), colour = "blue", size = 1) +  # Add linear regression line 
  theme_tufte() +
  xlab("Pres. Approval Rating") + ylab("Midterm House Change") 
