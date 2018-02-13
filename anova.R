require(tibble)
require(tidyr)
require(dplyr)
require(ggplot2)
require(ggthemes)
a <- tibble(c(254,263,241,237,251))
b <- tibble(c(234,218,235,227,216))
c <- tibble(c(200,222,197,206,204))
d <- bind_cols(a,b,c)


colnames(d) <- c(1,2,3)
e <- gather(d, "Club","n", 1:3)
gd <- e %>% 
  group_by(Club) %>% 
  summarise(n = mean(n))
ggplot(e) + geom_point(aes(x=Club, y = n), color=e$Club, size = 3) + 
  geom_point(data=gd,aes( x=gd$Club, y = gd$n), color="blue", size = 5,
             shape=17) +
  theme_tufte() +
  geom_hline(yintercept = mean(e$n)) + ylab("Distance") +
  annotate("text", x = 2.5, y = 260, size=5, 
           label = "ANOVA \n Comparisons three golf clubs") +
  annotate("text", x = .5, y = 229,  
           label = "bar(X)", parse=T) 


  