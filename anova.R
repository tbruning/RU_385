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
ggplot(e) + geom_point(aes(x=Club, y = n), color=e$Club) + theme_tufte() +
  geom_hline(yintercept = mean(e$n))

  