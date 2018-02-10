require(tibble)
require(tidyr)
require(dplyr)
dt <- tibble(before=c(107,110,143,168,145,125), after=c(123,122,145,156,160,134))
dtd <- dt %>% 
  mutate(diff = after - before)
sd(dtd$diff)
mean(dtd$before)
mean(dtd$after)
mean(dtd$diff)
  