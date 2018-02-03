require(ggplot2)
require(tibble)
require(tufte)
require(ggthemes)
df <- data.frame(
  group = c("Group1","Group2", "Group3"),
  value = c(25, 36, 34 )
)
head(df)

tst <-  c(30, 36, 34 )
tst
sum(tst)
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("n", start=0)
pie
pie + scale_fill_brewer("Blues") + theme_tufte() +
