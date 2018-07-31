library(tufte)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggthemes)
require(knitr)
require(tibble)
sfny_df <- read.csv(file= "./data/sf_ny_data.csv")
sfny_df$in_sf <- as.factor(sfny_df$in_sf)

df <- mutate(sfny_df, city= ifelse(in_sf == "0", "ny", "sf"))
##sfny_df <- setNames(sfny_df[-1,], sapply(sfny_df[1,], 
                             ##            as.character))
df$city <- as.factor(df$city)
ggplot(data=df, aes(x=city, y=elevation, fill=city)) +
  geom_dotplot(binaxis='y', stackdir='center',
               dotsize=.5) +
  geom_hline(yintercept = 73) +
  theme_tufte()

ggplot(data=df, aes(x=city, y=price_per_sqft, fill=city)) +
  geom_dotplot(binaxis='y', stackdir='center',
               dotsize=.5) +
  geom_hline(yintercept = 2200) +
  theme_tufte()

ggplot(data = df, aes(x=price_per_sqft, y = elevation, 
                      colour=city)) +
  annotate("rect",xmin = 100, xmax = 4900, ymin = 73, ymax = 250, 
           fill="cadetblue1",alpha=.4) +
  annotate("rect",xmin = 2270, xmax = 4900, ymin = 0, ymax = 73, 
           fill="sienna3",alpha=.15) +
  geom_point() +
  theme_tufte() 
``
  
