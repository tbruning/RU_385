library(segmented)
set.seed(12)
xx <- 1:100
zz <- runif(100)
yy <- 2 + 1.5*pmax(xx - 35, 0) - 1.5*pmax(xx - 70, 0) + 15*pmax(zz - .5, 0) + 
  rnorm(100,0,2)
dati <- data.frame(x = xx, y = yy, z = zz)
out.lm <- lm(y ~ x, data = dati)
o <- segmented(out.lm, seg.Z = ~x, psi = list(x = c(30,60)),
               control = seg.control(display = FALSE)
)
dat2 = data.frame(x = xx, y = broken.line(o)$fit)

library(ggplot2)
ggplot(dati, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat2, color = 'blue')