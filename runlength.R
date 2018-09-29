x <- c(rep(1,93),rep(0,69))
y <- sample(x,162,replace = TRUE)
y
z <- rle(y)
z
max(z$lengths)
