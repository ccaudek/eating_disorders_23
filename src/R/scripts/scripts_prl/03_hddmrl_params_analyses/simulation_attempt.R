
library("lme4")



n <- 10
y1g1 <- rnorm(n)
y2g1 <- rnorm(n)
y3g1 <- rnorm(n)
y1g2 <- rnorm(n)
y2g2 <- rnorm(n)
y3g2 <- rnorm(n)

df <- data.frame(
  y = c(y1g1, y2g1, y3g1, y1g2, y2g2, y3g2),
  gr = rep(0:1, each = 60),
  time = rep(c(0, 1, 2, 0, 1, 2), each = n),
  id = factor(c(rep(1:n, 3), rep(1:n, 3)+100))
)

m <- lme4::lmer(y ~ gr + time + (1 | id), data = df)
summary(m)




