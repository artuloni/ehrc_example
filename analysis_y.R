##### 
set.seed(123)
n <- 1000
x <- rnorm(n, mean = 10, sd = 2)
noise <- scale(x) + rnorm(n, mean = 0, sd = 0.3)
intercept <- 1
slope <- 0.8
lambda <- exp(intercept * slope * noise)
y <- rpois(n, lambda)
data <- data.frame(x = x, y = y)
#####

mod.lm <- lm(y ~ scale(x), data = data)
summary(mod.lm)
