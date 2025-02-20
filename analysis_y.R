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

type <- c()
for(i in 1 : length(y)){
  ifelse(y[i] < mean(y), type[i] <- "small", type[i]<- "big")
}
table(type)
