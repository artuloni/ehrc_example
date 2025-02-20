##### 
set.seed(123)
n <- 1000
b0 <- 1
b1 <- 2
x <- rnorm(1000, 1, 0.25)
mu <- exp(b0 + b1 * x)
y <- rpois(1000, mu)
data <- data.frame(x = x, y = y)
#####

# Detailed analysis ####
library(fitdistrplus)
library(performance)
library(parameters)
library(rethinking)
library(ggplot2)
library(boot)

precis(x)
precis(y)

hist(x)
hist(y)

boxplot(x)
boxplot(y)

class(x)
class(y)

plot(y ~ x)

mod.lm <- lm(y ~ x, data = data)
descdist(resid(mod.lm), discrete = FALSE, boot = 1000)
plot(fitdist(resid(mod.lm), distr="norm"))
check_model(mod.lm)
summary(mod.lm) # Not good results are not reliable


mod.glm <- glm(y ~ x, family = poisson(link = "log"), data = data)
sum(residuals(mod.glm, type = "deviance")^2) / df.residual(mod.glm) 
descdist(resid(mod.glm, type = "deviance"), discrete = T, boot = 1000)
check_model(mod.glm) 
summary(mod.glm) # Results more reliable, suggest using Quasi-poisson


ggplot(data = data, aes(x = x, y = y)) + geom_point(size = 3) + 
  stat_smooth(method = lm)

ggplot(data = data, aes(x = x, y = y)) + geom_point(size = 3) + 
  stat_smooth(method = glm,method.args = list(family = poisson(link = "log"))) 

mod.lm2 <- glm(y ~ x, data = data)
cv.glm(data, mod.lm2, K = n)$delta[1]
cv.glm(data, mod.glm, K = n)$delta[1]

type <- c()
for(i in 1 : length(y)){
  ifelse(y[i] < mean(y), type[i] <- "small", type[i]<- "big")
}
table(type)

