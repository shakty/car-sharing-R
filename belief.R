# CAR-SHARING
source("/home/stefano/kaycar/R/init.R")

## General Stats"

nrow(data)
summary(data)

table(data$car.level, data$payoff.bus)

#       50   70
#  25 5540 5539
#  50 5508 5465
#  75 5558 5986


normal.lik1 <- function(theta, y) {
  mu <- theta[1]
  sigma2 <- theta[2]
  n <- length(y)
  logl <- -.5 * n * log(2*pi) -.5 * n * log(sigma2) -
    (1 / (2*sigma2)) * sum((y-mu)^2)
  return(-logl)
}

normal.lik2 <- function(theta, y) {
  mu <- theta[1]
  sigma <- theta[2]
  n <- length(y)
  z <- (y - mu) / sigma
  logl <- -n*log(sigma) - sum(log(dnorm(z)))
  return(-logl)
}

y <- rnorm(2000, 0, 1)
optim(c(0,1), normal.lik2, y=y, method="BFGS")


optim(c(0,1), normal.lik1, y=y, method = "L-BFGS-B",
      lower = c(-Inf, 0), upper = c(Inf, Inf))

##
library(stats4)

set.seed(1001)
N <- 100

x <- runif(N)
y <- 5 * x + 3 + rnorm(N)


fit <- lm(y ~ x)
summary(fit)


# Pushing on to the MLE for the linear model parameters.

# First we need a likelihood function. The model is not a PDF, so we can't
# proceed in precisely the same way that we did with the normal distribution.
# However, if you fit a linear model then you want the residuals to be normally
# distributed. So the likelihood function fits a normal distribution
# to the residuals.

LL <- function(beta0, beta1, mu, sigma) {
    # Find residuals
    #
    R = y - x * beta1 - beta0
    #
    # Calculate the likelihood for residuals (with mu and sigma as parameters)
    #
    R = suppressWarnings(dnorm(R, mu, sigma))
    #
    # Sum the log likelihoods for all of the data points
    #
    -sum(log(R))
}

LL <- function(beta0, beta1, mu, sigma) {
    R = y - x * beta1 - beta0
    #
    R = suppressWarnings(dnorm(R, mu, sigma, log = TRUE))
    #
    -sum(R)
}

fit <- mle(LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma=1))

fit <- mle(LL, start = list(beta0 = 5, beta1 = 2, mu = 0, sigma=1))
summary(fit)

fit <- mle(LL, start = list(beta0 = 2, beta1 = 1.5, sigma=1),
           fixed = list(mu = 0), nobs = length(y))

summary(fit)


library(bbmle)

fit <- mle2(minuslogl = LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma = 1))
summary(fit)


## Example how likelihood vary.

x.seq <- seq(from=-1, to=3, length.out=150)
y.seq <- seq(from=-5, to=5, length.out=150)

cond.pdf <- function(x,y) { dnorm(y, mean=10-5*x, sd=0.5) }

z <- outer(x.seq, y.seq, cond.pdf)

persp(x.seq,y.seq,z, ticktype="detailed", phi=75, xlab="x",
ylab="y", zlab=expression(p(y|x)), cex.axis=0.8)


## DOING STUFF

mydata <- data[data$car2car == 1,]

mydata$departure.time.diff.share <- ifelse(mydata$departure.time.diff > 0,
                                           (mydata$departure.time.diff / (60 - mydata$departure.time.lag)),
                                           abs(mydata$departure.time.diff) / mydata$departure.time.lag)

mydata$miss.car.chose.car.lag <- ifelse(mydata$got.car.chose.car.lag == 1, 0, 1)

# departure.time.diff ~ alpha + beta1 * got.car.chose.car.lag + beta2 * miss.car.chose.car.lag
                                            
fit <- lm(departure.time.diff ~ got.car.chose.car.lag, data = mydata[mydata$round > 29,])
summary(fit)

fit <- lm(departure.time.diff.share ~ got.car.chose.car.lag, data = mydata[mydata$round < 3,])
summary(fit)




x <- mydata$got.car.chose.car.lag
y <- log(mydata$departure.time.diff.share)
LL <- function(beta0, beta1, mu, sigma) {
    R = y - x * beta1 - beta0
    #
    R = suppressWarnings(dnorm(R, mu, sigma, log = TRUE))
    #
    -sum(R)
}

fit <- mle(LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma=1))

fit <- mle(LL, start = list(beta0 = 5, beta1 = 2, mu = 0, sigma=1))
summary(fit)

fit <- mle(LL, start = list(beta0 = 2, beta1 = 1.5, sigma=1),
           fixed = list(mu = 0), nobs = length(y))

summary(fit)


library(bbmle)

fit <- mle2(minuslogl = LL, start = list(beta0 = -23, beta1 = 20, mu = 0, sigma = 1))
summary(fit)       

# LME
library(gvlma)
library(grid)
library(Matrix)
library(lme4)

fit.model <- lmer(departure.time.diff ~ got.car.chose.car.lag + (1 | player) + (1 | session), data = mydata)

fit.null <- lmer(departure.time.diff ~ (1 | player) + (1 | session) + poly(round, 2), data = data)

summary(fit.null)

anova(fit.null, fit.model)

coef(fit)


# Testing significance

fit.null <- lmer(d.pub.previous ~ (1 | p.id) + (1 | session), data = pr)

fit.model <- lmer(d.pub.previous ~ com + (1 | p.id) + (1 | session), data = pr)

anova(fit.null, fit.model)


p <- ggplot(data, aes(round, departure.time.diff))
p <- p + geom_jitter()
p <- p + geom_smooth()
p


p <- ggplot(data, aes(as.factor(round), departure.time.diff, color=payoff.bus))
p <- p + geom_boxplot()
p <- p + facet_grid(decision ~ car.level)
p
