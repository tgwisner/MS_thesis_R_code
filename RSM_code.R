# Fitting and estimating GARCH parameters

# log-likelihood
llgarch11 <- function(par, x) {
  mu <- par[1]; omega <- par[2]; alpha <- par[3]; beta <- par[4]
  e2 <- (x-mu)^2
  e2t <- omega + alpha*c(mean(e2), e2[-length(x)])
  s2 <- filter(e2t, beta, "recursive", init = mean(e2))
  0.5*sum(log(2*pi) + log(s2) + e2/s2)
}

x <- sectors$XLU
x <- log(x[2:4158]/x[1:4157])
mu <- mean(x); omega <- 0.1*var(x); alpha <- 0.1; beta <- 0.8
par <- c(mu, omega, alpha, beta)
low <- c(-10*abs(mu), 0,0,0)
up <- c(10*abs(mu), 100*abs(mu), 1, 1)
fit <- nlminb(star=par, objective=llgarch11, x = x, lower = low, upper = up)$par
names(fit) <- c("mu", "omega", "alpha", "beta")
round(fit, 5)
