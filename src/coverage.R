library(ProjectTemplate); load.project()
library(plyr)


stats <- function(x) {
  mu <- mean(x$rate)
  sigsq <- var(x$rate)
  alpha_p <- ((1 - mu) / sigsq - 1 / mu) * mu ^ 2
  beta_p <- alpha_p * (1 / mu - 1)

  alpha <- alpha_p+x$reports
  beta=beta_p + x$population - x$reports
  ebayes_rate <- (x$reports+alpha_p)/(x$population+alpha_p+beta_p)
  ebayes_lower <- qbeta(0.025, alpha, beta)
  ebayes_upper <- qbeta(0.975, alpha, beta)

  band <- qnorm(0.975)*sqrt(x$rate*(1-x$rate)/x$population)
  standard_upper <- x$rate+band
  standard_lower <- x$rate-band

  tibble(ebayes_rate, ebayes_upper, ebayes_lower, standard_upper, standard_lower)
}

covered <- function(x) {
  ebayes <- mean(x$rate > x$ebayes_lower & x$rate < x$ebayes_upper)
  standard <- mean(x$rate > x$standard_lower & x$rate < x$standard_upper)

  c(ebayes_coverage=ebayes, standard_coverage=standard)
}


experiment <- raply(1E4, function() {
  towns <- simulate_towns()
  counts <- simulate_crimes(towns$population, towns$cpp, law="binomial")
  data <- tibble(rate=towns$cpp, population=towns$population, reports=counts)
  data <- as.tibble(cbind(data, stats(data)))
  covered(data)
}, .progress="text")

errors <- cbind(experiment[, 2], experiment[, 1])

pdf(file="coverage.pdf", width=6, height=5)
plot(density(errors[,2]),type='n',
     xlim=c(0.86, 0.99),
     ylim=c(0,45),
     main='MC integration of coverage probabilities',
     xlab='Coverage probability',
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)


lines(density(errors[, 1]),lwd=2)
abline(v=mean(errors[, 1]),lwd=2)

lines(density(errors[, 2]),lwd=2, col=2,lty=2)
abline(v=mean(errors[, 2]),lwd=2, col=2, lty=2)


abline(v=0.95,lwd=2, col="grey")
legend(x='topleft', bty='n', lwd=2, lty=1:2, col=1:2, legend=c('Standard Wald', 'EB credible'))

dev.off()
