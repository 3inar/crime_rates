library(ProjectTemplate); load.project()
library(plyr)

covered <- function(x, true_rate) {
  ebayes <- mean(true_rate > x$ebayes_lower & true_rate < x$ebayes_upper)
  standard <- mean(true_rate > x$standard_lower & true_rate < x$standard_upper)

  c(ebayes_coverage=ebayes, standard_coverage=standard)
}

crime_type <- "¬ Vold og mishandling"
nsim <- 1E5

# experiment <- alply(crime_types, 1, function(ctype) {
towns <- mean_crimes(crime_type)
experiment <-  raply(nsim, function() {
  counts <- simulate_crimes(towns$population, towns$cpp, law="binomial")
  data <- tibble(rate=counts/towns$population, population=towns$population, reports=counts)
  data <- as.tibble(cbind(data, stats(data)))
  covered(data, towns$cpp)
}, .progress="text")
# }, .progress="text")



errors <- experiment

pdf(file="coverage.pdf", width=6, height=5)
plot(density(errors[,2]),type='n',
     xlim=c(0.86, 0.96),
     ylim=c(0,45),
     main='MC estimates of coverage probability',
     xlab='Coverage',
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)


bwd <- 0.002
lines(density(errors[, 2], bw=bwd),lwd=2)
abline(v=mean(errors[, 2]),lwd=2)

lines(density(errors[, 1], bw=bwd),lwd=2, col=2,lty=2)
abline(v=mean(errors[, 1]),lwd=2, col=2, lty=2)


abline(v=0.95,lwd=2, col="grey")
legend(x='topleft', bty='n', lwd=2, lty=1:2, col=1:2, legend=c('Standard Wald', 'EB credible'))

dev.off()
