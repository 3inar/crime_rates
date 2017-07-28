library(ProjectTemplate); load.project()
library(plyr)




covered <- function(x, true_rate) {
  ebayes <- mean(true_rate > x$ebayes_lower & true_rate < x$ebayes_upper)
  standard <- mean(true_rate > x$standard_lower & true_rate < x$standard_upper)

  c(ebayes_coverage=ebayes, standard_coverage=standard)
}

crime_types <- norwegian_crime$crime_type %>% unique %>% sort
experiment <- alply(crime_types, 1, function(ctype) {
  towns <- mean_crimes(ctype)
  rlply(5000, function() {
    counts <- simulate_crimes(towns$population, towns$cpp, law="binomial")
    data <- tibble(rate=counts/towns$population, population=towns$population, reports=counts)
    data <- as.tibble(cbind(data, stats(data)))
    list(covered(data, towns$cpp), ctype)
  }, .progress="text")
}, .progress="text")



exp2 <- NULL
for (it in experiment) {
  for (i in it) {
    exp2 <- rbind(exp2, data.frame(eb=i[[1]][1], wld=i[[1]][2], type=i[[2]]))
  }
}

errors <- exp2[exp2$type=="¬ Vold og mishandling", 1:2]

pdf(file="coverage.pdf", width=6, height=5)
plot(density(errors[,2]),type='n',
     xlim=c(0.86, 0.96),
     ylim=c(0,45),
     main='Distribution over all MC simulations',
     xlab='Coverage probability',
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)


lines(density(errors[, 2]),lwd=2)
abline(v=mean(errors[, 2]),lwd=2)

lines(density(errors[, 1]),lwd=2, col=2,lty=2)
abline(v=mean(errors[, 1]),lwd=2, col=2, lty=2)


abline(v=0.95,lwd=2, col="grey")
legend(x='topleft', bty='n', lwd=2, lty=1:2, col=1:2, legend=c('Standard Wald', 'EB credible'))

dev.off()
