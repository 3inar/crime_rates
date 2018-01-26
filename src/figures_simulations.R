# figures from simulations

library(ProjectTemplate); load.project()

# Funnel plot with one simulated dataset

towns <- mean_crimes()
towns %<>% mutate(crimes=simulate_crimes(towns$population, towns$cpp, law='binomial'), raw_est=crimes/population)

rates_sim <- towns %>% mutate(crimes=simulate_crimes(towns$population, towns$cpp, law='binomial'),
                             raw_est=crimes/population) %>%
            transmute(population/1000,raw_est)

pdf(file="funnel_sim.pdf", width=6, height=5)
plot(rates_sim, type="n", main="Example of simulated crime rates",
     xlab="Population (in thousands)",
     ylab="Violent crime rate",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)
abline(h=mean(rates_sim$raw_est), lwd=2, col="grey")
points(rates_sim, pch=20)

dev.off()


# Loss function density plot


Nsim=1E5
errors <- raply(Nsim, {
  towns %<>% mutate(crimes=simulate_crimes(towns$population, towns$cpp, law='binomial'), raw_est=crimes/population)

  mu <- mean(towns$raw_est)
  sigsq <- var(towns$raw_est)
  alpha_p <- ((1 - mu) / sigsq - 1 / mu) * mu ^ 2
  beta_p <- alpha_p * (1 / mu - 1)

  towns %<>% mutate(smoothed_est=(crimes+alpha_p)/(population+alpha_p+beta_p), js_est=jse(raw_est, population, tomean=T))

  qerror_raw <- sum((towns$raw_est - towns$cpp)^2)  #mean((towns$raw_est - towns$cpp)^2)
  cor_raw <- cor(towns$raw_est, towns$cpp, method = 'spearman')

  qerror_smooth <- sum((towns$smoothed_est - towns$cpp)^2) #mean((towns$smoothed_est - towns$cpp)^2)
  cor_smooth <- cor(towns$smoothed_est, towns$cpp, method = 'spearman')

  qerror_js <- sum((towns$js_est - towns$cpp)^2) #mean((towns$smoothed_est - towns$cpp)^2)
  cor_js <- cor(towns$js_est, towns$cpp, method = 'spearman')

  c(qerror_raw, qerror_smooth, cor_raw, cor_smooth, qerror_js, cor_js)
}, .progress="text")

pdf(file="loss.pdf", width=6, height=5)
bwd <- 0.00001
plot(density(errors[,2]),type='n',
     xlim=c(min(errors[,2]), 0.001),
     ylim=c(0,8000),
     main='MC estimates of risk',
     xlab='Loss',
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)

lines(density(errors[, 1], bw=bwd),lwd=2)
abline(v=mean(errors[, 1]),lwd=2)

lines(density(errors[, 2], bw=bwd),lwd=2, col=2,lty=2)
abline(v=mean(errors[, 2]),lwd=2, col=2, lty=2)

lines(density(errors[, 5], bw=bwd),lwd=2, col="grey",lty=1)
abline(v=mean(errors[, 5]),lwd=2, col="grey", lty=1)

legend(x='topright', bty='n', lwd=2, lty=c(1,2,1), col=c(1, 2, "grey"), legend=c('MLE', 'S', 'JS'))

dev.off()

mean(errors[,1]) %>% round(5)
mean(errors[,2]) %>% round(5)
mean(errors[,5]) %>% round(5)
