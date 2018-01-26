# data associated with article publication
library(ProjectTemplate); load.project()
load('src/data/norwegian_crime.rda')   # use backup data in case ssb scheme changes

rates_2016 <- norwegian_crime %>% filter(crime_type == "¬ Vold og mishandling", year=="2016") %>%
  transmute(population/1000, rate=reports/population)

# funnel plot

## Note: I'm doing lwd=2 for thicker lines, pch=20 for the smallish points,
## and the cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2 for larger
## text.
pdf(file="funnel.pdf", width=6, height=5)
plot(rates_2016, type="n", main="Crime rates more variable for smaller towns",
     xlab="Population (in thousands)",
     ylab="2016 Violent crime rate",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)
abline(h=mean(rates_2016$rate), lwd=2, col="grey")
points(rates_2016, pch=20)

dev.off()

rates_2016_2015 <- norwegian_crime %>% filter(crime_type == "¬ Vold og mishandling", year=="2016" | year=="2015") %>%
  transmute(rate=reports/population, year, place) %>% spread(year, rate) %>% select(-place) %>% na.omit

lm_fit <- lm(`2016`~`2015`, data=rates_2016_2015)
correlation <- cor(rates_2016_2015)[1,2]
limits <- c(min(rates_2016_2015), max(rates_2016_2015))

# regression to the mean figure
pdf(file="regression.pdf", width=6, height=5)
plot(rates_2016_2015, type="n", main="Crime rates regress to the mean",
     sub=paste0("(correlation = ", signif(correlation, 2),")"),
     xlim=limits, ylim=limits,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)
abline(0, 1, lwd=2, col="grey", lty=2)
abline(h=mean(rates_2016_2015$`2015`), lwd=2, col="grey")
abline(lm_fit, lwd=2, col="black")
points(rates_2016_2015, pch=20)

dev.off()


# histogram of 2016 rates with prior overlaid

# method of moments alpha & beta
mu <- mean(rates_2016$rate)
sigsq <- var(rates_2016$rate)
alpha_p <- ((1 - mu) / sigsq - 1 / mu) * mu ^ 2
beta_p <- alpha_p * (1 / mu - 1)


pdf(file="rate_histogram.pdf", width=6, height=5)

hist(rates_2016$rate, nclass=20, col="grey", border = "white",
     main="Pooled violent crime rates, 2016",
     xlab="Rate",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2, probability = T)
curve(dbeta(x, alpha_p, beta_p), lwd=2, add = T, col="black")

dev.off()

# qq plot
pdf(file="qqplot.pdf", width=6, height=5)
set.seed(20180801)
qqplot(rbeta(500, alpha_p, beta_p), rates_2016$rate, pch=19, col="black",
       main="Q-Q plot of 2016 rates against fitted prior",
       xlab="Prior quantiles",
       ylab="Empirical 2016 quantiles",
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)
qqline(rates_2016$rate, distribution = function(p) qbeta(p, alpha_p, beta_p), col = "black", lwd=2)
dev.off()
