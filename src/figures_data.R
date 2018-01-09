# data associated with article publication
load('src/data/norwegian_crime.rda')

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
plot(rates_2016_2015, type="n", main="Regression to the mean",
     sub=paste0("(correlation = ", signif(correlation, 2),")"),
     xlim=limits, ylim=limits,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)
abline(0, 1, lwd=2, col="grey", lty=2)
abline(h=mean(rates_2016_2015$`2015`), lwd=2, col="grey")
abline(lm_fit, lwd=2, col="black")
points(rates_2016_2015, pch=20)

dev.off()


