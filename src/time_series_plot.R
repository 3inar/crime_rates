library(ProjectTemplate); load.project()

rates_all_years <- norwegian_crime %>% filter(crime_type == "¬ Vold og mishandling") %>%
  transmute(rate=reports/population, year, place, population, reports)
pops2016 <- norwegian_crime %>% filter(crime_type == "¬ Vold og mishandling", year=="2016") %>%
  select(pop_2016=population, place)
rates_all_years <- join(rates_all_years, pops2016)

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

shrinkage <- alply(unique(rates_all_years$year), 1, function(yr) {
  dat <- filter(rates_all_years, year==yr)
  st <- stats(dat)
  as_tibble(cbind(dat,st))
})

shrinkage_2 <- NULL

for(x in shrinkage) {
  shrinkage_2 <- rbind(shrinkage_2, x)
}

shrinkage <- shrinkage_2; rm(shrinkage_2)

worst_2016 <- shrinkage %>% filter(year=="2016") %>% top_n(3, rate) %>% select(place) %>% .[[1]]
best_2016 <- shrinkage %>% filter(year=="2016") %>% top_n(-3, rate) %>% select(place) %>% .[[1]]
head(shrinkage)
shrinkage %<>% mutate(year=as.numeric(year))

year_limit <- c(min(shrinkage$year), max(shrinkage$year))
rate_limit <- c(min(shrinkage$ebayes_lower, shrinkage$standard_lower),
                max(shrinkage$ebayes_upper, shrinkage$standard_upper))

par(mfrow=c(2,3))
for (town in c(worst_2016, best_2016)) {
  towndata <- filter(shrinkage, place==town)
  pop <- filter(towndata, year=="2016") %>% select(population) %>% .[[1]]

  plot(year_limit, rate_limit, type="n",
       main=paste0(town, "\n(population: ", as.character(pop), ")"), xlab="", ylab="")

  points(towndata$year, towndata$rate, pch=20)
  lines(towndata$year, towndata$rate, lwd=2)
  for (i in 1:nrow(towndata)) {
    yr <- towndata[i, ]
    lines(rep(yr$year,2), as.vector(as.matrix(select(yr, standard_upper, standard_lower))), lwd=1.5)
  }

  points(towndata$year+0.1, towndata$ebayes_rate, pch=20, col="red")
  lines(towndata$year+0.1, towndata$ebayes_rate, col="red", lwd=2)

  for (i in 1:nrow(towndata)) {
    yr <- towndata[i, ]
    lines(rep(yr$year,2)+0.1, as.vector(as.matrix(select(yr, ebayes_lower, ebayes_upper))), lwd=1.5, col="red")
  }
}

