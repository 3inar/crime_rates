library(ProjectTemplate); load.project()
load("src/data/norwegian_crime.rda")

rates_all_years <- norwegian_crime %>% filter(crime_type == "¬ Vold og mishandling") %>%
  transmute(rate=reports/population, year, place, population, reports)
pops2016 <- norwegian_crime %>% filter(crime_type == "¬ Vold og mishandling", year=="2016") %>%
  select(pop_2016=population, place)
rates_all_years <- join(rates_all_years, pops2016)


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

pdf(file="timeseries.pdf", width=9, height=5)
par(mfrow=c(2,3))
for (town in c(worst_2016, best_2016)) {
  towndata <- filter(shrinkage, place==town)
  pop <- filter(towndata, year=="2016") %>% select(population) %>% .[[1]]

  plot(year_limit, rate_limit, type="n",
       main=paste0(town, "\n(population: ", as.character(pop), ")"), xlab="", ylab="",
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)

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

dev.off()


################################################################################
#                               sticksplot
################################################################################
x <- shrinkage %>% filter(year==2016) %>% select(place, rate, ebayes_rate) %>%
  mutate(place=(place=="Oslo kommune")) %>% arrange(place)
rate_limit <- c(min(x[,-1]), max(x[,-1]))

pdf(file="sticks.pdf", width=6, height=5)
plot(rate_limit, c(1,2), type="n", main="Shrinkage vs ordinary estimates",
     yaxt="n", ylab="", xlab="Crime rate estimate",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)
axis(2, at=1:2, labels=c("MLE", "S"), las=1, tick=F, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.2)
a_ply(x, 1, function(r) {
  color <- ifelse(r$place, "black", "grey")
  rates <- c(r$rate, r$ebayes_rate)
  yax <- c(1,2)
  points(rates, yax, col=color, pch=19)
  lines(rates, yax, lwd=3, col=color)
})
dev.off()
