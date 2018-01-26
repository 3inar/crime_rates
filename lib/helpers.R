
simulate_crimes <- function(population, cpp, law='poisson') {
  if(law=='poisson'){ # draw from the poisson w/ rate = cpp for each town
    rates <- population*cpp
    return(aaply(rates, 1, function (x) rpois(1, x)))
  }
  if(law=='binomial'){ # draw from binomial(population city,cpp city)
    return(rbinom(n=length(population),size=population,prob=cpp))
  }
}


# total no. crime reports by year
total_crime <- function(crime="¬ Vold og mishandling") {
  norwegian_crime %>% group_by(place,year,population)  %>%
    filter(crime_type == crime) %>% #Only violent crimes!
    summarise(reports=sum(reports)) %>% ungroup
}

# median total crime rate for all years, 2014 population
mean_crimes <- function(crime="¬ Vold og mishandling") {
  mediancrime <- total_crime(crime) %>%
                   mutate(reps=reports/population) %>%
                   group_by(place) %>%
                   summarise(rate=mean(reps)) %>% ungroup
  pops2014 <- norwegian_crime %>%
              filter(year=="2014") %>% group_by(place, population) %>%
              summarise() %>% ungroup

 left_join(pops2014, mediancrime, by='place') %>%
   mutate(rate=replace(rate,is.na(rate),0)) %>% #Because some cities have no violent crime reported
   transmute(population, cpp=rate)
}

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

jse <- function(rate, population, tomean=T) {
  vars <- rate*(1-rate)/population
  pooled_var <- sum(vars*(population - 1))/sum(population - 1)

  if(tomean) {
    origin <- mean(rate)
  } else {
    origin <- 0
  }
  sfactor <- (1 - (length(rate) - 2)*pooled_var/sum((rate - origin)^2))
  origin + (rate - origin)*sfactor
}
