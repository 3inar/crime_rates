# simulate n towns from the norwegian_crime data
simulate_towns <- function(n) {
  popc <- median_crimes()
  ret <- cbind(sample(popc$population, n, replace=T),
               sample(popc$cpp, n, replace=T))
  colnames(ret) <- c("population", "cpp")

  as_tibble(ret)
}


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
total_crime <- function() {
  norwegian_crime %>% group_by(place,year,population) %>% summarise(reports=sum(reports)) %>% ungroup
}

# median total crime rate for all years, 2014 population
mean_crimes <- function() {
  mediancrime <- total_crime() %>%
                   mutate(reps=reports/population) %>%
                   group_by(place) %>%
                   summarise(rate=mean(reps)) %>% ungroup
  pops2014 <- norwegian_crime %>%
              filter(year=="2014") %>% group_by(place, population) %>%
              summarise() %>% ungroup

  left_join(pops2014, mediancrime) %>% transmute(population, cpp=rate)
}
