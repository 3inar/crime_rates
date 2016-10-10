library(rjstat)
library(magrittr)
library(tibble)

crime_report_url <-"http://data.ssb.no/api/v0/dataset/81194.json?lang=no"
population_url <- "http://data.ssb.no/api/v0/dataset/26975.json?lang=no"

# fetch crime reports
crime_reports <- crime_report_url %>% fromJSONstat %>% as.data.frame %>% as_tibble
population <- population_url %>% fromJSONstat %>% as.data.frame %>% as_tibble

rm(crime_report_url)
rm(population_url)
