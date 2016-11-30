library(tidyverse)
library(stringr)

# Clean up crime_reports data
# ------------------------------------------------------------------------------

# colnames have the format [long table name].variable, extract the last bit
colnames(crime_reports) <- gsub("^(.*[.])", "", colnames(crime_reports))

# crime reports and reports/1000 people are in the same column. I'll simply drop
# the /1000 data. The year column contains ranges like 2007-2008, which we'll
# translate to a single year
crime_reports %<>% filter(statistikkvariabel=="Lovbrudd anmeldt (årlig gjennomsnitt)") %>%
                   select(-statistikkvariabel) %>%
                   mutate(tid=str_split(tid, pattern="-", simplify=T)[, 2])
colnames(crime_reports) <- c("place", "crime_type", "year", "reports")

# Harsdad and Bjarkøy merged in 2013, so there's two versions of Harstad,
# the current one and the pre-2013 one. Remove the entries that officially
# don't exist (eg. Bjarkøy after 2012, "old Harstad" entries from after 2012)
crime_reports %<>% filter(!(place=="Harstad" & year < 2013)) %>%
  filter(!(place=="Harstad (-2012)" & year > 2012)) %>%
  filter(!(place=="Bjarkøy" & year < 2013)) %>%
  filter(!(place=="Bjarkøy (-2012)" & year > 2012))

# Likewise, Mosvik and Inderøy were mreged in 2012
# note the crucial double space before (-2011) below...
crime_reports %<>% filter(!(place=="Mosvik" & year < 2012)) %>%
  filter(!(place=="Mosvik  (-2011)" & year > 2011)) %>%
  filter(!(place=="Inderøy" & year < 2012)) %>%
  filter(!(place=="Inderøy (-2011)" & year > 2011))

# Clean up population data
# ------------------------------------------------------------------------------
colnames(population) <- gsub("^(.*[.])", "", colnames(population))
population <- population %>% select(-statistikkvariabel)
colnames(population) <- c("place", "year", "population")

# Make handy joined data set
# ------------------------------------------------------------------------------
# there are some entries where the name of the town is for some reason missing,
# these will have population 0. Let's just remove them
norwegian_crime <- left_join(crime_reports, population) %>% na.omit %>%
                   filter(population > 0) %>%
                   filter(crime_type != "Alle lovbruddsgrupper")

# cleanup
rm(crime_reports)
rm(population)
