# OceanAK data pull
# Kyle Shedd
# Wed Jul 18 10:14:50 2018

date()
setwd("V:/Analysis/5_Coastwide/Multispecies/Alaska Hatchery Research Program/PWS Pink/")
library(tidyverse)
library(lubridate)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### OceanAK ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# List of silly's for OceanAK filter
writeClipboard(paste(paste0("P", rep(streams, each = 5), yrs), collapse = ";"))

oceanak <- read_csv(file = "OceanAK/AHRP - Salmon Biological Data 2_PWS_2013-2017.csv") %>% 
  unite(SillySource, `Silly Code`, `Fish ID`, sep = "_", remove = FALSE)
dups <- oceanak %>% 
  group_by(SillySource) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  arrange(desc(n)) %>% 
  left_join(oceanak)

write_csv(x = dups, path = "OceanAK/AHRP - Salmon Biological Data 2_PWS_2013-2017_duplicates.csv")

# samples per stream per year
addmargins(table(oceanak$`Location Code`, oceanak$`Sample Year`))

# otolith code per stream per year
table(oceanak$`Location Code`, oceanak$`Otolith Mark Present`, oceanak$`Sample Year`, useNA = "always")
table(oceanak$`Location Code`, oceanak$`Otolith Mark Status Code`, oceanak$`Sample Year`, useNA = "always")

# add otolith_read logical, stream as factor, and year
oceanak_mod <- oceanak %>% 
  mutate(otolith_read = !is.na(`Otolith Mark Status Code`)) %>% 
  mutate(stream = factor(x = `Location Code`, levels = c("Gilmour Creek", "Paddy Creek", "Erb Creek", "Hogan Creek", "Stockdale Creek"))) %>% 
  rename(year = `Sample Year`)

# table of stream, year, and otolith_read
table(oceanak_mod$stream, oceanak_mod$otolith_read, oceanak_mod$year)

# table of samples per stream per year
addmargins(table(oceanak_mod$stream, oceanak_mod$year))

# table of otoliths read by stream and year
oceanak_mod %>%
  filter(otolith_read == TRUE) %>% 
  group_by(stream, year) %>% 
  summarise(freq = n()) %>% 
  spread(year, freq)

# table of otolith mark read by stream and year
oceanak_mod %>%
  filter(`Otolith Mark Present` == "NO") %>% 
  group_by(stream, year) %>% 
  summarise(freq = n()) %>% 
  spread(year, freq)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Stream Specimens ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pws <- read_csv(file = "Stream Specimens/StreamSpecimens_PWS_2013-2017_CSVReport.csv")
pws

# Need year and stream (StreamName has multiple tribs, as does ADFGStreamCode)
unique(pws$StreamName)
unique(pws$ADFGStreamCode)

# Create level_key for recode for stream
level_key = list("Erb C" = "Erb", 
                 "Gilmour C" = "Gilmour",
                 "Gilmour Right Trib Below Lake" = "Gilmour",
                 "Hogan Bay" = "Hogan",
                 "Paddy C" = "Paddy", 
                 "Paddy Left Trib" = "Paddy", 
                 "Paddy Lower Right Trib" = "Paddy",
                 "Stockdale C" = "Stockdale", 
                 "Stockdale Right Trib" = "Stockdale")

# Add stream and year
pws_mod <- pws %>% 
  mutate(year = year(as_date(x = SurveyDate, format = "%m/%d/%Y", tz = "US/Alaska"))) %>% 
  mutate(stream = recode(.x = StreamName, !!!level_key))

addmargins(table(pws_mod$stream, pws_mod$year))

table(pws_mod$stream, pws_mod$MarkPresent, pws_mod$year, useNA = "always")
table(pws_mod$stream, pws_mod$MarkStatusDescription, pws_mod$year, useNA = "always")

table(pws_mod$MarkPresent, useNA = "always")
table(pws_mod$MarkId, useNA = "always")
table(pws_mod$MarkStatusDescription, useNA = "always")
table(pws_mod$MarkStatus, pws$MarkStatusDescription, useNA = "always")
