# Purpose: make presentation figures for AHRP Science Panel Meeting on 12/7/18
# Kyle Shedd
# Thu Nov 29 13:10:20 2018

rm(list = ls())
date()
setwd("../PWS Pink/")
dir_figures <- "V:/Documents/5_Coastwide/Multispecies/AHRP/Meetings/12-7-18 Science Panel/Presentation figures/"
library(tidyverse)
library(lubridate)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### OceanAK ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# List of silly's for OceanAK filter
streams <- c("ERB", "HOGAN", "GILMOUR", "PADDY", "STOCK")
yrs <- 13:18
writeClipboard(paste(paste0("P", rep(streams, each = 5), yrs), collapse = ";"))

# Read in OceanAK data for ALL PWS pink samples 2013-2018 for pedigree streams
oceanak <- read_csv(file = "OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv") %>% 
  unite(SillySource, `Silly Code`, `Fish ID`, sep = "_", remove = FALSE) %>% 
  unite(TrayCodeID, `DNA Tray Code`, `DNA Tray Well Code`, sep = "_", remove = FALSE)

# add otolith_read logical, stream as factor, and year
oceanak_mod <- oceanak %>% 
  mutate(otolith_read = !is.na(`Otolith Mark Status Code`)) %>% 
  mutate(stream = factor(x = `Location Code`, levels = c("Gilmour Creek", "Paddy Creek", "Erb Creek", "Hogan Creek", "Stockdale Creek"))) %>% 
  rename(year = `Sample Year`) %>% 
  mutate(origin = case_when(`Otolith Mark Present` == "NO" ~ "natural",
                            `Otolith Mark Present` == "YES" ~ "hatchery")) %>% 
  mutate(origin = factor(origin, levels = c("natural", "hatchery")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Figure 1 - number of samples ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# table of stream, year, and otolith_read
table(oceanak_mod$stream, oceanak_mod$otolith_read, oceanak_mod$year)

# table of samples per stream per year
addmargins(table(oceanak_mod$stream, oceanak_mod$year, oceanak_mod$origin))

# table of otoliths read by stream and year
oceanak_mod %>%
  filter(otolith_read == TRUE) %>% 
  group_by(stream, year) %>% 
  summarise(freq = n()) %>% 
  spread(year, freq)

# histogram of samples per date per year per stream by origin
png(filename = paste0(dir_figures, "fig_1_samples_by_year_stream_origin.png"), width = 12, height = 6, units = "in", res = 300)
oceanak_mod %>% 
  mutate(julian_date = yday(`Sample Date`)) %>% 
  ggplot(aes(x = julian_date, fill = origin)) +
  geom_histogram(binwidth = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        text = element_text(size = 20),
        axis.text = element_text(size = 10)) +
  facet_grid(year ~ stream) +
  labs(fill = "Origin") +
  ylab("Number of Samples") +
  xlab("Day of Year") +
  ggtitle("AHRP PWS Pink - number of samples by year, stream, and origin")
dev.off()
