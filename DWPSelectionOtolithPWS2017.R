# Using Finsight Data to pick list of PWS Pink Salmon to read Otoliths
# Paddy, Erb, Gilmour 2017 pick 8 full trays
# Stockdale and Hogan pick every other tray
# All remaining Stockdale trays due to extra Cordova Otolith lab capacity Wed Mar 14 15:34:32 2018

setwd("V:/Analysis/5_Coastwide/Multispecies/Alaska Hatchery Research Program/PWS Pink")
rm(list = ls())


# Read in Finsight data
require(data.table)
finsight.dt <- fread(input = "2017 HW Tray Inventory as of 10.18.17 FINAL to ADFG.txt")
str(finsight.dt)
# Convert to data.frame
finsight.df <- data.frame(finsight.dt)
finsight.df$Sample.Date <- as.Date(finsight.df$Sample.Date, format = "%m/%d/%Y")
str(finsight.df)


aggregate(Num.Otoliths ~ Stream, data = finsight.df, sum)
aggregate(Num.Otoliths ~ Stream, data = finsight.df, length)
aggregate(Num.Otoliths ~ Stream, data = finsight.df, function(x) {round(length(x) / 8)} )

table(is.na(finsight.df$Sample.Date))


iord <- order(finsight.df$Sample.Date)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Erb
Erb <- subset(x = finsight.df[iord, ], subset = Stream == "Erb")

plot(aggregate(Num.Otoliths ~ Sample.Date, data = Erb, sum), type = "h")

Erb.ind <- seq(from = 19, by = 39, length.out = 8)
Erb[Erb.ind, ]  # All full trays

# writeClipboard(as.character(Erb[Erb.ind, "Sample.Tray.Id.1"]))
# writeClipboard(as.character(Erb[Erb.ind, "Num.Otoliths"]))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Paddy
Paddy <- subset(x = finsight.df[iord, ], subset = Stream == "Paddy")

plot(aggregate(Num.Otoliths ~ Sample.Date, data = Paddy, sum), type = "h")

Paddy.ind <- seq(from = 14, by = 24, length.out = 8)
Paddy[Paddy.ind, ]  # All full but one

# writeClipboard(as.character(Paddy[Paddy.ind, "Sample.Tray.Id.1"]))
# writeClipboard(as.character(Paddy[Paddy.ind, "Num.Otoliths"]))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gilmour
Gilmour <- subset(x = finsight.df[iord, ], subset = Stream == "Gilmour")

plot(aggregate(Num.Otoliths ~ Sample.Date, data = Gilmour, sum), type = "h")

Gilmour.ind <- seq(from = 9, by = 16, length.out = 8)
Gilmour[Gilmour.ind, ]  # All full but one

# writeClipboard(as.character(Gilmour[Gilmour.ind, "Sample.Tray.Id.1"]))
# writeClipboard(as.character(Gilmour[Gilmour.ind, "Num.Otoliths"]))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Stockdale
Stockdale <- subset(x = finsight.df[iord, ], subset = Stream == "Stockdale")

plot(aggregate(Num.Otoliths ~ Sample.Date, data = Stockdale, sum), type = "h")

Stockdale.ind <- seq(from = 1, by = 2, length.out = round(nrow(Stockdale)/2))
Stockdale[Stockdale.ind, ]  # All full but one

# Pick all remaining Hogan fish in order to provide Cordova with addtional otoliths
# They have capacity for an additional 3K, but we'll send all just in case
# "Wed Mar 14 15:40:37 2018"
Stockdale.ind2 <- seq(from = 2, to = nrow(Stockdale), by = 2)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hogan
Hogan <- subset(x = finsight.df[iord, ], subset = Stream == "Hogan")

plot(aggregate(Num.Otoliths ~ Sample.Date, data = Hogan, sum), type = "h")

Hogan.ind <- seq(from = 1, by = 2, length.out = round(nrow(Hogan)/2))
Hogan[Hogan.ind, ]  # All full but one

# Pick 488 extra Hogan fish to replace missing/bad fish (0, 3, 4 otoliths per well)
# Pick 10 extra DWPs randomly
#"Fri Jan 05 14:04:24 2018"
Hogan.ind2 <- sort(sample(x = seq(nrow(Hogan))[-Hogan.ind], size = 10, replace = FALSE))
Hogan[Hogan.ind2, ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create Table ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(xlsx)

write.xlsx(x = Erb[Erb.ind, c("Stream", "Sample.Date", "Sample.Tray.Id", "Num.Otoliths", "Shipping.Box.Number")], file = "PWS Pink Otolith Separation DWPs 2017.xlsx", sheetName = "Erb", col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(x = Paddy[Paddy.ind, c("Stream", "Sample.Date", "Sample.Tray.Id", "Num.Otoliths", "Shipping.Box.Number")], file = "PWS Pink Otolith Separation DWPs 2017.xlsx", sheetName = "Paddy", col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(x = Gilmour[Gilmour.ind, c("Stream", "Sample.Date", "Sample.Tray.Id", "Num.Otoliths", "Shipping.Box.Number")], file = "PWS Pink Otolith Separation DWPs 2017.xlsx", sheetName = "Gilmour", col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(x = Stockdale[Stockdale.ind, c("Stream", "Sample.Date", "Sample.Tray.Id", "Num.Otoliths", "Shipping.Box.Number")], file = "PWS Pink Otolith Separation DWPs 2017.xlsx", sheetName = "Stockdale", col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(x = Hogan[Hogan.ind, c("Stream", "Sample.Date", "Sample.Tray.Id", "Num.Otoliths", "Shipping.Box.Number")], file = "PWS Pink Otolith Separation DWPs 2017.xlsx", sheetName = "Hogan", col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(x = Hogan[Hogan.ind2, c("Stream", "Sample.Date", "Sample.Tray.Id", "Num.Otoliths", "Shipping.Box.Number")], file = "PWS Pink Otolith Separation DWPs 2017.xlsx", sheetName = "Hogan2", col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(x = Stockdale[Stockdale.ind2, c("Stream", "Sample.Date", "Sample.Tray.Id", "Num.Otoliths", "Shipping.Box.Number")], file = "PWS Pink Otolith Separation DWPs 2017.xlsx", sheetName = "Stockdale2", col.names = TRUE, row.names = FALSE, append = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### All Remaining 2017 Hogan ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Need to otolith separate all remaining 2017 Hogan (originally did every other + 10 extra)

require(tidyverse)
oceanak <- read_csv("../OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv")
oceanak %>% 
  mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, side = "left", pad = "0")) %>% 
  filter(`Silly Code` == "PHOGAN17") %>% 
  group_by(`DNA Tray Code`) %>% 
  summarise(sum_na = sum(is.na(`Otolith Mark Present`)),
            n = n()) %>% 
  filter(sum_na == n)
# some of the dwp's that were separated, but all the otoliths were NA are still in there...

# New idea
# Got from 
# "V:\Analysis\5_Coastwide\Multispecies\Alaska Hatchery Research Program\PWS Pink\Otolith Separation/PWS Pink Otolith Separation DWPs 2017.xlsx"
# tab Hogan and Hogan2
hogan_1 <- as.numeric(readClipboard())
hogan_2 <- as.numeric(readClipboard())

hogan_already_separated <- c(hogan_1, hogan_2)

all_hogan <- oceanak %>% 
  filter(`Silly Code` == "PHOGAN17") %>% 
  pull(`DNA Tray Code`)

all_hogan <- unique(all_hogan)

hogan_to_separate <- setdiff(all_hogan, hogan_already_separated)

str_pad(string = hogan_to_separate, width = 10, side = "left", pad = "0") %>% 
  as_tibble() %>% 
  rename(`DNA Tray Code` = value) %>% 
  write_csv("../Otolith Separation/PHOGAN17_remaining_to_separate_20181219.csv")
