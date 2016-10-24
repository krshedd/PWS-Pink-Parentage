# Using Finsight Data to pick list of PWS Pink Salmon to read Otoliths
# Paddy, Erb, Gilmour 2016

setwd("V:/Analysis/5_Coastwide/Multispecies/Alaska Hatchery Research Program/PWS Pink")
rm(list = ls())


# Read in Finsight data
finsight.dt <- fread(input = "2016 HW Tray Inventory as of 9.23.16 FINAL to ADFG.txt")
str(finsight.dt)
# Convert to data.frame
finsight.df <- data.frame(finsight.dt)
finsight.df$Sample.Date <- as.Date(finsight.df$Sample.Date, format = "%m/%d/%Y")
str(finsight.df)


aggregate(Number.Otoliths ~ Stream, data = finsight.df, sum)
aggregate(Number.Otoliths ~ Stream, data = finsight.df, length)
aggregate(Number.Otoliths ~ Stream, data = finsight.df, function(x) {round(length(x) / 8)} )

table(is.na(finsight.df$Sample.Date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Erb
iord <- order(finsight.df$Sample.Date)
Erb <- subset(x = finsight.df[iord, ], subset = Stream == "Erb")

Erb.ind <- seq(from = 23, by = 23, length.out = 8)
Erb[Erb.ind, ]  # Fill last tray

Erb.ind[8] <- 181

Erb[Erb.ind, ]  # All full trays

writeClipboard(as.character(Erb[Erb.ind, "Sample.Tray.Id.1"]))
writeClipboard(as.character(Erb[Erb.ind, "Number.Otoliths"]))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Paddy
Paddy <- subset(x = finsight.df[iord, ], subset = Stream == "Paddy")

Paddy.ind <- seq(from = 14, by = 14, length.out = 8)
Paddy[Paddy.ind, ]  # Need to fix 5 plates

Paddy.ind <- Paddy.ind + 2
Paddy[Paddy.ind, ]  # Need to fix 3

Paddy[Paddy$Sample.Date == "2016-08-21", ]
Paddy.ind[2] <- 29
Paddy[Paddy.ind, ]  # Need to fix 2 more

Paddy[Paddy$Sample.Date == "2016-09-06", ]
Paddy.ind[7] <- 99
Paddy[Paddy.ind, ]  # Need to fix 1 more

Paddy[Paddy$Sample.Date == "2016-09-13", ]
Paddy.ind[8] <- 104
Paddy[Paddy.ind, ]  # All full enough (except 1st)

writeClipboard(as.character(Paddy[Paddy.ind, "Sample.Tray.Id.1"]))
writeClipboard(as.character(Paddy[Paddy.ind, "Number.Otoliths"]))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gilmour
Gilmour <- subset(x = finsight.df[iord, ], subset = Stream == "Gilmour")

Gilmour.ind <- seq(from = 12, by = 12, length.out = 8)
Gilmour[Gilmour.ind, ]  # Need to fix late plate

Gilmour[Gilmour$Sample.Date == "2016-09-09", ]
Gilmour.ind[8] <- 95
Gilmour[Gilmour.ind, ]  # All full trays

writeClipboard(as.character(Gilmour[Gilmour.ind, "Sample.Tray.Id.1"]))
writeClipboard(as.character(Gilmour[Gilmour.ind, "Number.Otoliths"]))
