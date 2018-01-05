# Using Finsight Data to pick list of PWS Pink Salmon to read Otoliths
# Paddy, Erb, Gilmour 2017 pick 8 full trays
# Stockdale and Hogan pick every other tray

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hogan
Hogan <- subset(x = finsight.df[iord, ], subset = Stream == "Hogan")

plot(aggregate(Num.Otoliths ~ Sample.Date, data = Hogan, sum), type = "h")

Hogan.ind <- seq(from = 1, by = 2, length.out = round(nrow(Hogan)/2))
Hogan[Hogan.ind, ]  # All full but one


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
