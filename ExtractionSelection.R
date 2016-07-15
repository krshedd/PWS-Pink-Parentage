
setwd("V:/Analysis/5_Coastwide/Multispecies/Alaska Hatchery Research Program/PWS Pink")

# Read in OceanAK data as data.table (lightning fast!)
require(data.table)
oceanak.dt <- fread(input = "OceanAK 14-7-2015 Salmon Biological Data All Stockdale and Hogan.txt")  # amazing
str(oceanak.dt)
# Convert to data.frame
oceanak.df <- data.frame(oceanak.dt)

# Read in Finsight data (need to join Spawning State that isn't available on OceanAK)
finsight.dt <- fread(input = "Finsight 13-7-2015 All Stockdale and Hogan.txt")
str(finsight.dt)
# Convert to data.frame
finsight.df <- data.frame(finsight.dt)


# Create data keys for both (barcode + position)
oceanak.df$Key <- paste(oceanak.df$DNA.Tray.Code, oceanak.df$DNA.Tray.Well.Code, sep = "_")
finsight.df$Key <- paste(finsight.df$Sample.Tray.Id, finsight.df$Sample.Cell, sep = "_")


# Use data key to match up Stream Status from Finsight
# First confirm that other fields match up with Key
key.match <- match(oceanak.df$Key, finsight.df$Key)

table(oceanak.df$Sex == finsight.df$Sex[key.match])  # perfect match
table(oceanak.df$Length.Mm == finsight.df$MEHLength[key.match])  # perfect match
table(sapply(oceanak.df$Otolith.Mark.Present, function(ind) {unlist(strsplit(x = ind, split = ""))[1]} ) == finsight.df$Mark.Present[key.match])  # some issues...

# Confirm that these indeed do not match up
oto.match <- sapply(oceanak.df$Otolith.Mark.Present, function(ind) {unlist(strsplit(x = ind, split = ""))[1]} ) == finsight.df$Mark.Present[key.match]  # where are conflicts?
cbind(oceanak.df$Otolith.Mark.Present[which(oto.match == FALSE)],
      finsight.df$Mark.Present[match(oceanak.df$Key, finsight.df$Key)][which(oto.match == FALSE)]
)

# Which fish do not match up?
cbind(oceanak.df$Key[which(oto.match == FALSE)],
      finsight.df$Key[match(oceanak.df$Key, finsight.df$Key)][which(oto.match == FALSE)]
)

# This is evidence that finsight only has 1st read data, not 2nd reads
# This is why we have a data warehouse that is constantly updating from various department databases
# Confirmed with Stacy Vega in CDV that 1300001557_9 was "wild" on 1st read, but "hatchery" on 2nd

# Given that we trust Finsight field data, append Spawning State data to oceanak.df
oceanak.df$Spawning.State <- finsight.df$Spawning.State[key.match]


# We don't have finsight data on fish with unread otoliths from 2015
oceanak.df[which(oceanak.df$DNA.Tray.Code == 0000002876), ]
oceanak.df[which(oceanak.df$DNA.Tray.Code == 0000002876), "Otolith.Mark.Present"]

table(oceanak.df$Otolith.Mark.Present, oceanak.df$Spawning.State, useNA = "ifany")

# Subset data such that we only keep non-rotting fish with known sex
table(oceanak.df$Sex, useNA = "ifany")
table(oceanak.df$Spawning.State, useNA = "ifany")
table(oceanak.df$Otolith.Mark.Present, useNA = "ifany")





str(oceanak.df)
str(subset(x = oceanak.df, subset = Sex == "F" | Sex == "M"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Subset for extraction

# All Hogan 13 with otolith data
Hogan13Extract <- collections.df[which(collections.df$Silly.Code == "PHOGAN13" & collections.df$Otolith.Mark.Present == "YES" | collections.df$Silly.Code == "PHOGAN13" & collections.df$Otolith.Mark.Present == "NO"),
                                 c("Silly.Code", "DNA.Tray.Code", "DNA.Tray.Well.Code", "DNA.Tray.Well.Pos", "Otolith.Mark.Present", "Sex")]

# All Hogan 14 natural and 454 hatchery
Hogan14Extract <- collections.df[sort(c(which(collections.df$Silly.Code == "PHOGAN14" & collections.df$Otolith.Mark.Present == "NO"),
                                        sample(x = which(collections.df$Silly.Code == "PHOGAN14" & collections.df$Otolith.Mark.Present == "YES"), size = 454, replace = FALSE))),
                                 c("Silly.Code", "DNA.Tray.Code", "DNA.Tray.Well.Code", "DNA.Tray.Well.Pos", "Otolith.Mark.Present", "Sex")]

# All Hogan 15 natural with otolith data
Hogan15Extract <- collections.df[which(collections.df$Silly.Code == "PHOGAN15" & collections.df$Otolith.Mark.Present == "NO"),
                                 c("Silly.Code", "DNA.Tray.Code", "DNA.Tray.Well.Code", "DNA.Tray.Well.Pos", "Otolith.Mark.Present", "Sex")]

# All Stockdale 14 natural and 455 hatchery
Stockdale14Extract <- collections.df[sort(c(which(collections.df$Silly.Code == "PSTOCK14" & collections.df$Otolith.Mark.Present == "NO"),
                                            sample(x = which(collections.df$Silly.Code == "PSTOCK14" & collections.df$Otolith.Mark.Present == "YES"), size = 455, replace = FALSE))),
                                     c("Silly.Code", "DNA.Tray.Code", "DNA.Tray.Well.Code", "DNA.Tray.Well.Pos", "Otolith.Mark.Present", "Sex")]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create single extraction list
extraction.list <- rbind(Hogan13Extract, Hogan14Extract, Hogan15Extract, Stockdale14Extract)
str(extraction.list)

table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present)

sum(table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present))  # number of fish to extract
sum(table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present)) / 95  # number of plates (divisible by 12 for QC purposes)

## Need to convert DNA.Tray.Code to character and then add leading zeros for 2015
extraction.list$DNA.Tray.Code <- as.character(extraction.list$DNA.Tray.Code)

table(nchar(extraction.list$DNA.Tray.Code))  # either 4 or 10

extraction.list$DNA.Tray.Code[nchar(extraction.list$DNA.Tray.Code) == 4] <- paste("000000", extraction.list$DNA.Tray.Code[nchar(extraction.list$DNA.Tray.Code) == 4], sep = '')

table(nchar(extraction.list$DNA.Tray.Code))  # resolved


extraction.list$DNA.Tray.Code <- paste("'", extraction.list$DNA.Tray.Code, "'", sep = '')

write.table(x = extraction.list, file = "ExtractionList13072016quote.txt", sep = "\t")
