# Using OceanAK and Finsight Data to create extraction list of PWS Pink Salmon
# Stockdale + Hogan Bay 2013-2015

setwd("V:/Analysis/5_Coastwide/Multispecies/Alaska Hatchery Research Program/PWS Pink")
rm(list = ls())


# Read in OceanAK data as data.table (lightning fast!)
require(data.table)
oceanak.dt <- fread(input = "OceanAK 15-7-2015 Salmon Biological Data All Stockdale and Hogan.txt")  # amazing
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


# Remove fish with Keys that have missing tissue or that have double samples
# NOTE: OceanAK gave duplicate rows for fish that had missing tissue or double samples, so that is why I remove any fish with that Key
dim(oceanak.df)[1]
keys2remove <- oceanak.df$Key[c(which(!is.na(oceanak.df$Well.Has.More.Than.One.Sample)), which(!is.na(oceanak.df$Is.Missing.Paired.Data.Exists)))]
oceanak.df <- oceanak.df[!oceanak.df$Key %in% keys2remove, ]


# Subset data such that we only keep non-rotting fish with known sex
table(oceanak.df$Sex, useNA = "ifany")
table(oceanak.df$Spawning.State, useNA = "ifany")
table(oceanak.df$Otolith.Mark.Present, useNA = "ifany")

table(oceanak.df$Silly.Code, oceanak.df$Otolith.Mark.Present, oceanak.df$Sex, oceanak.df$Spawning.State, useNA = "ifany")



str(oceanak.df)
oceanak.sex.df <- subset(x = oceanak.df, subset = Sex == "F" | Sex == "M")
str(oceanak.sex.df)
table(oceanak.sex.df$Otolith.Mark.Present, useNA = "ifany")

oceanak.sex.oto.df <- subset(x = oceanak.sex.df, subset = Otolith.Mark.Present == "YES" | Otolith.Mark.Present == "NO")
str(oceanak.sex.oto.df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Subset for extraction

extraction.fields <- c("Key", "Silly.Code", "DNA.Tray.Code", "DNA.Tray.Well.Code", "DNA.Tray.Well.Pos", "Sample.Date", "Otolith.Mark.Present", "Sex", "Length.Mm", "Spawning.State")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## These silly's do NOT change, regardless of random vs, random+non-rot vs. stratified+non-rot
# All Hogan 13 with otolith data and sex
Hogan13Extract <- oceanak.sex.oto.df[which(oceanak.sex.oto.df$Silly.Code == "PHOGAN13"), extraction.fields]
table(Hogan13Extract$Otolith.Mark.Present)

# All Hogan 15 natural-origin with otolith data and sex
Hogan15Extract <- oceanak.sex.oto.df[which(oceanak.sex.oto.df$Silly.Code == "PHOGAN15" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"), extraction.fields]
table(Hogan15Extract$Otolith.Mark.Present)

# All Stockdale 15 natural-origin with otolith data and sex
Stockdale15Extract <- oceanak.sex.oto.df[which(oceanak.sex.oto.df$Silly.Code == "PSTOCK15" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"), extraction.fields]
table(Stockdale15Extract$Otolith.Mark.Present)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Random everything
# All Hogan 14 natural and 512 hatchery
Hogan14Extract <- oceanak.sex.oto.df[sort(c(
  which(oceanak.sex.oto.df$Silly.Code == "PHOGAN14" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"),
  sample(x = which(oceanak.sex.oto.df$Silly.Code == "PHOGAN14" & oceanak.sex.oto.df$Otolith.Mark.Present == "YES"), size = 512, replace = FALSE))),
  extraction.fields]
dim(Hogan14Extract)[1]; table(Hogan14Extract$Otolith.Mark.Present); table(Hogan14Extract$Otolith.Mark.Present, Hogan14Extract$Spawning.State)

# All Stockdale 13 hatchery and 866 natural
Stockdale13Extract <- oceanak.sex.oto.df[sort(c(
  sample(x = which(oceanak.sex.oto.df$Silly.Code == "PSTOCK13" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"), size = 866, replace = FALSE),
  which(oceanak.sex.oto.df$Silly.Code == "PSTOCK13" & oceanak.sex.oto.df$Otolith.Mark.Present == "YES"))),
  extraction.fields]
dim(Stockdale13Extract)[1]; table(Stockdale13Extract$Otolith.Mark.Present); table(Stockdale13Extract$Otolith.Mark.Present, Stockdale13Extract$Spawning.State)

# All Stockdale 14 natural and 512 hatchery
Stockdale14Extract <- oceanak.sex.oto.df[sort(c(
  which(oceanak.sex.oto.df$Silly.Code == "PSTOCK14" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"),
  sample(x = which(oceanak.sex.oto.df$Silly.Code == "PSTOCK14" & oceanak.sex.oto.df$Otolith.Mark.Present == "YES"), size = 512, replace = FALSE))),
  extraction.fields]
dim(Stockdale14Extract)[1]; table(Stockdale14Extract$Otolith.Mark.Present); table(Stockdale14Extract$Otolith.Mark.Present, Stockdale14Extract$Spawning.State)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Random everything, but since we are sub-sampling, we don't want rotting fish
# table(oceanak.sex.oto.df$Otolith.Mark.Present, oceanak.sex.oto.df$Spawning.State, oceanak.sex.oto.df$Silly.Code, useNA = "ifany")
# 
# # All Hogan 14 natural and 512 hatchery
# Hogan14Extract <- oceanak.sex.oto.df[sort(c(
#   which(oceanak.sex.oto.df$Silly.Code == "PHOGAN14" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"),
#   sample(x = which(oceanak.sex.oto.df$Silly.Code == "PHOGAN14" & oceanak.sex.oto.df$Otolith.Mark.Present == "YES" & oceanak.sex.oto.df$Spawning.State == "Alive" |
#                      oceanak.sex.oto.df$Silly.Code == "PHOGAN14" & oceanak.sex.oto.df$Otolith.Mark.Present == "YES" & oceanak.sex.oto.df$Spawning.State == "Grey Gill" |
#                      oceanak.sex.oto.df$Silly.Code == "PHOGAN14" & oceanak.sex.oto.df$Otolith.Mark.Present == "YES" & oceanak.sex.oto.df$Spawning.State == "Pink Gill"),
#          size = 512, replace = FALSE))),
#   extraction.fields]
# dim(Hogan14Extract)[1]; table(Hogan14Extract$Otolith.Mark.Present, Hogan14Extract$Spawning.State)
# 
# # All Stockdale 13 hatchery and 866 natural
# Stockdale13Extract <- oceanak.sex.oto.df[sort(c(
#   sample(x = which(oceanak.sex.oto.df$Silly.Code == "PSTOCK13" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO" & oceanak.sex.oto.df$Spawning.State == "Alive" |
#                      oceanak.sex.oto.df$Silly.Code == "PSTOCK13" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO" & oceanak.sex.oto.df$Spawning.State == "Grey Gill" |
#                      oceanak.sex.oto.df$Silly.Code == "PSTOCK13" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO" & oceanak.sex.oto.df$Spawning.State == "Pink Gill"),
#          size = 866, replace = FALSE),
#   which(oceanak.sex.oto.df$Silly.Code == "PSTOCK13" & oceanak.sex.oto.df$Otolith.Mark.Present == "YES"))),
#   extraction.fields]
# dim(Stockdale13Extract)[1]; table(Stockdale13Extract$Otolith.Mark.Present, Stockdale13Extract$Spawning.State)
# 
# # All Stockdale 14 natural and 512 hatchery
# Stockdale14Extract <- oceanak.sex.oto.df[sort(c(
#   which(oceanak.sex.oto.df$Silly.Code == "PSTOCK14" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"),
#   sample(x = which(oceanak.sex.oto.df$Silly.Code == "PSTOCK14" & oceanak.sex.oto.df$Otolith.Mark.Present == "YES" & oceanak.sex.oto.df$Spawning.State == "Alive" |
#                      oceanak.sex.oto.df$Silly.Code == "PSTOCK14" & oceanak.sex.oto.df$Otolith.Mark.Present == "YES" & oceanak.sex.oto.df$Spawning.State == "Grey Gill" |
#                      oceanak.sex.oto.df$Silly.Code == "PSTOCK14" & oceanak.sex.oto.df$Otolith.Mark.Present == "YES" & oceanak.sex.oto.df$Spawning.State == "Pink Gill"),
#          size = 512, replace = FALSE))),
#   extraction.fields]
# dim(Stockdale14Extract)[1]; table(Stockdale14Extract$Otolith.Mark.Present, Stockdale14Extract$Spawning.State)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Stratified when subsampling, but since we are sub-sampling, we don't want rotting fish
# table(oceanak.sex.oto.df$Otolith.Mark.Present, oceanak.sex.oto.df$Spawning.State, oceanak.sex.oto.df$Silly.Code, useNA = "ifany")
# 
# #~~~~~~~~~~~~~~~~~~
# # All Hogan 14 natural and 512 hatchery
# 
# # Subset out Hogan 14 Hatchery fish
# hogan14.hatchery.df <- subset(x = oceanak.sex.df, subset = Silly.Code == "PHOGAN14" & Otolith.Mark.Present == "YES")
# dim(hogan14.hatchery.df)[1]
# 
# # Spawning state by date
# table(hogan14.hatchery.df$Sample.Date, hogan14.hatchery.df$Spawning.State)
# 
# # Stratified number of fish to sample by date
# n.fish.per.date <- round(table(hogan14.hatchery.df$Sample.Date) / dim(hogan14.hatchery.df)[1] * 511.5)
# sum(n.fish.per.date)
# n.fish.per.date
# 
# # Confirm there are enough non-rotten fish per day
# n.fish.per.date.nonrot <- apply(table(hogan14.hatchery.df$Sample.Date, hogan14.hatchery.df$Spawning.State), 1, function(date) {sum(date[1:3])})
# n.fish.per.date.nonrot > n.fish.per.date
# 
# n.fish.per.date.samp <- apply(cbind(n.fish.per.date,
#                                     apply(table(hogan14.hatchery.df$Sample.Date, hogan14.hatchery.df$Spawning.State), 1, function(date) {sum(date[1:3])})),
#                               1, min)
# 
# # We will grab all non-rotten fish available, then randomly from whatever rotten ones are left to fill out the stratified
# n.fish.per.date.rotsamp <- (n.fish.per.date - n.fish.per.date.samp)[(n.fish.per.date - n.fish.per.date.samp) > 0]
# 
# n.fish.per.date.samp <- n.fish.per.date.samp[!n.fish.per.date.samp == 0]
# 
# sum(n.fish.per.date.samp, n.fish.per.date.rotsamp)
# 
# # Randomly sample non-rotten stratified fish per date
# hogan14.hatchery.norot.df <- subset(x = hogan14.hatchery.df, 
#                                         subset = Spawning.State == "Alive" |
#                                           Spawning.State == "Grey Gill" |
#                                           Spawning.State == "Pink Gill")
# table(hogan14.hatchery.norot.df$Sample.Date, hogan14.hatchery.norot.df$Spawning.State)
# 
# 
# hogan14.hatchery.norot.keys <- sort(c(
#   hogan14.hatchery.norot.df$Key[
#     unlist(sapply(seq_along(n.fish.per.date.samp), function(i) {
#       sample(x = which(hogan14.hatchery.norot.df$Sample.Date == names(n.fish.per.date.samp[i])), size = n.fish.per.date.samp[i], replace = FALSE)
#     } ))],
#   hogan14.hatchery.df$Key[
#     unlist(sapply(seq_along(n.fish.per.date.rotsamp), function(j) {
#       sample(x = which(hogan14.hatchery.df$Sample.Date == names(n.fish.per.date.rotsamp[j]) & hogan14.hatchery.df$Spawning.State == "Rotting"), 
#              size = n.fish.per.date.rotsamp[j], replace = FALSE)
#     } ))]
# ))
# 
# # Use key's to grab all PHOGAN14 fish
# Hogan14Extract <- oceanak.sex.oto.df[sort(c(
#   which(oceanak.sex.oto.df$Silly.Code == "PHOGAN14" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"),
#   which(oceanak.sex.oto.df$Key %in% hogan14.hatchery.norot.keys)
# )),  extraction.fields]
# dim(Hogan14Extract)[1]; table(Hogan14Extract$Otolith.Mark.Present, Hogan14Extract$Spawning.State)
# table(Hogan14Extract$Otolith.Mark.Present)
# 
# 
# #~~~~~~~~~~~~~~~~~~
# # All Stockdale 14 natural and 512 hatchery
# 
# # Subset out Stockdale 14 Hatchery fish
# stockdale14.hatchery.df <- subset(x = oceanak.sex.df, subset = Silly.Code == "PSTOCK14" & Otolith.Mark.Present == "YES")
# dim(stockdale14.hatchery.df)[1]
# 
# # Spawning state by date
# table(stockdale14.hatchery.df$Sample.Date, stockdale14.hatchery.df$Spawning.State)
# 
# # Stratified number of fish to sample by date
# n.fish.per.date <- round(table(stockdale14.hatchery.df$Sample.Date) / dim(stockdale14.hatchery.df)[1] * 512)
# sum(n.fish.per.date)
# n.fish.per.date
# 
# # Confirm there are enough non-rotten fish per day
# n.fish.per.date.nonrot <- apply(table(stockdale14.hatchery.df$Sample.Date, stockdale14.hatchery.df$Spawning.State), 1, function(date) {sum(date[1:3])})
# n.fish.per.date.nonrot > n.fish.per.date
# 
# n.fish.per.date.samp <- apply(cbind(n.fish.per.date,
#             apply(table(stockdale14.hatchery.df$Sample.Date, stockdale14.hatchery.df$Spawning.State), 1, function(date) {sum(date[1:3])})),
#       1, min)
# 
# # We will grab all non-rotten fish available, then randomly from whatever rotten ones are left to fill out the stratified
# n.fish.per.date.rotsamp <- (n.fish.per.date - n.fish.per.date.samp)[(n.fish.per.date - n.fish.per.date.samp) > 0]
# 
# sum(n.fish.per.date.samp, n.fish.per.date.rotsamp)
# # Randomly sample non-rotten stratified fish per date
# stockdale14.hatchery.norot.df <- subset(x = stockdale14.hatchery.df, 
#                                     subset = Spawning.State == "Alive" |
#                                       Spawning.State == "Grey Gill" |
#                                       Spawning.State == "Pink Gill")
# table(stockdale14.hatchery.norot.df$Sample.Date, stockdale14.hatchery.norot.df$Spawning.State)
# 
# 
# stockdale14.hatchery.norot.keys <- sort(c(
#   stockdale14.hatchery.norot.df$Key[
#     unlist(sapply(seq_along(n.fish.per.date.samp), function(i) {
#       sample(x = which(stockdale14.hatchery.norot.df$Sample.Date == names(n.fish.per.date.samp[i])), size = n.fish.per.date.samp[i], replace = FALSE)
#     } ))],
#   stockdale14.hatchery.df$Key[
#     unlist(sapply(seq_along(n.fish.per.date.rotsamp), function(j) {
#       sample(x = which(stockdale14.hatchery.df$Sample.Date == names(n.fish.per.date.rotsamp[j]) & stockdale14.hatchery.df$Spawning.State == "Rotting"), 
#              size = n.fish.per.date.rotsamp[j], replace = FALSE)
#     } ))]
# ))
# 
# # Use key's to grab all PSTOCK14 fish
# Stockdale14Extract <- oceanak.sex.oto.df[sort(c(
#   which(oceanak.sex.oto.df$Silly.Code == "PSTOCK14" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"),
#   which(oceanak.sex.oto.df$Key %in% stockdale14.hatchery.norot.keys)
# )),  extraction.fields]
# dim(Stockdale14Extract)[1]; table(Stockdale14Extract$Otolith.Mark.Present, Stockdale14Extract$Spawning.State)
# table(Stockdale14Extract$Otolith.Mark.Present)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create single extraction list
extraction.list <- rbind(Hogan13Extract, Hogan14Extract, Hogan15Extract, Stockdale13Extract, Stockdale14Extract, Stockdale15Extract)
str(extraction.list)

table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present)

sum(table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present))  # number of fish to extract
sum(table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present)) / 95  # number of plates
sum(table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present)) / 95 / 12 # number of plates (divisible by 12 for QC purposes)

## Need to convert DNA.Tray.Code to character and then add leading zeros for 2015
extraction.list$DNA.Tray.Code <- as.character(extraction.list$DNA.Tray.Code)

table(nchar(extraction.list$DNA.Tray.Code))  # either 4 or 10

extraction.list$DNA.Tray.Code[nchar(extraction.list$DNA.Tray.Code) == 4] <- paste("000000", extraction.list$DNA.Tray.Code[nchar(extraction.list$DNA.Tray.Code) == 4], sep = '')

table(nchar(extraction.list$DNA.Tray.Code))  # resolved

extraction.list$Key <- paste(extraction.list$DNA.Tray.Code, extraction.list$DNA.Tray.Well.Code, sep = "_")
extraction.list$DNA.Tray.Code <- paste("'", extraction.list$DNA.Tray.Code, "'", sep = '')
extraction.list$Key <- paste("'", extraction.list$Key, "'", sep = '')

write.table(x = extraction.list, file = "ExtractionList15072016.txt", sep = "\t")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
date()
#"Wed Aug 17 10:20:52 2016" Zach P. found some missing fish, decided to fill in with natural origin

# Figure out which 866 natural I picked, then add 5 more

# All natural fish
Stockdale13Natural <- oceanak.sex.oto.df[sort(
  which(oceanak.sex.oto.df$Silly.Code == "PSTOCK13" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO")),
  extraction.fields]
dim(Stockdale13Natural)[1]; table(Stockdale13Natural$Otolith.Mark.Present); table(Stockdale13Natural$Otolith.Mark.Present, Stockdale13Natural$Spawning.State)

# Read in Key for extract
Stockdale13ExtractNaturalKey <- readClipboard()

# Sample 5 fish that have not been extracted
Stockdale13ExtractAdditionalKey <- sample(x = Stockdale13Natural$Key[!Stockdale13Natural$Key %in% Stockdale13ExtractNaturalKey], size = 5, replace = FALSE)

Stockdale13ExtractAdditional <- oceanak.sex.oto.df[which(oceanak.sex.oto.df$Key %in% sort(Stockdale13ExtractAdditionalKey)), extraction.fields]

write.table(x = Stockdale13ExtractAdditional, file = "Stockdale13ExtractAdditional17082016.txt", sep = "\t")
