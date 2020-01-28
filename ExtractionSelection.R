# Using OceanAK and Finsight Data to create extraction list of PWS Pink Salmon

setwd("V:/Analysis/5_Coastwide/Multispecies/Alaska Hatchery Research Program/PWS Pink")
rm(list = ls())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP001: Create extraction list for ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Stockdale and Hogan 2013-2015

#### Read in OceanAK data ####
# Read in as data.table (lightning fast!)
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
## Subset for extraction ####

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
## Random everything ####
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create single extraction list ####
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

write.table(x = extraction.list, file = "Extraction/eP001_ExtractionList15072016.txt", sep = "\t")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fill in "missing" fish ####
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

write.table(x = Stockdale13ExtractAdditional, file = "eP001_Stockdale13ExtractAdditional17082016.txt", sep = "\t")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP002: Create extraction list for ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Stockdale and Hogan 2016 to meet NPRB (4,300 more fish) and SK (7,050 more fish) funding requirements

#### Read in OceanAK data ####
# Read in as data.table (lightning fast!)
require(data.table)
oceanak.dt <- fread(input = "OceanAK 10-5-2017 Salmon Biological Data 2016 Stockdale and Hogan.txt")  # amazing
str(oceanak.dt)
# Convert to data.frame
oceanak.df <- data.frame(oceanak.dt)

# Since we are sampling fish randomly with respect to spawning state, no need to get any Finsight data
# Everything we need is in OceanAK

# Create data keys for both (barcode + position)
oceanak.df$Key <- paste(oceanak.df$DNA.Tray.Code, oceanak.df$DNA.Tray.Well.Code, sep = "_")

# Reformat Sample Date
oceanak.df$Sample.Date <- as.Date(x = oceanak.df$Sample.Date, format = "%m/%d/%Y")

table(oceanak.df$Otolith.Mark.Present, oceanak.df$Silly.Code, useNA = "ifany")
table(oceanak.df$Is.Missing.Paired.Data.Exists, oceanak.df$Well.Has.More.Than.One.Sample, useNA = "ifany")


# Remove fish with Keys that have missing tissue or that have double samples
# NOTE: OceanAK gave duplicate rows for fish that had missing tissue or double samples, so that is why I remove any fish with that Key
dim(oceanak.df)[1]
keys2remove <- oceanak.df$Key[which(oceanak.df$Is.Missing.Paired.Data.Exists == 1 | oceanak.df$Well.Has.More.Than.One.Sample == 1)]
str(keys2remove)
oceanak.df <- oceanak.df[!oceanak.df$Key %in% keys2remove, ]

table(oceanak.df$Otolith.Mark.Present, oceanak.df$Sex, oceanak.df$Silly.Code, useNA = "ifany")

# Remove fish that do not have known sex
str(oceanak.df)
oceanak.sex.df <- subset(x = oceanak.df, subset = Sex == "F" | Sex == "M")
str(oceanak.sex.df)
table(oceanak.sex.df$Otolith.Mark.Present, useNA = "ifany")

# Remove fish that do not have known otolith state
oceanak.sex.oto.df <- subset(x = oceanak.sex.df, subset = Otolith.Mark.Present == "YES" | Otolith.Mark.Present == "NO")
str(oceanak.sex.oto.df)

table(oceanak.sex.oto.df$Otolith.Mark.Present, oceanak.sex.oto.df$Sex, oceanak.sex.oto.df$Silly.Code, useNA = "ifany")
table(oceanak.sex.oto.df$Otolith.Mark.Present, oceanak.sex.oto.df$Sample.Date, oceanak.sex.oto.df$Silly.Code, useNA = "ifany")

# Stray rate over time
plot(by(data = oceanak.sex.oto.df, INDICES = oceanak.sex.oto.df[, "Sample.Date"], FUN = function(x) {sum(x$Otolith.Mark.Present == "YES") / length(x$Otolith.Mark.Present)}), ylim = c(0, 1))
plot(by(data = subset(x = oceanak.sex.oto.df, subset = oceanak.sex.oto.df$Silly.Code == "PSTOCK16"), INDICES = subset(x = oceanak.sex.oto.df, subset = oceanak.sex.oto.df$Silly.Code == "PSTOCK16")[, "Sample.Date"], FUN = function(x) {sum(x$Otolith.Mark.Present == "YES") / length(x$Otolith.Mark.Present)}), ylim = c(0, 1))
plot(by(data = subset(x = oceanak.sex.oto.df, subset = oceanak.sex.oto.df$Silly.Code == "PHOGAN16"), INDICES = subset(x = oceanak.sex.oto.df, subset = oceanak.sex.oto.df$Silly.Code == "PHOGAN16")[, "Sample.Date"], FUN = function(x) {sum(x$Otolith.Mark.Present == "YES") / length(x$Otolith.Mark.Present)}), ylim = c(0, 1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Subset for extraction ####
extraction.fields <- c("Key", "Silly.Code", "DNA.Tray.Code", "DNA.Tray.Well.Code", "Sample.Date", "Otolith.Mark.Present", "Sex", "Length.Mm")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Random everything ####
# 4300 Hogan 16 natural-origin with otolith data and sex
Hogan16Extract <- oceanak.sex.oto.df[sort(sample(x = which(oceanak.sex.oto.df$Silly.Code == "PHOGAN16" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"), size = 4300, replace = FALSE)), extraction.fields]
table(Hogan16Extract$Otolith.Mark.Present)

# 7050 Stockdale 16 natural-origin with otolith data and sex
Stockdale16Extract <- oceanak.sex.oto.df[sort(sample(x = which(oceanak.sex.oto.df$Silly.Code == "PSTOCK16" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO"), size = 7050, replace = FALSE)), extraction.fields]
table(Stockdale16Extract$Otolith.Mark.Present)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create single extraction list ####
extraction.list <- rbind(Hogan16Extract, Stockdale16Extract)
str(extraction.list)

table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present)

sum(table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present))  # number of fish to extract
sum(table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present)) / 95  # number of plates
sum(table(extraction.list$Silly.Code, extraction.list$Otolith.Mark.Present)) / 95 / 12 # number of plates (divisible by 12 for QC purposes)

## Need to convert DNA.Tray.Code to character and then add leading zeros for 2016
extraction.list$DNA.Tray.Code <- as.character(extraction.list$DNA.Tray.Code)

table(nchar(extraction.list$DNA.Tray.Code))  # 5

extraction.list$DNA.Tray.Code[nchar(extraction.list$DNA.Tray.Code) == 5] <- paste0("00000", extraction.list$DNA.Tray.Code[nchar(extraction.list$DNA.Tray.Code) == 5])

table(nchar(extraction.list$DNA.Tray.Code))  # resolved

extraction.list$Key <- paste(extraction.list$DNA.Tray.Code, extraction.list$DNA.Tray.Well.Code, sep = "_")
extraction.list$DNA.Tray.Code <- paste0("'", extraction.list$DNA.Tray.Code, "'")
extraction.list$Key <- paste0("'", extraction.list$Key, "'")

# Write extraction list
write.table(x = extraction.list, file = "Extraction/eP002_ExtractionList10052017.txt", sep = "\t", row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Extra fish ####
# Write separate lists for all remaining natural-origin fish from these two sillys

#~~~~~~~~~~~~~~~~~~
# Hogan
Hogan16Natural <- oceanak.sex.oto.df[sort(which(oceanak.sex.oto.df$Silly.Code == "PHOGAN16" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO")), extraction.fields]
dim(Hogan16Natural)[1]; table(Hogan16Natural$Otolith.Mark.Present)

# All fish that have not been extracted
Hogan16ExtractAdditionalKey <- setdiff(Hogan16Natural$Key, Hogan16Extract$Key)

Hogan16ExtractAdditional <- oceanak.sex.oto.df[oceanak.sex.oto.df$Key %in% Hogan16ExtractAdditionalKey, extraction.fields]

write.table(x = Hogan16ExtractAdditional, file = "Extraction/eP002_PHOGAN16_ExtraFish_10502017.txt", sep = "\t", row.names = FALSE)

#~~~~~~~~~~~~~~~~~~
# Stockdale
Stockdale16Natural <- oceanak.sex.oto.df[sort(which(oceanak.sex.oto.df$Silly.Code == "PSTOCK16" & oceanak.sex.oto.df$Otolith.Mark.Present == "NO")), extraction.fields]
dim(Stockdale16Natural)[1]; table(Stockdale16Natural$Otolith.Mark.Present)

# All fish that have not been extracted
Stockdale16ExtractAdditionalKey <- setdiff(Stockdale16Natural$Key, Stockdale16Extract$Key)

Stockdale16ExtractAdditional <- oceanak.sex.oto.df[oceanak.sex.oto.df$Key %in% Stockdale16ExtractAdditionalKey, extraction.fields]

write.table(x = Stockdale16ExtractAdditional, file = "Extraction/eP002_PSTOCK16_ExtraFish_10502017.txt", sep = "\t", row.names = FALSE)


# Save
save.image("Extraction/eP002_ExtractionList10052017.RData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP003: Create extraction list for ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All remaining Stockdale and Hogan DWPs from 2015, previously we did every other tray

#### Read in OceanAK data ####
# Read in as data.table (lightning fast!)
require(data.table)
oceanak.dt <- fread(input = "OceanAK 15-7-2016 Salmon Biological Data All Stockdale and Hogan.txt")  # amazing
str(oceanak.dt)
# Convert to data.frame
oceanak.df <- data.frame(oceanak.dt)

# Read in Finsight data (need to join Spawning State that isn't available on OceanAK)
finsight.dt <- fread(input = "Finsight 13-7-2016 All Stockdale and Hogan.txt")
str(finsight.dt)
# Convert to data.frame
finsight.df <- data.frame(finsight.dt)


# Create data Key for both (barcode + position)
require(tidyverse)

# Only want spawning state from finsight, all other data is from OceanAK
finsight.df <- finsight.df %>% 
  mutate(Sample.Tray.Id = str_pad(string = Sample.Tray.Id, width = 10, pad = "0")) %>% 
  mutate(Key = paste(Sample.Tray.Id, Sample.Cell, sep = "_")) %>% 
  select(Key, Spawning.State)

# Join with finsight, filter for more than one sample, paired data, known sex, random with respect to spawning state
oceanak.df <- oceanak.df %>% 
  mutate(DNA.Tray.Code = str_pad(string = DNA.Tray.Code, width = 10, pad = "0")) %>% 
  mutate(Key = paste(DNA.Tray.Code, DNA.Tray.Well.Code, sep = "_")) %>% 
  left_join(finsight.df, by = "Key") %>% 
  filter(!Well.Has.More.Than.One.Sample %in% 1) %>% 
  filter(!Is.Missing.Paired.Data.Exists %in% 1) %>% 
  filter(Sex %in% c("M", "F"))

# Table silly by oto read
# NOTE: some unknown otoliths are from DWPs that have been read, most are from DWPs that haven't been read
table(oceanak.df$Silly.Code, oceanak.df$Otolith.Mark.Present)

# Which fields do we want?
extraction.fields <- c("Key", "Silly.Code", "DNA.Tray.Code", "DNA.Tray.Well.Code", "DNA.Tray.Well.Pos", "Sample.Date", "Otolith.Mark.Present", "Sex", "Length.Mm", "Spawning.State")

#~~~~~~~~~~~~~~~~~~
# Create extraction list for DWPs that have NOT been read
extraction_eP003_fullDWP.df <- oceanak.df %>% 
  filter(Silly.Code %in% c("PHOGAN15", "PSTOCK15")) %>% 
  group_by(DNA.Tray.Code) %>% 
  filter(all(Otolith.Mark.Present == "")) %>% 
  ungroup() %>% 
  select(extraction.fields)
str(extraction_eP003_fullDWP.df)

# Create extraction list for Hogan and Stockdale 2015, hatchery fish
extraction_eP003_cherryDWP.df <- oceanak.df %>% 
  filter(Silly.Code %in% c("PHOGAN15", "PSTOCK15")) %>% 
  filter(Otolith.Mark.Present %in% c("YES")) %>% 
  select(extraction.fields)
str(extraction_eP003_cherryDWP.df)


# Merge extraction lists
extraction_eP003.df <- rbind(extraction_eP003_fullDWP.df, extraction_eP003_cherryDWP.df)
str(extraction_eP003.df)


#~~~~~~~~~~~~~~~~~~
## Summary statistics
# How many fish per silly
table(extraction_eP003.df$Silly.Code)

# How many DWPs per silly
extraction_eP003.df %>% 
  group_by(Silly.Code, Otolith.Mark.Present) %>% 
  summarise(nDWP = n_distinct(DNA.Tray.Code)) %>% 
  spread(Silly.Code, nDWP)

# How many fish per silly
extraction_eP003.df %>% 
  group_by(Silly.Code, Otolith.Mark.Present) %>% 
  summarise(n = n()) %>% 
  spread(Silly.Code, n)

# Unique DWPs from unread
extraction_eP003.df %>% 
  filter(Otolith.Mark.Present == "") %>% 
  group_by(Silly.Code) %>% 
  distinct(DNA.Tray.Code)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create single extraction list ####

# Write extraction list
write.table(x = extraction_eP003.df, file = "Extraction/eP003_ExtractionList13022018.txt", sep = "\t", row.names = FALSE)
save.image("Extraction/eP003_ExtractionList13022018.RData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Error checking ####
extraction_eP003_fullDWP_n.df <- oceanak.df %>% 
  filter(Silly.Code %in% c("PHOGAN15", "PSTOCK15")) %>% 
  group_by(Silly.Code, DNA.Tray.Code) %>% 
  filter(all(Otolith.Mark.Present == "")) %>% 
  summarize(n = n()) %>% 
  select(c(DNA.Tray.Code, Silly.Code, n))

# Write extraction list
write.table(x = extraction_eP003_fullDWP_n.df, file = "Extraction/eP003_ExtractionList13022018_fullplate_n.txt", sep = "\t", row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP004: Create extraction list for ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All Stockdale from 2017
# 1K Hatchery and all Natural from Hogan 2017

#### Read in OceanAK data ####
require(tidyverse)
oceanak <- read_csv("../OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv")

oceanak <- oceanak %>% 
  mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, pad = "0", side = "left")) %>% 
  mutate(`DNA Tray Well Code` = str_pad(string = `DNA Tray Well Code`, width = 2, pad = "0", side = "left")) %>% 
  mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")) %>% 
  filter(`Well Has More Than One Sample` != 1) %>% 
  filter(`Is Missing Paired Data Exists` != 1) %>% 
  filter(Sex %in% c("M", "F"))


#### Filter for Stockdale Samples ####
Stockdale_17 <- oceanak %>% 
  filter(`Silly Code` == "PSTOCK17") %>% 
  filter(!is.na(`Otolith Mark Present`))

Stockdale_17 %>% 
  count(Sex, `Otolith Mark Present`)


#### Filter for Hogan Natural ####
Hogan_Natural_17 <- oceanak %>% 
  filter(`Silly Code` == "PHOGAN17") %>% 
  filter(`Otolith Mark Present`=="NO")

Hogan_Natural_17 %>% 
  count(Sex, `Otolith Mark Present`)


#### Filter for Hogan Hatchery ####
Hogan_Hatchery_17 <- oceanak %>% 
  filter(`Silly Code` == "PHOGAN17") %>% 
  filter(`Otolith Mark Present`=="YES") %>% 
  sample_n(size = 1000, replace = FALSE)

Hogan_Hatchery_17 %>% 
  count(Sex, `Otolith Mark Present`)


#### Verify that Hogan Hatchery samples are distributed throughout the run ####

oceanak %>% 
  filter(`Silly Code` == "PHOGAN17") %>% 
  filter(`Otolith Mark Present`=="YES") %>%
  ggplot(aes(x = `Sample Date`)) +
  geom_bar()

Hogan_Hatchery_17 %>% 
  ggplot(aes(x=`Sample Date`)) +
  geom_bar()


#### Combine tibbles ####

extraction_eP004 <- bind_rows(Stockdale_17, Hogan_Natural_17, Hogan_Hatchery_17) %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`)

write_csv(x = extraction_eP004, path = "../Extraction/eP004_Extraction_List_181219.csv")

save.image("../Extraction/eP004_Extraction_List_181219.RData")

extraction_eP004 %>% 
  count(`Silly Code`, `Tissue Type`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP005: Create extraction list for ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All remaining Stockdale from 2016 (hatchery and left over natural) + all natural Stockdale 2018 (offspring)

#### Read in OceanAK data ####
require(tidyverse)
og_names <- suppressMessages(names(read_csv(file = "../OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv", progress = FALSE)))
oceanak <- read_csv(file = "../OceanAK/Salmon Biological Data 2 Export from DFGCFRESP.csv")
names(oceanak) <- og_names

(oceanak <- oceanak %>% 
    mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, pad = "0", side = "left")) %>% 
    mutate(`DNA Tray Well Code` = str_pad(string = `DNA Tray Well Code`, width = 2, pad = "0", side = "left")) %>% 
    mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")) %>% 
    unite(col = "SillySource", c("Silly Code", "Fish ID"), sep = "_", remove = FALSE) %>% 
    filter(`Well Has More Than One Sample` != 1) %>% 
    filter(`Is Missing Paired Data Exists` != 1) %>% 
    filter(Sex %in% c("M", "F")))

#### Read in eP002 extraction list
(eP002_list <- read_delim(file = "../Extraction/eP002_ExtractionList10052017.txt", delim = "\t") %>% 
    mutate("DNA Tray Code" = str_remove(string = DNA.Tray.Code, pattern = "'")) %>% 
    mutate("DNA Tray Code" = str_remove(string = `DNA Tray Code`, pattern = "'")) %>% 
    mutate("DNA Tray Well Code" = str_pad(string = DNA.Tray.Well.Code, width = 2, side = "left", pad = "0")) %>% 
    mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")))

#### Read in eP002 extracted fish from iStrategy wells table dump
(eP002_extracted <- read_csv("../Extraction/eP002_Wells.csv") %>% 
    filter(`Silly Code` == "PSTOCK16") %>% 
    unite(col = "SillySource", c("Silly Code", "Fish"), sep = "_", remove = FALSE))

#### Filter for Stockdale 2016 Samples ####
Stockdale_16 <- oceanak %>% 
  filter(`Silly Code` == "PSTOCK16") %>% 
  filter(!is.na(`Otolith Mark Present`)) %>% 
  anti_join(eP002_list, by = "Key") %>% 
  anti_join(eP002_extracted, by = "SillySource")

Stockdale_16 %>% 
  count(`Otolith Mark Present`)


#### Filter for Stockdale 2018 Samples ####
Stockdale_Natural_18 <- oceanak %>% 
  filter(`Silly Code` == "PSTOCK18") %>% 
  filter(`Otolith Mark Present`=="NO")

Stockdale_Natural_18 %>% 
  count(Sex, `Otolith Mark Present`)


#### Combine tibbles ####

extraction_eP005 <- bind_rows(Stockdale_16, Stockdale_Natural_18) %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`)

write_csv(x = extraction_eP005, path = "../Extraction/eP005_Extraction_List_190716.csv")

save.image("../Extraction/eP005_Extraction_List_190716.RData")

extraction_eP005 %>% 
  count(`Silly Code`, `Tissue Type`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Update eP005: Stockdale Hatchery fish ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(tidyverse)
og_names <- suppressMessages(names(read_csv(file = "../OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv", progress = FALSE)))
oceanak <- read_csv(file = "../OceanAK/Salmon Biological Data 2 Export from DFGCFRESP.csv")
names(oceanak) <- og_names

(oceanak <- oceanak %>% 
    mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, pad = "0", side = "left")) %>% 
    mutate(`DNA Tray Well Code` = str_pad(string = `DNA Tray Well Code`, width = 2, pad = "0", side = "left")) %>% 
    mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")) %>% 
    unite(col = "SillySource", c("Silly Code", "Fish ID"), sep = "_", remove = FALSE) %>% 
    filter(`Well Has More Than One Sample` != 1) %>% 
    filter(`Is Missing Paired Data Exists` != 1) %>% 
    filter(Sex %in% c("M", "F")))

#### Filter for Stockdale 2018 Hatchery Samples ####
Stockdale_Hatchery_18 <- oceanak %>% 
  filter(`Silly Code` == "PSTOCK18") %>% 
  filter(`Otolith Mark Present`=="YES")

Stockdale_Hatchery_18 %>% 
  count(Sex, `Otolith Mark Present`)


#### Combine tibbles ####

extraction_eP005_addendum <- Stockdale_Hatchery_18 %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`)

write_csv(x = extraction_eP005_addendum, path = "../Extraction/eP005_Extraction_List_Addendum_190917.csv")

save.image("../Extraction/eP005_Extraction_List_Addendum_190917.RData")

extraction_eP005_addendum %>% 
  count(`Silly Code`, `Tissue Type`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP006: Create extraction list for ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All remaining Hogan from 2016 (hatchery and left over natural)
# Can't select Hogan 2018 yet, because we are uncertain if/how many hatchery-origin to select (are we sampling in 2020???)

#### Read in OceanAK data ####
require(tidyverse)
og_names <- suppressMessages(names(read_csv(file = "../OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv", progress = FALSE)))
oceanak <- read_csv(file = "../OceanAK/Salmon Biological Data 2 Export from DFGCFRESP.csv")
names(oceanak) <- og_names

(oceanak <- oceanak %>% 
    mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, pad = "0", side = "left")) %>% 
    mutate(`DNA Tray Well Code` = str_pad(string = `DNA Tray Well Code`, width = 2, pad = "0", side = "left")) %>% 
    mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")) %>% 
    unite(col = "SillySource", c("Silly Code", "Fish ID"), sep = "_", remove = FALSE) %>% 
    filter(`Well Has More Than One Sample` != 1) %>% 
    filter(`Is Missing Paired Data Exists` != 1) %>% 
    filter(Sex %in% c("M", "F")))

#### Read in eP002 extraction list
(eP002_list <- read_delim(file = "../Extraction/eP002_ExtractionList10052017.txt", delim = "\t") %>% 
    mutate("DNA Tray Code" = str_remove(string = DNA.Tray.Code, pattern = "'")) %>% 
    mutate("DNA Tray Code" = str_remove(string = `DNA Tray Code`, pattern = "'")) %>% 
    mutate("DNA Tray Well Code" = str_pad(string = DNA.Tray.Well.Code, width = 2, side = "left", pad = "0")) %>% 
    mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")))

#### Read in eP002 extracted fish from iStrategy wells table dump
(eP002_extracted <- read_csv("../Extraction/eP002_Wells.csv") %>% 
    filter(`Silly Code` == "PSTOCK16") %>% 
    unite(col = "SillySource", c("Silly Code", "Fish"), sep = "_", remove = FALSE))

#### Filter for Hogan 2016 Samples ####
Hogan_16 <- oceanak %>% 
  filter(`Silly Code` == "PHOGAN16") %>% 
  filter(!is.na(`Otolith Mark Present`)) %>% 
  anti_join(eP002_list, by = "Key") %>% 
  anti_join(eP002_extracted, by = "SillySource")

Hogan_16 %>% 
  count(`Otolith Mark Present`)

#### Combine tibbles ####

extraction_eP006 <- bind_rows(Hogan_16) %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`) %>% 
  arrange(`Silly Code`, `Fish ID`)

write_csv(x = extraction_eP006, path = "../Extraction/eP006_Extraction_List_190716.csv")

save.image("../Extraction/eP006_Extraction_List_190926.RData")

extraction_eP006 %>% 
  count(`Silly Code`, `Tissue Type`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP007: Create extraction list for ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Our next priority was Hogan 2018, but we don't have a firm decision on whether we are doing 2020 sampling yet,
# so I don't want to extract several thousand hatchery-origin 2018 fish if it isn't necessary
# The next priority after that was to run natural-origin 2019 fish from Hogan and Stockdale, but we don't have otolith reads yet.

# So we are skipping down to the NEXT priority, Gilmour even-years, and maybe some odd too
# All fish from Gilmour even years (2014, 2016, 2018)
# Worse case is we don't end up doing 2020 and we "waste" extracting a few hundred hatchery strays from 2018

#### Read in OceanAK data ####
require(tidyverse)
og_names <- suppressMessages(names(read_csv(file = "../OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv", progress = FALSE)))
oceanak <- read_csv(file = "../OceanAK/Salmon Biological Data 2 Export from DFGCFRESP.csv")
names(oceanak) <- og_names

## Modify for extraction list prep
oceanak <- oceanak %>% 
  replace_na(list(`Well Has More Than One Sample` = 0, `Is Missing Paired Data Exists` = 0)) %>% 
  mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, pad = "0", side = "left")) %>% 
  mutate(`DNA Tray Well Code` = str_pad(string = `DNA Tray Well Code`, width = 2, pad = "0", side = "left")) %>% 
  mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")) %>% 
  unite(col = "SillySource", c("Silly Code", "Fish ID"), sep = "_", remove = FALSE) %>% 
  filter(`Well Has More Than One Sample` != 1) %>% 
  filter(`Is Missing Paired Data Exists` != 1) %>% 
  filter(Sex %in% c("M", "F"))

## How many samples we got?
oceanak %>% 
  count(`Location Code`, `Sample Year`) %>% 
  spread(`Sample Year`, n, fill = 0)

## How about what we want to extract
oceanak %>% 
  filter(`Location Code` == "Gilmour Creek") %>%  #, `Sample Year` %in% c(2014, 2016, 2018)
  count(`Sample Year`, `Otolith Mark Present`) %>% 
  spread(`Sample Year`, n, fill = 0)

# Just go ahead and extract all 2014, 2016, 2017, and 2018 Gilmour
# Going to pass on 2015 for now, because we probably don't need several thousand parents for each origin
# We'll revisit 2015 after analyzing Stockdale/Hogan 2015/2017 and doing a sensitivity analysis on parent sample size
# Also passing on 2019 until we get otolith reads
# Worse case is we don't end up doing 2020 and we "waste" extracting a few hundred hatchery strays from 2018

# We need to otolith separate Gilmour 2015-2019, how many DWPs?
oceanak %>% 
  filter(`Location Code` == "Gilmour Creek") %>%  #, `Sample Year` %in% c(2014, 2016, 2018)
  distinct(`Sample Year`, `DNA Tray Code`) %>% 
  count(`Sample Year`) 



#### Filter for Gilmour Even-year Samples ####
Gilmour_14_16_17_18 <- oceanak %>% 
  filter(`Location Code` == "Gilmour Creek" & `Sample Year` %in% c(2014, 2016, 2017, 2018)) %>% 
  filter(!is.na(`Otolith Mark Present`) & `Sample Year` == 2014 | `Sample Year` %in% c(2016, 2017, 2018))  # need NA's from 2016 and 2018 as we don't have reads yet

Gilmour_14_16_17_18 %>% 
  count(`Sample Year`, `Otolith Mark Present`) %>% 
  spread(`Sample Year`, n, fill = 0)

#### Combine tibbles ####

extraction_eP007 <- bind_rows(Gilmour_14_16_17_18) %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`) %>% 
  arrange(`Silly Code`, `Fish ID`)

write_csv(x = extraction_eP007, path = "../Extraction/eP007_Extraction_List_191119.csv")

save.image("../Extraction/eP007_Extraction_List_191119.RData")

extraction_eP007 %>% 
  count(`Silly Code`, `Tissue Type`)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Otolith Transfer Extravaganza!!! ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Wed Nov 20 14:03:17 2019
# Heather is super swamped and doesn't have time to set up multiple small projects
# I suggested that while we have extra lab staff, they separate ALL remaining DWPs
# This is likely Paddy, Erb, and Gilmour 2015-2018

#### Read in OceanAK data ####
library(tidyverse)
library(lubridate)
og_names <- suppressMessages(names(read_csv(file = "../OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv", progress = FALSE)))
oceanak <- read_csv(file = "../OceanAK/AHRP Salmon Biological Data 20191119_1053.csv")
names(oceanak) <- og_names

## How many samples we got?
oceanak %>% 
  count(`Location Code`, `Sample Year`) %>% 
  spread(`Sample Year`, n, fill = 0)

## Modify for extraction list prep, NO FILTERING, this is tissue transfer, not extraction
oceanak <- oceanak %>% 
  replace_na(list(`Well Has More Than One Sample` = 0, `Is Missing Paired Data Exists` = 0)) %>% 
  mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, pad = "0", side = "left")) %>% 
  mutate(`DNA Tray Well Code` = str_pad(string = `DNA Tray Well Code`, width = 2, pad = "0", side = "left")) %>% 
  mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")) %>% 
  unite(col = "SillySource", c("Silly Code", "Fish ID"), sep = "_", remove = FALSE) %>% 
  mutate(otolith_read = !is.na(`Otolith Mark Status Code`) & `Otolith Mark Status Code` != "n") %>% 
  mutate(stream = factor(x = `Location Code`, levels = c("Gilmour Creek", "Paddy Creek", "Erb Creek", "Hogan Creek", "Stockdale Creek"))) %>% 
  rename(year = `Sample Year`) %>% 
  mutate(origin = case_when(`Otolith Mark Present` == "NO" ~ "natural",
                            `Otolith Mark Present` == "YES" ~ "hatchery")) %>% 
  mutate(origin = factor(origin, levels = c("natural", "hatchery"))) %>% 
  mutate(date = dmy(`Sample Date`))

  # filter(`Well Has More Than One Sample` != 1) %>% 
  # filter(`Is Missing Paired Data Exists` != 1) %>% 
  # filter(Sex %in% c("M", "F"))

## How many samples we got?
oceanak %>% 
  count(stream, year) %>% 
  spread(year, n, fill = 0)

# How many samples
oceanak %>% count()

# How many otoliths read
oceanak %>% 
  count(otolith_read)

# Get trays to otolith separate from Gilmour, Paddy, Erb 2015-2018
trays_to_separate <- oceanak %>% 
  group_by(year, stream, `Silly Code`, `DNA Tray Code`) %>% 
  count(otolith_read) %>% 
  spread(otolith_read, n, fill = 0) %>% 
  filter(`TRUE` == 0 & `FALSE` > 0 & year != 2019 & stream %in% c("Gilmour Creek", "Paddy Creek", "Erb Creek")) %>% 
  ungroup()

# How many per stream per year
trays_to_separate %>% 
  count(year, stream) %>% 
  spread(year, n, fill = 0)

# How many total
trays_to_separate %>% count()

# Write for Heather
trays_to_separate %>% 
  select(`Silly Code`, `DNA Tray Code`) %>% 
  write_csv(path = "../Extraction/DWP_otolith_transfer_paddy_erb_gilmour_2015-2018.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP008 + eP009: Create extraction list for ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Our next priority was Hogan 2018, but we don't have a firm decision on whether we are doing 2020 sampling yet,
# so I don't want to extract several thousand hatchery-origin 2018 fish if it isn't necessary
# The next priority after that was to run natural-origin 2019 fish from Hogan and Stockdale, but we don't have otolith reads yet.
# eP007 was Gilmour even-years + 2017

# This extraction list is going to look at doing the same thing we did with Gilmour for Paddy and Erb
# i.e. All fish from Paddy and Erb even years (2014, 2016, 2018) + maybe odd years?
# Worse case is we don't end up doing 2020 and we "waste" extracting a few hundred hatchery strays from 2018

#### Read in OceanAK data ####
require(tidyverse)
og_names <- suppressMessages(names(read_csv(file = "../OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv", progress = FALSE)))
oceanak <- read_csv(file = "../OceanAK/AHRP Salmon Biological Data 20191119_1053.csv")
names(oceanak) <- og_names

## Modify for extraction list prep
oceanak <- oceanak %>% 
  replace_na(list(`Well Has More Than One Sample` = 0, `Is Missing Paired Data Exists` = 0)) %>% 
  mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, pad = "0", side = "left")) %>% 
  mutate(`DNA Tray Well Code` = str_pad(string = `DNA Tray Well Code`, width = 2, pad = "0", side = "left")) %>% 
  mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")) %>% 
  unite(col = "SillySource", c("Silly Code", "Fish ID"), sep = "_", remove = FALSE) %>% 
  filter(`Well Has More Than One Sample` != 1) %>% 
  filter(`Is Missing Paired Data Exists` != 1) %>% 
  filter(Sex %in% c("M", "F"))

## How many samples we got?
oceanak %>% 
  count(`Location Code`, `Sample Year`) %>% 
  spread(`Sample Year`, n, fill = 0)

## How about what we want to extract
oceanak %>% 
  filter(`Sample Year` != 2013) %>%  # we have deep sixed this year
  count(`Sample Year`, `Otolith Mark Present`, `Location Code`) %>%  # how many 
  spread(`Otolith Mark Present`, n, fill = 0) %>%  # yes/no/na
  mutate(pHOS = YES / (YES + NO),  # calculate pHOS
         n = YES + NO + `<NA>`) %>%  # total sample size
  mutate(nHatchery = pHOS * n,  # how many projected hatchery fish
         nNatural = (1 - pHOS) * n) %>%  # and how many project natural fish
  filter(`Location Code` %in% c("Paddy Creek", "Erb Creek", "Gilmour Creek"))  # we want Paddy and Erb, use Gilmour to compare


# Just Paddy
oceanak %>% 
  filter(`Sample Year` != 2013) %>%  # we have deep sixed this year
  count(`Sample Year`, `Otolith Mark Present`, `Location Code`) %>%  # how many 
  spread(`Otolith Mark Present`, n, fill = 0) %>%  # yes/no/na
  mutate(pHOS = YES / (YES + NO),  # calculate pHOS
         nTotal = YES + NO + `<NA>`) %>%  # total sample size
  mutate(nHatchery = pHOS * nTotal,  # how many projected hatchery fish
         nNatural = (1 - pHOS) * nTotal) %>%  # and how many project natural fish
  filter(`Location Code` == "Paddy Creek")


# Just Erb
oceanak %>% 
  filter(`Sample Year` != 2013) %>%  # we have deep sixed this year
  count(`Sample Year`, `Otolith Mark Present`, `Location Code`) %>%  # how many 
  spread(`Otolith Mark Present`, n, fill = 0) %>%  # yes/no/na
  mutate(pHOS = YES / (YES + NO),  # calculate pHOS
         nTotal = YES + NO + `<NA>`) %>%  # total sample size
  mutate(nHatchery = pHOS * nTotal,  # how many projected hatchery fish
         nNatural = (1 - pHOS) * nTotal) %>%  # and how many project natural fish
  filter(`Location Code` == "Erb Creek")




# Just go ahead and extract all 2014, 2016, and 2018 Paddy and Erb
# Going to pass on 2015 and 2017 for now, because we probably don't need several thousand parents for each origin
# We'll revisit 2015 and 2017 after analyzing Stockdale/Hogan 2015/2017 and doing a sensitivity analysis on parent sample size
# Also passing on 2019 until we get otolith reads
# Worse case is we don't end up doing 2020 and we "waste" extracting a few hundred hatchery strays from 2018

# We need to otolith separate Paddy + Erb 2015-2019, how many DWPs?
# oceanak %>% 
#   filter(`Location Code` == "Gilmour Creek") %>%  #, `Sample Year` %in% c(2014, 2016, 2018)
#   distinct(`Sample Year`, `DNA Tray Code`) %>% 
#   count(`Sample Year`) 

#### Filter for Paddy/Erb Even-year Samples ####
Paddy_14_16_18 <- oceanak %>% 
  filter(`Location Code` %in% c("Paddy Creek") & `Sample Year` %in% c(2014, 2016, 2018)) %>% 
  filter(!is.na(`Otolith Mark Present`) & `Sample Year` == 2014 | `Sample Year` %in% c(2016, 2018))  # need NA's from 2016 and 2018 as we don't have reads yet

Paddy_14_16_18 %>% 
  count(`Sample Year`, `Otolith Mark Present`) %>% 
  spread(`Sample Year`, n, fill = 0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Erb_14_16_18 <- oceanak %>% 
  filter(`Location Code` %in% c("Erb Creek") & `Sample Year` %in% c(2014, 2016, 2018)) %>% 
  filter(!is.na(`Otolith Mark Present`) & `Sample Year` == 2014 | `Sample Year` %in% c(2016, 2018))  # need NA's from 2016 and 2018 as we don't have reads yet

Erb_14_16_18 %>% 
  count(`Sample Year`, `Otolith Mark Present`) %>% 
  spread(`Sample Year`, n, fill = 0)

#### Combine tibbles ####

extraction_eP008 <- bind_rows(Paddy_14_16_18) %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`) %>% 
  arrange(`Silly Code`, `Fish ID`)

write_csv(x = extraction_eP008, path = "../Extraction/eP008_Extraction_List_191210.csv")

save.image("../Extraction/eP008_Extraction_List_191210.RData")

extraction_eP008 %>% 
  count(`Silly Code`, `Tissue Type`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
extraction_eP009 <- bind_rows(Erb_14_16_18) %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`) %>% 
  arrange(`Silly Code`, `Fish ID`)

write_csv(x = extraction_eP009, path = "../Extraction/eP009_Extraction_List_191210.csv")

save.image("../Extraction/eP009_Extraction_List_191210.RData")

extraction_eP009 %>% 
  count(`Silly Code`, `Tissue Type`)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP010: Create extraction list for Hogan 2018 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Our next priority was Hogan 2018, and now that we have a decision on 2020 sampling
# (not doing it for Hogan, only Paddy + Erb), we can go ahead and extract all natural-origin
# Hogan 2018 fish since it is our final year!!!

#### Read in OceanAK data ####
require(tidyverse)
oceanak <- read_csv(file = "../OceanAK/Salmon Biological Data 2 - Kyle's Filter_Hogan_2018_20191224.csv")  # needed an updated pull from OceanAK, just Hogan 2018

## Modify for extraction list prep
oceanak <- oceanak %>% 
  replace_na(list(`Well Has More Than One Sample` = 0, `Is Missing Paired Data Exists` = 0)) %>% 
  mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, pad = "0", side = "left")) %>% 
  mutate(`DNA Tray Well Code` = str_pad(string = `DNA Tray Well Code`, width = 2, pad = "0", side = "left")) %>% 
  mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")) %>% 
  unite(col = "SillySource", c("Silly Code", "Fish ID"), sep = "_", remove = FALSE) %>% 
  filter(`Well Has More Than One Sample` != 1) %>% 
  filter(`Is Missing Paired Data Exists` != 1)
  # filter(Sex %in% c("M", "F"))  # doesn't have Sex in this data view (uggggggh)

## How many samples we got?
oceanak %>% 
  count(`Location Code`, `Sample Year`) %>% 
  spread(`Sample Year`, n, fill = 0)

## How about what we want to extract
oceanak %>% 
  filter(`Silly Code` == "PHOGAN18") %>%  # we have deep sixed this year
  count(`Silly Code`, `Otolith Mark Present`)

## Were all trays read?
oceanak %>% 
  count(`DNA Tray Code`, `Otolith Mark Present`) %>% 
  spread(`Otolith Mark Present`, n, fill = 0) %>% 
  mutate(n = (NO + YES + `<NA>`),
         p_na = `<NA>` / n) %>% 
  arrange(desc(p_na))
# Looks like a few trays don't have any reads, but after calling Crystal (CDV otolith lab manager), they have closed out Hogan 2018
# These two trays probably got lost/dropped

# Forge ahead with an extraction list!


#### Filter for Hogan 2018 Natural-only Samples ####
Hogan_18 <- oceanak %>% 
  filter(`Silly Code` == "PHOGAN18" & `Otolith Mark Present` == "NO")

Hogan_18 %>% 
  count(`Silly Code`, `Otolith Mark Present`)

#### Combine tibbles ####

extraction_eP010 <- bind_rows(Hogan_18) %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`) %>% 
  arrange(`Silly Code`, `Fish ID`)

write_csv(x = extraction_eP010, path = "../Extraction/eP010_Extraction_List_191224.csv")

save.image("../Extraction/eP010_Extraction_List_191224.RData")

extraction_eP010 %>% 
  count(`Silly Code`, `Tissue Type`)

#### Update with missing DNA Tray ####
# Crystal found data that was miscoded for a "missing" tray

require(tidyverse)
load("../Extraction/eP010_Extraction_List_191224.RData")

(lost_DWP <- read_csv("../OceanAK/Hogan_2018_missing_tray_CDV_29379_20191226.csv"))

# add the leading 0 to DNA tray well code
lost_DWP <- lost_DWP %>% 
  mutate(CELL_NUMBER = str_pad(string = CELL_NUMBER, width = 2, side = "left", pad = "0"))

# filter for this tray, need FISH_ID from LOKI and add to extraction list
extraction_eP010_update <- oceanak %>% 
  left_join(lost_DWP, by = c("DNA Tray Code" = "TRAY_ID", "DNA Tray Well Code" = "CELL_NUMBER")) %>% 
  filter(`DNA Tray Code` == "0000029379" & MARK_PRESENT == "NO") %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`) %>% 
  bind_rows(extraction_eP010) %>% 
  arrange(`Silly Code`, `Fish ID`)

write_csv(x = extraction_eP010_update, path = "../Extraction/eP010_Extraction_List_Update_191226.csv")

save.image("../Extraction/eP010_Extraction_List_Update_191226.RData")

extraction_eP010_update %>% 
  count(`Silly Code`, `Tissue Type`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP011: Create extraction list for Gimour 2015 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We already have created a list for Gilmour 2014-2018 (except for 2015)
# At the time, the concern was that there may be "excess" hatchery-origin parents for 2015
# But based on pHOS and our current extraction capacity, we should just move forward and extract all!

#### Read in OceanAK data ####
require(tidyverse)
og_names <- suppressMessages(names(read_csv(file = "../OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv", progress = FALSE)))
oceanak <- read_csv(file = "../OceanAK/AHRP Salmon Biological Data 20191119_1053.csv")
names(oceanak) <- og_names

## Modify for extraction list prep
oceanak <- oceanak %>% 
  replace_na(list(`Well Has More Than One Sample` = 0, `Is Missing Paired Data Exists` = 0)) %>% 
  mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, pad = "0", side = "left")) %>% 
  mutate(`DNA Tray Well Code` = str_pad(string = `DNA Tray Well Code`, width = 2, pad = "0", side = "left")) %>% 
  mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")) %>% 
  unite(col = "SillySource", c("Silly Code", "Fish ID"), sep = "_", remove = FALSE) %>% 
  filter(`Well Has More Than One Sample` != 1) %>% 
  filter(`Is Missing Paired Data Exists` != 1) %>% 
  filter(Sex %in% c("M", "F"))

## How many samples we got?
oceanak %>% 
  count(`Location Code`, `Sample Year`) %>% 
  spread(`Sample Year`, n, fill = 0)

## How about what we want to extract
oceanak %>% 
  filter(`Sample Year` == 2015 & `Location Code` == "Gilmour Creek") %>%  # we have deep sixed this year
  count(`Sample Year`, `Otolith Mark Present`, `Location Code`)

# Just Gilmour 2015, calculate raw pHOS and estimate sample size of Hatchery vs. Natural
oceanak %>% 
  filter(`Sample Year` == 2015) %>%  # we have deep sixed this year
  count(`Sample Year`, `Otolith Mark Present`, `Location Code`) %>%  # how many 
  spread(`Otolith Mark Present`, n, fill = 0) %>%  # yes/no/na
  mutate(pHOS = YES / (YES + NO),  # calculate pHOS
         nTotal = YES + NO + `<NA>`) %>%  # total sample size
  mutate(nHatchery = pHOS * nTotal,  # how many projected hatchery fish
         nNatural = (1 - pHOS) * nTotal) %>%  # and how many project natural fish
  filter(`Location Code` == "Gilmour Creek")

# Just go ahead and extract all 2015 Gilmour
# Also passing on 2019 until we get otolith reads

#### Filter for Gilmour 2015 Samples ####
Gilmour_15 <- oceanak %>% 
  filter(`Location Code` %in% c("Gilmour Creek") & `Sample Year` == 2015)  # need NA's from 2015 as we don't have reads yet

#### Combine tibbles ####

extraction_eP011 <- bind_rows(Gilmour_15) %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`) %>% 
  arrange(`Silly Code`, `Fish ID`)

write_csv(x = extraction_eP011, path = "../Extraction/eP011_Extraction_List_191224.csv")

save.image("../Extraction/eP011_Extraction_List_191224.RData")

extraction_eP011 %>% 
  count(`Silly Code`, `Tissue Type`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### eP012: Create extraction list for Gimour 2019 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We already have created a list for Gilmour 2014-2018
# At the time, we did not have otolith reads for 2019 (natural-only)

#### Read in OceanAK data ####
require(tidyverse)
og_names <- suppressMessages(names(read_csv(file = "../OceanAK/PedigreeData_AHRP - Salmon Biological Data 2_PWS_2013-2018_no_otoliths.csv", progress = FALSE)))
oceanak <- read_csv(file = "../OceanAK/Salmon Biological Data 2 - Kyle's Filter_Gilmour_2019_20200128.csv")
# names(oceanak) <- og_names
setdiff(names(oceanak), og_names); setdiff(og_names, names(oceanak))



## Modify for extraction list prep
oceanak <- oceanak %>% 
  replace_na(list(`Well Has More Than One Sample` = 0, `Is Missing Paired Data Exists` = 0)) %>% 
  mutate(`DNA Tray Code` = str_pad(string = `DNA Tray Code`, width = 10, pad = "0", side = "left")) %>% 
  mutate(`DNA Tray Well Code` = str_pad(string = `DNA Tray Well Code`, width = 2, pad = "0", side = "left")) %>% 
  mutate(Key = paste(`DNA Tray Code`, `DNA Tray Well Code`, sep = "_")) %>% 
  unite(col = "SillySource", c("Silly Code", "Fish ID"), sep = "_", remove = FALSE) %>% 
  filter(`Well Has More Than One Sample` != 1) %>% 
  filter(`Is Missing Paired Data Exists` != 1) %>% 
  filter(Sex %in% c("M", "F"))

## How many samples we got?
oceanak %>% 
  count(`Location Code`, `Sample Year`) %>% 
  spread(`Sample Year`, n, fill = 0)

## How about what we want to extract
oceanak %>% 
  filter(`Sample Year` == 2019 & `Location Code` == "Gilmour Creek") %>%  # we have deep sixed this year
  count(`Sample Year`, `Otolith Mark Present`, `Location Code`)

# Just go ahead and extract all 2019 Gilmour natural-origin

#### Filter for Gilmour 2019 Samples ####
Gilmour_19 <- oceanak %>% 
  filter(`Location Code` %in% c("Gilmour Creek") & `Sample Year` == 2019 & `Otolith Mark Present` == "NO")  # need NA's from 2019 as we don't have reads yet

#### Combine tibbles ####

extraction_eP012 <- bind_rows(Gilmour_19) %>% 
  select(`Silly Code`, `Fish ID`, `DNA Tray Code`, `DNA Tray Well Code`, `Tissue Type`) %>% 
  arrange(`Silly Code`, `Fish ID`)

write_csv(x = extraction_eP012, path = "../Extraction/eP012_Extraction_List_200128.csv")

save.image("../Extraction/eP012_Extraction_List_200128.RData")

extraction_eP012 %>% 
  count(`Silly Code`, `Tissue Type`)
