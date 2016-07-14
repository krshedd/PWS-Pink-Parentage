
setwd("V:/Analysis/5_Coastwide/Multispecies/Alaska Hatchery Research Program/PWS Pink")

# Read in as data.table (lightning fast!)
require(data.table)
collections.df <- fread(input = "OceanAK 13-7-2015 Salmon Biological Data All Stockdale and Hogan.txt")  # amazing
str(collections.df)

collections.df$Concatenate <- paste(collections.df$`DNA Tray Code`, collections.df$`DNA Tray Well Code`, sep = "_")

head(collections.df)
dimnames(collections.df)

collections.df[Year == 2013, ]
collections.df[collections.df$Year==2013, ]

collections.df[collections.df$`Silly Code` == "PHOGAN13" & collections.df$`Otolith Mark Present` == "YES", c("Silly Code", "DNA Tray Code", "DNA Tray Well Code", "DNA Tray Well Pos", "Otolith Mark Present", "Sex")]

collections.df[collections.df$`Silly Code` == "PHOGAN13" & collections.df$`Otolith Mark Present` == "YES", ]
collections.df[collections.df$`Silly Code` == "PHOGAN13" & collections.df$`Otolith Mark Present` == "YES", ]

# Convert to data.frame
collections.df <- data.frame(collections.df)
str(collections.df)

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
