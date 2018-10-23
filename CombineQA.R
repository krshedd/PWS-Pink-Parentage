#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script is intended for quality assurance across all Hogan samples for
# the NPRB project. The goal is to: 
# 1. Create locus control with all lab project codes combined.
# 2. Calculate failure rate and conflict reports from QC.R.
# 3. Filter to include only Hogan samples (minus the one fish that failed QC).
# 4. Perform quality assurance measures from QC.R.
# 5. Print summary report. 

# The 2 desired outputs are:
# 1) QC publication table (discrepancy + error rate by year)
# 2) Error rate by locus (FRANz input parameter)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Setup ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

date()
# rm(list=ls(all=TRUE))

# This sources all of the new GCL functions to this workspace
# source("C:/Users/krshedd/R/Functions.GCL.R")

# Some of the required code expects "ProjectSillys", so I assigned both
sillyvec <- ProjectSillys <- c("PHOGAN13", "PHOGAN14", "PHOGAN15", "PHOGAN16") 

markersuite <- "Pink_PWS_304"

# I changed this up one level to keep the output out of the GitHub repository
dirQC <- "V:/Analysis/5_Coastwide/Multispecies/Alaska Hatchery Research Program/PWS Pink/"

species <- "pink"

project <- "NPRB Hogan Bay"

# We are going to pull genotypes by silly, not by project
# projectID <- c("2415", "2420", "2422", "2423")

# I made these "hidden" objects
.username <- "  "
.password <- "  "

# I changed the name to something more descriptive, feel free to change if you'd like
QCSummaryfile <- "Combined QC for NPRB Hogan Bay.xlsx"

# Load packages
while(!require(pacman)){ install.packages("pacman") }

p_load(tidyverse, lattice, writexl, abind)  # use pacman to load or install + load necessary packages

bbind <- function(...) { abind(..., along = 3) }

source(path.expand("~/R/Functions.GCL.R"))  # user may need to change depending on where you put this directory

setwd(dirQC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create Locus Control ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CreateLocusControl.GCL(markersuite = markersuite, username = .username, password = .password)

loci <- LocusControl$locusnames

nalleles <- LocusControl$nalleles

ploidy <- LocusControl$ploidy

alleles <- LocusControl$alleles

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in Project Genotypes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LOKI2R.GCL(sillyvec = sillyvec, username = .username, password = .password)

rm(.password)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Failure Rate ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

failure_rate <- FailureRate.GCL(sillyvec = sillyvec)

failure_rate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in QC Genotypes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Kyle was incorrect, we do need to read in QC genotypes because we need to
# determine the "error rate" which is the n_conflicts / n_qc_genotypes / 2

QCfiles <- c(
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P012 AHRP Parentage GTSeq Part 1/QC/Genotype Data Files/", pattern = ".csv", full.names = TRUE),
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P014 AHRP Parentage GTSeq Part 2/QC/Genotype Data Files/", pattern = ".csv", full.names = TRUE),
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P015 AHRP Parentage GTSeq Part 3/QC/Genotype Data Files/", pattern = ".csv", full.names = TRUE),
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P016 AHRP Parentage GTSeq Part 4/QC/Genotype Data Files/", pattern = ".csv", full.names = TRUE)
)

# Read in QC genotype files as one filtered tibble
QC_genotypes <- QCfiles %>%  # loop over each file
  purrr::map(function(x) readr::read_csv(file = x, col_types = cols(.default = "c"), na = c("", "NA", "0"))) %>%  # read in each file with default column type = "c" for character
  dplyr::bind_rows() %>%  # bind each file together into one tibble
  dplyr::filter(SILLY_CODE %in% sillyvec)  # filter for only sillys in sillyvec

# Number of non-zero QC genotypes per locus
n_geno_per_locus <- QC_genotypes %>% 
  dplyr::filter(!(SILLY_CODE == "PHOGAN15" & SAMPLE_NUM == "4424")) %>%  # remove PHOGAN15_4424, catastrophic conflict ind from P014
  dplyr::filter(!is.na(GENOTYPE)) %>%  # remove all "zero" or NA genotypes
  dplyr::count(LOCUS) %>%  # count genotypes per locus
  dplyr::rename(locus = LOCUS, n_qc = n)

# Number of non-zero QC genotypes per silly (year)
n_geno_per_silly <- QC_genotypes %>% 
  dplyr::filter(!(SILLY_CODE == "PHOGAN15" & SAMPLE_NUM == "4424")) %>%  # remove PHOGAN15_4424, catastrophic conflict ind from P014
  dplyr::filter(!is.na(GENOTYPE)) %>%  # remove all "zero" or NA genotypes
  dplyr::count(SILLY_CODE) %>%  # count genotypes per locus
  dplyr::rename(silly = SILLY_CODE, n_qc = n)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in Conflict Report ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Need to filter again for Hogan. Pull from .GCL script
# Kyle was mistaken, as part of the "CombineConflictsWithPlateID.GCL" function,
# it writes out the output as a "CombinedConflictsWithPlateID.csv"
# So we just need to read in that output .csv from each project, bind them,
# and filter for just Hogan fish

# List the output.csv for each lab Project, P012-P016
QCConcordanceReportfile <- c(
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P012 AHRP Parentage GTSeq Part 1/QC/Conflict Reports/", pattern = "CombinedConflictsWithPlateID.csv", full.names = TRUE),
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P014 AHRP Parentage GTSeq Part 2/QC/Conflict Reports/", pattern = "CombinedConflictsWithPlateID.csv", full.names = TRUE),
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P015 AHRP Parentage GTSeq Part 3/QC/Conflict Reports/", pattern = "CombinedConflictsWithPlateID.csv", full.names = TRUE),
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P016 AHRP Parentage GTSeq Part 4/QC/Conflict Reports/", pattern = "CombinedConflictsWithPlateID.csv", full.names = TRUE)
)

# Read in concordance files as one filtered tibble
concordance <- QCConcordanceReportfile %>%  # loop over each file
  purrr::map(function(x) readr::read_csv(file = x, col_types = cols(.default = "c"))) %>%  # read in each file with default column type = "c" for character
  dplyr::bind_rows() %>%  # bind each file together into one tibble
  dplyr::filter(silly %in% sillyvec) %>%  # filter for only sillys in sillyvec
  dplyr::filter(silly_source != "PHOGAN15_4424")  # remove PHOGAN15_4424, catastrophic conflict ind from P014

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Conflict summaries

conflicts_by_plate <- concordance %>% 
  dplyr::group_by(plate_id, concordance_type) %>%
  dplyr::summarise(n = n()) %>% 
  tidyr::spread(concordance_type, n, fill = 0, drop = FALSE) %>% 
  dplyr::mutate(Conflict = sum(`Het-Het`, `Het-Homo`, `Homo-Het`, `Homo-Homo`)) %>% 
  dplyr::ungroup()

conflicts_by_silly <- concordance %>% 
  dplyr::group_by(silly, concordance_type) %>% 
  dplyr::summarise(n = n()) %>% 
  tidyr::spread(concordance_type, n, fill = 0, drop = FALSE) %>% 
  dplyr::mutate(Conflict = sum(`Het-Het`, `Het-Homo`, `Homo-Het`, `Homo-Homo`)) %>% 
  dplyr::ungroup()

conflicts_by_locus <- concordance %>% 
  dplyr::group_by(locus, concordance_type) %>% 
  dplyr::summarise(n = n()) %>% 
  tidyr::spread(concordance_type, n, fill = 0, drop = FALSE) %>% 
  dplyr::mutate(Conflict = sum(`Het-Het`, `Het-Homo`, `Homo-Het`, `Homo-Homo`)) %>% 
  dplyr::ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Locus-specific error rate for FRANz
conflicts_by_locus_FRANz <- conflicts_by_locus %>%
  dplyr::left_join(n_geno_per_locus, by = "locus") %>%  # join with number of qc genotypes per locus
  dplyr::mutate(conflict_rate = Conflict / n_qc) %>%  # conflict rate is number of conflicts / number of qc genotypes
  dplyr::mutate(error_rate = conflict_rate / 2) %>%  # error rate is conflict rate / 2, because conflict could be due to error in "project fish" or "qc fish"
  dplyr::select(locus, error_rate)  # drop unnecessary variables

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Silly-specific error rate for publication table
conflicts_by_silly_pub <- conflicts_by_silly %>% 
  dplyr::left_join(n_geno_per_silly, by = "silly") %>%  # join with number of qc genotypes per silly
  dplyr::mutate(conflict_rate = Conflict / n_qc) %>%  # conflict rate is number of conflicts / number of qc genotypes
  dplyr::mutate(error_rate = conflict_rate / 2) %>%  # error rate is conflict rate / 2, because conflict could be due to error in "project fish" or "qc fish"
  dplyr::select(silly, error_rate)  # drop unnecessary variables

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in Full Conflict Report ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Need to filter again for Hogan. Pull from .GCL script
# Kyle was mistaken, as part of the "CombineConflictsWithPlateID.GCL" function,
# it writes out the output as a "CombinedConflictsWithPlateID.csv"
# So we just need to read in that output .csv from each project, bind them,
# and filter for just Hogan fish

# List the output.csv for each lab Project, P012-P016 from LOKI (conflicts + agreements)
QC_concordance_files_all <- c(
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P012 AHRP Parentage GTSeq Part 1/QC/Conflict Reports/Concordance Tables Loki/", pattern = "_csv", full.names = TRUE),
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P014 AHRP Parentage GTSeq Part 2/QC/Conflict Reports/Concordance Tables Loki/", pattern = "_csv", full.names = TRUE),
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P015 AHRP Parentage GTSeq Part 3/QC/Conflict Reports/Concordance Tables Loki/", pattern = "_csv", full.names = TRUE),
  list.files(path = "V:/Lab/Genotyping/SNP Projects/Pink/Project P016 AHRP Parentage GTSeq Part 4/QC/Conflict Reports/Concordance Tables Loki/", pattern = "_csv", full.names = TRUE)
)

# Read in concordance files as one filtered tibble
concordance_all <- QC_concordance_files_all %>%  # loop over each file
  purrr::map(function(x) readr::read_csv(file = x, col_types = cols(.default = "c"), na = c("", "NA", "0"))) %>%  # read in each file with default column type = "c" for character
  dplyr::bind_rows() %>%  # bind each file together into one tibble
  dplyr::filter(`Silly Code` %in% sillyvec) %>%  # filter for only sillys in sillyvec
  tidyr::unite(silly_source, c("Silly Code", "Sample Number"), sep = "_", remove = FALSE) %>%  # create silly_source
  dplyr::filter(silly_source != "PHOGAN15_4424")  # remove PHOGAN15_4424, catastrophic conflict ind from P014

# Number of non-zero QC genotypes per silly (year)
concordance_all %>% 
  dplyr::filter(!is.na(`File: Allele 1`) & !is.na(`Database: Allele 1`)) %>% 
  dplyr::count(`Silly Code`)

n_geno_per_silly

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### QA of Project Genotypes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We do not necessarily need this here, but this is what you will recycle for
# your exploratory analysis R notebook

ProjectSillys_SampleSizes <- matrix(data = NA, nrow = length(ProjectSillys), ncol = 5, dimnames = list(ProjectSillys, c("Genotyped", "Alternate", "Missing", "Duplicate", "Final")))

ProjectSillys_SampleSizes[, "Genotyped"] <- sapply(paste(ProjectSillys, ".gcl", sep = ''), function(x) get(x)$n)

if(species %in% c("chum", "sockeye")) {
  
  Alternate <- FindAlternateSpecies.GCL(sillyvec = ProjectSillys, species = species) %>% 
    dplyr::as_tibble()
  
  nAltBySilly <- sapply(ProjectSillys, function(silly) {
    AlternateSpeciesReport <- Alternate[grep(pattern = silly, x = rownames(Alternate)), ]
    sum(AlternateSpeciesReport$Alternate > 0.5 & AlternateSpeciesReport$Failure > 0.5)
  })
  # RemoveAlternateSpecies.GCL(AlternateSpeciesReport = Alternate, AlternateCutOff = 0.5, FailedCutOff = 0.5)  # Do not remove fish, just note how many per silly. Still want to catch them in conflicts later.
  
} else {
  
  Alternate = tibble::tibble(x = "Not applicable")
  
}

ColSizePostAlternate <- ProjectSillys_SampleSizes[, "Genotyped"]
if(exists(x = "nAltBySilly")) {ColSizePostAlternate <- ColSizePostAlternate - nAltBySilly}
# ColSizePostAlternate <- sapply(paste(ProjectSillys, ".gcl", sep = ''), function(x) get(x)$n)

ProjectSillys_SampleSizes[, "Alternate"] <- ProjectSillys_SampleSizes[, "Genotyped"] - ColSizePostAlternate 

MissLoci <- RemoveIndMissLoci.GCL(sillyvec = ProjectSillys, proportion = 0.8)

ColSizePostMissLoci <- sapply(paste(ProjectSillys, ".gcl", sep = ''), function(x) get(x)$n) - ProjectSillys_SampleSizes[, "Alternate"]

ProjectSillys_SampleSizes[, "Missing"] <-  ColSizePostAlternate - ColSizePostMissLoci

DuplicateCheck95MinProportion <- CheckDupWithinSilly.GCL(sillyvec = ProjectSillys, loci = loci, quantile = NULL, minproportion = 0.95)

DuplicateCheckReportSummary <- sapply(ProjectSillys, function(x) DuplicateCheck95MinProportion[[x]]$report, simplify = FALSE)

nDupsBySilly <- sapply(DuplicateCheckReportSummary, function(silly) {ifelse(is.character(silly), 0, nrow(as.matrix(silly)))})
# RemovedDups <- RemoveDups.GCL(DuplicateCheck95MinProportion)  # Do not remove fish, just note how many per silly. Still want to catch them in conflicts later.

sapply(DuplicateCheckReportSummary[nDupsBySilly >=1], function(silly) {if(1 %in% abs(as.numeric(levels(silly$ID1)) - as.numeric(levels(silly$ID2)))) {"Sequential IDs found as duplicates, check 'DuplicateCheckReportSummary' for duplicated rows"} else {"Duplicates exist, but IDs do not appear sequential"} } )

DuplicateCheckReportSummary[nDupsBySilly >= 1]  # Show within silly duplicates

ColSizePostDuplicate <- ColSizePostMissLoci - nDupsBySilly
# ColSizePostDuplicate <- sapply(paste(ProjectSillys, ".gcl", sep = ''), function(x) get(x)$n)

ProjectSillys_SampleSizes[, "Duplicate"] <- ColSizePostMissLoci - ColSizePostDuplicate

ProjectSillys_SampleSizes[, "Final"] <- ColSizePostDuplicate

ProjectSillys_SampleSizes

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create Summary Tables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We do not need all of this


summary_table_1 <- dplyr::bind_cols(tibble(Silly = ProjectSillys), as.tibble(ProjectSillys_SampleSizes)) %>% 
  dplyr::left_join(failure_rate$silly_failure_rate, by = c("Silly" = "silly")) %>% 
  dplyr::rename("Failure Rate" = fail) %>% 
  dplyr::mutate("Total QC Fish" = QCColSizeAll)

qc_silly_genotypes <- tibble::tibble(silly = factor(ProjectSillys),
                                     qc_genotypes = sapply(ProjectSillys, function(silly) {
                                       qc_silly = paste0(silly, "QC.gcl")
                                       ifelse(qc_silly %in% names(QCColSizeAll), QCColSizeAll[qc_silly] * length(loci), 0)
                                     } ))

summary_table_2 <- conflicts_by_silly %>% 
  tidyr::gather(type, number, -silly) %>%  # make tall
  dplyr::left_join(qc_silly_genotypes, by = "silly") %>%  # join number of QC genotypes by silly
  dplyr::mutate(rate = number / qc_genotypes) %>%  # conflict numbers to rates
  tidyr::gather(variable, value, -silly, -qc_genotypes, -type) %>%  # make tall
  tidyr::unite(temp, type, variable) %>%  # unite conflict type with both number and rate
  tidyr::spread(temp, value) %>%  # make wide
  dplyr::rename(Silly = silly, 
                "Total QC Genotypes" = qc_genotypes, 
                "Total Discrepancies" = Conflict_number, 
                "Discrepancy Rate" = Conflict_rate,
                "DB Zeros" = `DB Zero_number`,
                "DB Zero Rate" = `DB Zero_rate`,
                "QC Zeros" = `File Zero_number`,
                "QC Zero Rate" = `File Zero_rate`,
                "Total Het-Het" = `Het-Het_number`,
                "Het-Het Rate" = `Het-Het_rate`,
                "Total Het-Homo" = `Het-Homo_number`,
                "Het-Homo Rate" = `Het-Homo_rate`,
                "Total Homo-Het" = `Homo-Het_number`,
                "Homo-Het Rate" = `Homo-Het_rate`,
                "Total Homo-Homo" = `Homo-Homo_number`,
                "Homo-Homo Rate" = `Homo-Homo_rate`)

summary_table_3 <- conflicts_by_locus %>% 
  tidyr::gather(type, number, -locus) %>%  # make tall
  dplyr::mutate(qc_genotypes = sum(QCColSizeAll)) %>%  # join number of QC genotypes by locus
  dplyr::mutate(rate = number / qc_genotypes) %>%  # conflict numbers to rates
  tidyr::gather(variable, value, -locus, -qc_genotypes, -type) %>%  # make tall
  tidyr::unite(temp, type, variable) %>%  # unite conflict type with both number and rate
  tidyr::spread(temp, value) %>%  # make wide
  dplyr::rename(Locus = locus,
                "Total QC Genotypes" = qc_genotypes, 
                "Total Discrepancies" = Conflict_number, 
                "Discrepancy Rate" = Conflict_rate,
                "DB Zeros" = `DB Zero_number`,
                "DB Zero Rate" = `DB Zero_rate`,
                "QC Zeros" = `File Zero_number`,
                "QC Zero Rate" = `File Zero_rate`,
                "Total Het-Het" = `Het-Het_number`,
                "Het-Het Rate" = `Het-Het_rate`,
                "Total Het-Homo" = `Het-Homo_number`,
                "Het-Homo Rate" = `Het-Homo_rate`,
                "Total Homo-Het" = `Homo-Het_number`,
                "Homo-Het Rate" = `Homo-Het_rate`,
                "Total Homo-Homo" = `Homo-Homo_number`,
                "Homo-Homo Rate" = `Homo-Homo_rate`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Append Summary Tables to QCSummaryfile.xlsx ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create list object for all output to simple summary.xlsx file
summary_lst <- suppressWarnings(list("Summary by Silly" = summary_table_1,
                                     "Conflicts by Silly" = summary_table_2,
                                     "Conflicts by Locus" = summary_table_3,
                                     "Conflicts by PlateID" = conflicts_by_plate,
                                     "QC Duplicate Check" = dup_check_results,
                                     "Failure Rate by Silly" = failure_rate$silly_failure_rate,
                                     "Failure Rate by Locus" = failure_rate$locus_failure_rate,
                                     "Failure Rate by Plate" = failure_rate$plate_failure_rate,
                                     "Overall Failure Rate" = failure_rate$overall_failure_rate,
                                     "Project Sample Size by Locus" = OriginalProjectSampleSizebyLocus %>% tibble::rownames_to_column("silly") %>% tibble::as_tibble(),
                                     "Duplicate Check in Project" = dplyr::bind_rows(DuplicateCheckReportSummary[!DuplicateCheckReportSummary == "NO Duplicates"], .id = "silly") %>% tibble::as_tibble(),
                                     "Alternate Species" = Alternate))

# Write out a "Simple" file, can't update normal Summary File by inserting new tabs
if(file.exists(QCSummaryfile)) {
  stop(paste0("QC Summary file: '", QCSummaryfile ,"' already exists, change the file name so you don't overwrite it, hoser!!!"))
} else {
  writexl::write_xlsx(x = summary_lst, path = QCSummaryfile, col_names = TRUE)
}