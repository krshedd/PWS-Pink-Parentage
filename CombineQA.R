
######################################################################################################################################################################################
#
#  This script is intended for quality assurance across all Hogan samples. The goal is to: 
# 1. Create locus control with all project codes combined.
# 2. Calculate failure rate and conflict reports from QC.R.
# 3. Filter to include only Hogan samples (minus the one fish that failed QC).
# 4. Perform quality assurance measures from QC.R.
# 5. Print summary report. 
#########

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Setup ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

date()
# rm(list=ls(all=TRUE))

# This sources all of the new GCL functions to this workspace
# source("C:/Users/krshedd/R/Functions.GCL.R")

#Arguments
sillyvec <- c("PHOGAN13", "PHOGAN14", "PHOGAN15", "PHOGAN16") 

markersuite="Pink_PWS_304"

dirQC <- "V:/Analysis/5_Coastwide/Multispecies/Alaska Hatchery Research Program/PWS Pink/GitHub-PWS-Pink-Parentage/"

species <- "pink"

project <- c("P12", "P014", "P15", "P16")

projectID <- c("2415", "2420", "2422", "2423")

username <- "ealescak"

.password <- "1234"

QCSummaryfile <- paste("Project", project,"QC Summary R Script.xlsx") #  Do name normal summary file!!! If you do, it will overwrite it, not append it

conflict_rate <- 0.10  # conflict rate at which dupcheck between sillys occurs


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create Locus Control ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CreateLocusControl.GCL(markersuite = markersuite, username = username, password = .password)

LOKI2R.GCL(sillyvec,username,password)

loci <- LocusControl$locusnames

nalleles <- LocusControl$nalleles

ploidy <- LocusControl$ploidy

alleles <- LocusControl$alleles


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in Project Genotypes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ReadProjectLOKI2R.GCL(projectID = projectID, username = username, password = .password)

rm(.password)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Failure Rate ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

failure_rate <- FailureRate.GCL(sillyvec = ProjectSillys)

failure_rate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in QC Genotypes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

QCfiles <- list.files(path = "Genotype Data Files", pattern = ".csv", full.names = TRUE, recursive = FALSE)

if(max(nalleles) <= 2) {
  # SNP
  ReadBiomarkQC.GCL(QCcsvFilepaths = QCfiles)
} else {
  if(max(nalleles) <= 4) {
    # GTseq
    ReadGTseqQC.GCL(QCcsvFilepaths = QCfiles)
  } else {
    # uSat
    ReadUSatQC.GCL(QCcsvFilepaths = QCfiles)
  } # else for usat
} # else for usat or GTseq

QCColSize <- sapply(paste(QCSillys, ".gcl", sep = ''), function(x) get(x)$n)

QCColSizeAll <- setNames(rep(0, length(ProjectSillys)),paste0(ProjectSillys, "QC.gcl"))

QCColSizeAll[paste0(QCSillys, ".gcl")] <- QCColSize[paste0(QCSillys, ".gcl")]

QCColSizeAll

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in Conflict Report ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

QCConcordanceReportfile <- list.files (path = "Conflict Reports", pattern = "ConcordanceReport", full.names = TRUE)

CombineConflictsWithPlateID.GCL(files = QCConcordanceReportfile)

# Old conflict report has "0" for mitochondrial conflicts, new has " " for mitochondrial conflicts, we will refer to them as "Homo-Homo".

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Conflict summaries

conflicts_by_plate <- combined_conflicts %>% 
  dplyr::group_by(plate_id, concordance_type) %>%
  dplyr::summarise(n = n()) %>% 
  tidyr::spread(concordance_type, n, fill = 0, drop = FALSE) %>% 
  dplyr::mutate(Conflict = sum(`Het-Het`, `Het-Homo`, `Homo-Het`, `Homo-Homo`)) %>% 
  dplyr::ungroup()

conflicts_by_silly <- combined_conflicts %>% 
  dplyr::group_by(silly, concordance_type) %>% 
  dplyr::summarise(n = n()) %>% 
  tidyr::spread(concordance_type, n, fill = 0, drop = FALSE) %>% 
  dplyr::mutate(Conflict = sum(`Het-Het`, `Het-Homo`, `Homo-Het`, `Homo-Homo`)) %>% 
  dplyr::ungroup()

conflicts_by_locus <- combined_conflicts %>% 
  dplyr::group_by(locus, concordance_type) %>% 
  dplyr::summarise(n = n()) %>% 
  tidyr::spread(concordance_type, n, fill = 0, drop = FALSE) %>% 
  dplyr::mutate(Conflict = sum(`Het-Het`, `Het-Homo`, `Homo-Het`, `Homo-Homo`)) %>% 
  dplyr::ungroup()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Sample Size by Locus for Project Genotypes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OriginalProjectSampleSizebyLocus <- SampSizeByLocus.GCL(sillyvec = ProjectSillys, loci = loci)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Sample Size by Locus for QC Genotypes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OriginalQCSampleSizebyLocus <- SampSizeByLocus.GCL(sillyvec = QCSillys, loci = loci)

OriginalQCPercentbyLocus <- apply(OriginalQCSampleSizebyLocus, 1, function(row) {row / max(row)} )

rerunsQC <- which(apply(OriginalQCPercentbyLocus, 2, min) < 0.8)

new_colors <- colorRampPalette(c("black", "white"))

levelplot(t(OriginalQCPercentbyLocus), col.regions = new_colors, at = seq(0, 1, length.out = 100), main = "% Genotyped", xlab = "SILLY", ylab = "Locus", scales = list(x = list(rot = 90)), aspect = "fill") # aspect = "iso" will make squares

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### QA of Project Genotypes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
#### QA of QC Genotypes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MissLociQC <- RemoveIndMissLoci.GCL(sillyvec = QCSillys, proportion = 0.8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create Summary Tables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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