#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script is intended for quality assurance across all Hogan samples for
# the NPRB project. The 2 desired outputs are:
# 1) QC publication table (discrepancy + error rate by year)
# 2) Error rate by locus (FRANz input parameter)
#
# Output will be saved as separate .csv files in the QC folder of this project
#
# To do this, we will read in the FULL concordance reports (conflicts + 
# agreements) to make sure that our error rate only uses instances where we
# called genotypes in the denominator.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all.names = TRUE))  # clear workspace

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in Full Conflict Report ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)

sillyvec <- c("PHOGAN13", "PHOGAN14", "PHOGAN15", "PHOGAN16")  # collections to read in

# List the output.csv for each lab Project, P012-P016 from LOKI (conflicts + agreements)
# Kyle had to open the "original" files and save as .csv files
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
  dplyr::rename(silly = `Silly Code`, 
                fish_id = `Sample Number`,
                locus = Locus,
                file_allele_1 = `File: Allele 1`,
                file_allele_2 = `File: Allele 2`,
                db_allele_1 = `Database: Allele 1`,
                db_allele_2 = `Database: Allele 2`,
                concordance = Concordance,
                concordance_type = `Concordance Type`) %>%  # rename for convenience
  dplyr::select(silly, fish_id, locus, file_allele_1, file_allele_2, db_allele_1, db_allele_2, concordance, concordance_type) %>%  # only keep fields we need
  dplyr::filter(silly %in% sillyvec) %>%  # filter for only sillys in sillyvec
  tidyr::unite(silly_source, c(silly, fish_id), sep = "_", remove = FALSE) %>%  # create silly_source
  dplyr::filter(silly_source != "PHOGAN15_4424")  # remove PHOGAN15_4424, catastrophic conflict indiv from P014

# Table to make sure no NA values
concordance_all %>% 
  dplyr::count(concordance, concordance_type) %>%
  tidyr::spread(concordance, n, fill = 0, drop = FALSE)

# Investigate NA values
concordance_all %>% 
  filter(concordance == "Conflict" & is.na(concordance_type))

# Fill NA values as Het-Het
concordance_all <- concordance_all %>% 
  tidyr::replace_na(list(concordance_type = "Het-Het"))

# Table to make sure no NA values
concordance_all %>% 
  dplyr::count(concordance, concordance_type) %>% 
  tidyr::spread(concordance, n, fill = 0, drop = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Genotyping Error by Silly ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Number of non-zero QC genotypes per silly (year)
n_geno_per_silly <- concordance_all %>% 
  dplyr::filter(!is.na(file_allele_1) & !is.na(db_allele_1)) %>%  # only consider cases where we called genotypes for qc (file) and project (database) fish
  dplyr::count(silly) %>%  # count instances by silly
  dplyr::rename(n_qc = n)  # rename for convenience

# Conflicts by silly
conflict_rate_by_silly <- concordance_all %>% 
  dplyr::mutate(concordance_type = dplyr::recode(concordance_type, "Het-Homo" = "Homo-Het")) %>%  # recode "Het-Homo" as "Homo-Het", the directionality does not matter for publication tables
  dplyr::filter(concordance_type %in% c("Het-Het", "Homo-Het", "Homo-Homo")) %>%  # filter our file zero and database zero
  dplyr::group_by(silly, concordance_type) %>%  # group
  dplyr::summarise(n = n()) %>%  # count instances by silly and type
  dplyr::left_join(n_geno_per_silly, by = "silly") %>%  # join with number of qc genotypes per silly
  dplyr::mutate(conflict_rate = n / n_qc) %>%  # conflict rate is number of conflicts / number of qc genotypes
  dplyr::select(silly, n_qc, concordance_type, conflict_rate) %>%  # only keep fields we need
  tidyr::spread(concordance_type, conflict_rate, fill = 0, drop = TRUE) %>%  # go from "tall" to "wide" format
  dplyr::mutate(Overall = sum(`Het-Het`, `Homo-Het`, `Homo-Homo`)) %>%  # sum all conflicts to get overall rate
  dplyr::mutate(Error_rate = Overall / 2) %>%  # error rate = conflict / 2
  dplyr::rename(Silly = silly, Genotypes_compared = n_qc) %>%  # rename for publication
  dplyr::ungroup() # always ungroup when done

# Write out conflict (discrepancy) rate table
write_csv(x = conflict_rate_by_silly,
          path = "../QC/discrepancy_rate_by_silly.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Genotyping Error by Locus ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of non-zero QC genotypes per locus
n_geno_per_locus <- concordance_all %>% 
  dplyr::filter(!is.na(file_allele_1) & !is.na(db_allele_1)) %>%  # only consider cases where we called genotypes for qc (file) and project (database) fish
  dplyr::count(locus) %>%  # count instances by locus
  dplyr::rename(n_qc = n)  # rename for convenience

conflicts_by_locus <- concordance_all %>% 
  dplyr::group_by(locus, concordance_type) %>%  # group 
  dplyr::summarise(n = n()) %>%  # count instances by locus and type
  tidyr::spread(concordance_type, n, fill = 0, drop = FALSE) %>%  #  go from "tall" to "wide" format
  dplyr::mutate(conflict = sum(`Het-Het`, `Het-Homo`, `Homo-Het`, `Homo-Homo`)) %>%  # sum all conflicts to get overall rate
  dplyr::ungroup()  # always ungroup when done

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Locus-specific error rate for FRANz
error_rate_by_locus_FRANz <- conflicts_by_locus %>%
  dplyr::left_join(n_geno_per_locus, by = "locus") %>%  # join with number of qc genotypes per locus
  dplyr::mutate(conflict_rate = conflict / n_qc) %>%  # conflict rate is number of conflicts / number of qc genotypes
  dplyr::mutate(error_rate = conflict_rate / 2) %>%  # error rate is conflict rate / 2, because conflict could be due to error in "project fish" or "qc fish"
  dplyr::select(locus, error_rate)  # drop unnecessary variables

# Write out locus-specific error rate (conflict rate / 2) table
write_csv(x = error_rate_by_locus_FRANz,
          path = "../QC/error_rate_by_locus.csv")

# Histogram of locus-specific error rates
conflicts_by_locus_FRANz %>% 
  ggplot(aes(x = error_rate)) + 
  geom_histogram() +
  ggtitle("Histogram of locus-specific error rates")