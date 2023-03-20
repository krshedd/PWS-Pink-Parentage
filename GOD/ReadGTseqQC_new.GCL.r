#' This function reads in GTseq QC genotypes from .csv files as .gcl objects
#'
#' @param QCcsvFilepaths character vector with relative path for QC genotype .csv files
#'
#' @export
#' 
#' @return QC.gcl rubias-style tibble objects in global environment
#' 
#' @note This function requires a LocusControl object. 
#'
#' @example 
#' 
#' QCcsvFilepaths <- "V:/Lab/Genotyping/SNP Projects/Tests/Project T028 DNA Extractions from Pink Otoliths/Chips/T028_GT_seq_outputs/All data files/T028_LOKI_input_all.csv"
#' 
#' ProjectSillys <- "PSTOCK17"
#' 
#' LocusNames <- readr::read_csv(QCcsvFilepaths) %>% pull(LOCUS) %>% unique()
#' 
#' CreateLocusControl.GCL(markersuite = NULL, locusnames = LocusNames, username = "awbarclay", password = .password)
#' 
#' ReadGTseqQC.GCL(QCcsvFilepaths, ProjectSillys = ProjectSillys)
#' 
#' 
ReadGTseqQC.GCL <- function(QCcsvFilepaths, ProjectSillys) {
  
  if(!exists("LocusControl")){
    
   stop("'LocusControl' not yet built.")
    
    }
  
  while(!require(tidyverse)){install.packages("tidyverse")}
  
  loci <- LocusControl$locusnames
  
  # Read in .csv files
  QC_genotypes <- suppressMessages(
    
    suppressWarnings(
      
      dplyr::bind_rows(
        
        lapply(QCcsvFilepaths, function(fle) {
          
          readr::read_csv(file = fle, na = c("", "NA", "0", "0/0"))
          
          })  # GENOTYPE == "0" is NA
      )  # bind_rows
    )  # supressWarnings
  )  # suppressMessages
  
  # Rename columns, split genotype, unite silly_source
  QC_genotypes <- QC_genotypes %>% 
    dplyr::rename(FISH_ID = SAMPLE_NUM) %>% 
    dplyr::mutate(FISH_ID = as.character(FISH_ID)) %>% 
    tidyr::separate(GENOTYPE, into = c("ALLELE_1", "ALLELE_2"), sep = "/") %>% 
    tidyr::unite(SillySource, c(SILLY_CODE, FISH_ID), sep = "QC_", remove = FALSE) %>% 
    dplyr::select(SillySource, SILLY_CODE, FISH_ID, LOCUS, ALLELE_1, ALLELE_2)
  
  # Verify that all QC silly are in the project
  
  ProjectSillysQC <- unique(QC_genotypes$SILLY_CODE)
  
  if(!all(ProjectSillysQC %in% ProjectSillys)){
    
    stop(paste0(ProjectSillysQC[! ProjectSillysQC %in% ProjectSillys], " not found in ProjectSillys.")) 
  
    }
  
  # Verify that all QC loci are in project loci
  lociQC <- sort(unique(QC_genotypes$LOCUS))
  
  if(!all(lociQC %in% loci)){ 
  
  stop(paste0(lociQC[! lociQC %in% loci], " not found in LocusControl.")) 
  
  }
  
  haploci <- LocusControl$ploidy[lociQC][LocusControl$ploidy[lociQC] == 1]
  
  nloci <- length(lociQC)
  
  lapply(ProjectSillysQC, function(silly){
    
    message0 <- paste0(silly, "QC.gcl created ", match(silly, ProjectSillysQC)," of ", length(ProjectSillysQC), " completed.")
    
    sillydata <- QC_genotypes %>%
      dplyr::filter(SILLY_CODE==silly)
    
    ids <- sillydata$FISH_ID %>%
      unique() %>%
      sort() %>%
      as.character()
    
    sillyvials <- paste(silly, ids, sep = "_")
    
    nind <- length(sillyvials)
    
    silly_df_cols <- rep(NA_real_, nloci*2) %>%
      purrr::set_names(c(lociQC, paste0(lociQC, ".1")) %>% sort())
    
    silly_df0 <- sillydata %>%
      dplyr::arrange(LOCUS) %>%
      tidyr::pivot_longer(cols = c("ALLELE_1", "ALLELE_2"), values_to = "Allele") %>%
      dplyr::mutate(scores_header = case_when(name == "ALLELE_2" ~ paste0(LOCUS, ".1"),
                                              TRUE ~ LOCUS)) %>%
      dplyr::select(-LOCUS, -name) %>%
      tidyr::pivot_wider(names_from = scores_header, values_from = Allele, names_sep = "" ) %>%
      dplyr::mutate(
        CAPTURE_DATE = NA_character_,
        END_CAPTURE_DATE =  NA_character_
      )
    
    # Create tibble in rubias format. Adding in attribute variables so the objects match those built using LOKI2R.GCL
    silly_df <- tibble::add_column(silly_df0, !!!silly_df_cols[setdiff(names(silly_df_cols), names(silly_df0))]) %>%
      dplyr::mutate(COLLECTION_ID = NA_integer_,
                    SILLY_CODE = paste0(SILLY_CODE, "QC"),
                    PLATE_ID = NA_integer_,
                    PK_TISSUE_TYPE = NA_character_,
                    CAPTURE_LOCATION = NA_character_,
                    CAPTURE_DATE = NA,
                    END_CAPTURE_DATE = NA,
                    MESH_SIZE = NA_character_,
                    MESH_SIZE_COMMENT = NA_character_,
                    LATITUDE = NA_real_,
                    LONGITUDE = NA_real_,
                    AGENCY = NA_character_,
                    VIAL_BARCODE = NA_integer_,
                    DNA_TRAY_CODE = NA_character_,
                    DNA_TRAY_WELL_CODE = NA_integer_,
                    DNA_TRAY_WELL_POS = NA_character_,
                    CONTAINER_ARRAY_TYPE_ID = NA_integer_) %>% 
      dplyr::select(
        FK_FISH_ID = FISH_ID,
        COLLECTION_ID,
        SILLY_CODE,
        PLATE_ID,
        PK_TISSUE_TYPE,
        CAPTURE_LOCATION,
        CAPTURE_DATE,
        END_CAPTURE_DATE,
        MESH_SIZE,
        MESH_SIZE_COMMENT,
        LATITUDE,
        LONGITUDE,
        AGENCY,
        VIAL_BARCODE,
        DNA_TRAY_CODE,
        DNA_TRAY_WELL_CODE,
        DNA_TRAY_WELL_POS,
        CONTAINER_ARRAY_TYPE_ID,
        SillySource,
        tidyselect::all_of(names(silly_df_cols))
      ) %>%
      dplyr::arrange(FK_FISH_ID)
    
    #Make sure all haploid loci have NA's for allele 2
    if(length(haploci) > 0){
      
      silly_df[ paste0(hap_loci, ".1")] <- NA_character_
      
    }
    
    message(message0)
    
    assign(paste0(silly, "QC.gcl"), silly_df, pos = 1, .GlobalEnv)
    
  })

}
