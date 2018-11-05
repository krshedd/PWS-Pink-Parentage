genepop2franz.GCL=function(Genepop, OceanAK, Year, Stream, output_dir){
  ####################################################################################################################################################################################################################################################################
  # This function converts a genepop file into a FRANz pedigree format. 
  # It relies on the adegenet and tidyverse packages. The function filters out
  # individuals with more than 20% missing data and removes duplicate individuals, 
  # keeping those with the most genotypes. It pulls sex and otolith data from 
  # Oceanak and creates a unique 10-digit franz_id that's paired. 
  # 
  # Input parameters~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # genepop = 
  #   ~ The path to the genepop file, in .gen format.
  # oceanak = 
  #   ~ The path to the OceanAK file, in .csv format. 
  # Stream = 
  #   ~ The streamname for the population(s) you're interested in. passed as a vector list in caps.
  #   ~ Note that the input is taken from SILLY, so may not be full stream name (e.g., stockdate = STOCK)
  # Year = 
  #   ~ The year of the population as taken from the SILLY. This is for getting at parent vs offspring
  #     (i.e., odd vs even years). 
  # output = 
  #   ~ The path to the directory for the output files (note: they will be placed in a "Franz" folder).
  # 
  # Output~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # 1- A FRANz compatible .dat file.
  # 
  # 2- A .csv with FRANz-paired OceanAK data.
  #
  # Created by Chase Jalbert on 7/27/2018
  ####################################################################################################################################################################################################################################################################
  
  if (!require("pacman")) install.packages("pacman"); library(pacman) # install pacman, if not installed
  p_load(adegenet,tidyverse) # use pacman to load or install+load necessary packages
  
  genepop <- read.genepop(Genepop) #import genepop file as genind object using adegenet
  
  gind_df <- genind2df(genepop, pop = NULL, sep = "/", usepop = FALSE, oneColPerAll = FALSE) # convert genind to dataframedata using adegenet

 gind_df$silly <- str_extract(row.names(gind_df),"^([^_]*)")# get silly from rownames using regex
 gind_df$death <- as.numeric(str_extract( gind_df$silly, "([0-9]{2})")) + 2000 # get death year from silly
 gind_df$river <- str_extract(gind_df$silly, "(?<=[A-Z]{1})(.*)(?=[0-9]{2})") # get stream name from silly
 
 # Filter the dataframe based on input Stream name and Year (both taken from SILLY). First we're filtering for year
 # and Year + 2 to get parents and offspring. Then we filter based on list of stream names. 
 # Just FYI- good explanation of "%in%" vs "==" https://stackoverflow.com/questions/25647470/filter-multiple-conditions-dplyr
gind_df <- gind_df %>%
  rownames_to_column('names') %>% 
  filter(death %in% as.numeric(Year) | 
  death %in% (as.numeric(Year) + 2)) %>% 
  filter(river %in% Stream) %>% 
  column_to_rownames('names') %>% 
  select(-c(silly,death,river))
  
  # # Filter by all rows (individuals) missing more than 20%
  # filt_df <- gind_df[ -which( rowMeans( 
  #   is.na( gind_df)) > 0.2 ), ] 

  # we removed filtering steps because this will be done before running this function. But I rely on the "filt_df" so make a copy of the original df for now. 
  filt_df <- gind_df

  # replace NAs (0000) with ?/? as requird by FRANz
  filt_df[ is.na( filt_df )] <- "?/?"
  
  # add column to count the number of no calls per invididual
  filt_df$NOCALLS <- rowSums( filt_df == "?/?") 
  
  # Extract fish id by selecting everything between the first "_" and the end ($) in the id list
  FISHID = str_extract(row.names(filt_df), "(?<=_)(.*?)(?=$)")
  
  # Extract silly from the IDs by matchiung everyting that is not a "_", from the start of the string, until the "_"
  SILLY = str_extract(row.names(filt_df), "^([^_]*)") 
  
  # Extract species code by starting at the begining of the line ^ and selecting the first letter [A-Z]{1} of SILLY.
  SPECIES = str_extract(SILLY, "^[A-Z]{1}")
  
  # Extract year of death by selecting the group of two numbers ([0-9]{2}) in the SILLY add 2000 for 2000's. 
  DEATH_YR = as.numeric(str_extract( SILLY, "([0-9]{2})")) + 2000 
  
  # Calculate year of birth. Subtract 2 from death year, since pinks
  BIRTH_YR = DEATH_YR-2
  
  # Extract the stream name by selecting everyting (.*) between the first letter ?<=[A-Z]{1} and last two digits of the SILLY ?=[0-9]{2}.
  STREAM = str_extract(SILLY, "(?<=[A-Z]{1})(.*)(?=[0-9]{2})") 
  
  # Add column signaling how often the genotype is observed. This is for clonal organisms so our fish will always be "1".
  OBSERVED = rep("1", length(row.names(filt_df)))

  
  # combine the above values into a dataframe
  combined_df = bind_cols(list(SILLY, FISHID, STREAM, SPECIES, BIRTH_YR, DEATH_YR, OBSERVED)) 
  
  names(combined_df) = c("SILLY","FISHID","STREAM", "SPECIES", "BIRTH_YR", "DEATH_YR", "OBSERVED") # add names to dataframe 
  
  
  # Create unique FRANz ID - this MUST be 10-digits and can be padded with spaces, if necessary. Here, I combined as follows:
  combined_df$franz_id <- str_c(SPECIES, # 1st letter of species name
                                 substr(STREAM,1,1), # 1st letter of stream name
                                 substr(DEATH_YR,3,4), # last two digits of death year
                                 "_", # underscore
                                 str_pad(FISHID,5, pad = "0")  # 5 digit fishid using padded zeros up front
                                 )
  
  
  # Read in csv containing OceanAK data for collections
  oceanak <- read_csv(OceanAK) %>% 
    mutate("FISHID" = as.character(`Fish ID`))
  
  # Combine oceanak and genotype data
  combined_ocean_franz <- left_join( combined_df,
                              oceanak,
                              by = c(SILLY = "Silly Code",
                                     "FISHID")) %>%
    rename(SEX = "Sex")
  
  
  # create the first lime of the FRANz file by combining: total # populations, total number of loci, "/", projectname. 
  # projectname is automatically assigned from the Genepop input filename for record keeping
  franz_first_line = set_names(
    as.tibble(
      paste(
        NROW(as.data.frame( table(STREAM))), # get counts of the number of unique streams
        NCOL(gind_df), # count number of loci
        "/",
#  pull the filename of the genepop file by searching in the genepop object. Regex: search for any characters, to backslash,
# then in group 1 ([^.]+), one or more characters until a period and anything after the period [.].*, returning only the 
#group 1 [\\1] (Simply, get everything between slash and period...)
        gsub(".*[/]([^.]+)[.].*", "\\1", Genepop),
        sep = " ")
      ),
    "tmp")
  
  # Create dataframe with oceanak and filterd genotypes
  FRANZ <- bind_cols(combined_ocean_franz, 
                     filt_df) %>% 
    select(contains("Sample"),
           everything())
  
  # Filter by the number of individuals with the least missing data (i.e., minimum number of no calls). 
  # Note - I have no idea what happens if there is a tie for duplcates with missing data... perhaps it just picks one? 
  FRANZ <- FRANZ %>% 
    group_by( franz_id ) %>% # goup same ids together
    slice( which.min( NOCALLS )) %>% # only keep id with least number of no calls
    select(-NOCALLS ) # remove column from df
  
  # Create object containing all oceanak and franz data, for use with end output
  OCEANAK_FRANZ <- FRANZ
  
  # Order the franz columns, followed by genotypes [everything()]
  FRANZ <- FRANZ %>%
    select(franz_id,
           OBSERVED,
           BIRTH_YR,
           DEATH_YR,
           SEX, 
           everything(),
           # remove unnecessary columns (have to do this because used everything to add genotypes)
           -c( SILLY,
               FISHID,
               STREAM,
               SPECIES,
               contains('otolith'),
               contains('Code'),
               contains('Sample'),
               contains('ID', ignore.case = FALSE),
               starts_with("Length"),
               starts_with("Tissue"),
               starts_with("Is") 
           )
    )
  
  # Combine all columns into a single column, seperated by a space
  FRANZ <- unite(FRANZ,
                 tmp,
                 sep = " ")
  
  FRANZ$strms <- substr(FRANZ$tmp,1,2) # Create stream id using first two letters of franz_id (SPECIES&STREAM)
  
  # Remove headers and add first line to franz file, then loop through dataframe
  
  f <- unique(FRANZ$strms)
  
  file <- bind_rows( franz_first_line,
                     bind_rows(
                       lapply( f, function(s) {
                         j <- FRANZ %>%
                           filter(strms == s ) %>% 
                           select(-strms)
                         tmp = paste(nrow(j), s, sep = " ")
                         rbind(tmp, j)
                         }
                         )
                       )
                     )
  
  # create franz directory
  if ( !dir.exists(paste0(
    output_dir, 
    "/Franz"))) { 
  dir.create( paste0(
    output_dir, 
    "/Franz")
    )
  }
  
  # export in franz file format
  write.table(file, file = paste0(output_dir,"/Franz/",
                                  gsub(".*[/]([^.]+)[.].*", "\\1", Genepop),
                                  "_",
                                  paste(Year, collapse ="_"),
                                  "_",
                                  paste(Stream, collapse = "_"),
                                  ".dat"), 
              row.names=FALSE,
              col.names=FALSE, 
              sep = " ", 
              quote = FALSE
              )
  
  # export the object combining OceanAK data and franz_id, for pairing FRANz output back to original data.
  write_csv(OCEANAK_FRANZ, path = paste0(output_dir, "/Franz/",
                                  gsub(".*[/]([^.]+)[.].*", "\\1", 
                                       Genepop),
                                  "_OceanAK_paired_",
                                  paste(Year, collapse ="_"), 
                                  "_",
                                  paste(Stream, collapse = "_"),
                                       ".csv"),
            col_names = TRUE
            )
  
}