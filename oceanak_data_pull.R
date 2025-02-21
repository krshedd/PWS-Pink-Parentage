library(tidyverse)
library(lubridate)

rm(list = ls())

# Begin user input ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.username = readLines("~/usr_pw.txt", n = 1)  # LOKI username

.password = readLines("~/usr_pw.txt" , n = 2)[[2]]  # LOKI password

# Currently set up to read all pink data from all years
species = "P"  # P = pink, C = chum

streams <- c("ERB", "HOGAN", "GILMOUR", "PADDY", "STOCK")  # these are the back half of the silly codes

yrs <- 13:20  # 2 digit years

sillyvec <- paste0(species, rep(streams, each = length(yrs)), yrs)  # put it all together to get all possible silly codes

not_tissues = "Otolith"  # OceanAK has 1 row of data per tissue, including both otolith and genetic tissues, we do not want the otolith stuff (this is different than otolith reads)

# End user input ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_time <- proc.time()

drvpath <- system.file("java", "ojdbc8.jar", package = "GCLr")  # get from GCLr package

drv <- RJDBC::JDBC("oracle.jdbc.OracleDriver", classPath = drvpath, " ")

source("~/GitHub_repos/GCLr/R/loki_url.r")  # new with GCLr package, not exported with package, change path as necessary

url <- loki_url() # get from local GitHub clone of GCLr package

con <- RJDBC::dbConnect(drv,
                        url = url,
                        user = .username,
                        password = .password)

data_qry <-
  paste(
    "SELECT * FROM AKFINADM.V_SALMON_BIO_FACT_GEN_TISS WHERE SILLY_CODE IN (",
    paste0("'", sillyvec, "'", collapse = ","),
    ") AND TISSUE_TYPE NOT IN (",
    paste0("'", not_tissues, "'", collapse = ","),
    ")",
    sep = ""
  )

dataAll0 <- RJDBC::dbGetQuery(con, data_qry)   

discon <- RJDBC::dbDisconnect(con)  # disconnect from OceanAK

proc.time() - start_time  # how long did it take?

glimpse(dataAll0)   # what does out data look like?

dataAll0 %>% 
  count(SILLY_CODE, TISSUE_TYPE)  # sample size by silly code and tissue type, should only be hearts and occasionally other random tissues


# Write out the data to "V:\Analysis\5_Coastwide\Multispecies\Alaska Hatchery Research Program\PWS Pink\OceanAK" with timestamp
readr::write_csv(
  x = dataAll0,
  file = paste0(
    "../OceanAK/AHRP Salmon Biological Data ",
    Sys.time() %>% stringr::str_remove_all(pattern = "-") %>% stringr::str_remove_all(pattern = ":") %>% stringr::str_replace(pattern = " ", replacement = "_"),
    ".csv"
  )
)

# for spot checking the data warehouse SALMON_BIO_FACT
# data_qry_SBF <- "SELECT * FROM DWASL.SALMON_BIO_FACT@DWPROD WHERE DNA_TRAY_CODE IN ('0000028170')"  # this PERB18 DWP barcode did not exist as of 2025-02-20
# data_qry_SBF_HWI <- "SELECT * FROM DWASL.SALMON_BIO_FACT@DWPROD WHERE BATCH_NUMBER IN ('HWI-PEDIGREE')"  # try getting all?

# end