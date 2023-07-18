# Quick Fst among pedigree streams (and AFK strays)
# For reviewer comment in Sam May's Fine-scale Homing Manuscript
# Kyle Shedd
# 2023-07-18
source("~//R/Functions.GCL.R")  # develop branch!!!
load_objects(path = "../Objects/All_Streams_14_16_develop")
load_sillys(path = "../Genotypes/All_Streams_14_16_develop/OceanAK_Origin_PostQA/")

# subsample down to 95 per collection
PERB14_natural_sample.gcl <- PERB14.gcl %>% 
  dplyr::filter(`Otolith Mark Present` == "NO") %>% 
  dplyr::slice_sample(n = 95)

PHOGAN14_natural_sample.gcl <- PHOGAN14.gcl %>% 
  dplyr::filter(`Otolith Mark Present` == "NO") %>% 
  dplyr::slice_sample(n = 95)

PGILMOUR_natural_sample.gcl <- PGILMOUR14.gcl %>% 
  dplyr::filter(`Otolith Mark Present` == "NO") %>% 
  dplyr::slice_sample(n = 95)

PSTOCK_natural_sample.gcl <- PSTOCK14.gcl %>% 
  dplyr::filter(`Otolith Mark Present` == "NO") %>% 
  dplyr::slice_sample(n = 95)

PAFK_hatchery_sample.gcl <- PHOGAN14.gcl %>% 
  dplyr::filter(`Otolith Mark ID` == "AFK12B") %>% 
  dplyr::slice_sample(n = 95)

sillyvec = c("PERB14_natural_sample",
             "PHOGAN14_natural_sample",
             "PGILMOUR_natural_sample",
             "PSTOCK_natural_sample",
             "PAFK_hatchery_sample")

# calculate Fst
gcl2Genepop.GCL(sillyvec = sillyvec,
                loci = loci,
                path = "../Genepop/All_Streams_14_16_develop/slim_95-sample_Sam_May_streams.txt")

fst <-
  PW_FST.GCL(
    sillyvec = sillyvec,
    loci = loci,
    inputfile = "../Genepop/All_Streams_14_16_develop/slim_95-sample_Sam_May_streams.txt",
    popnames = c("Erb", "Hogan", "Gilmour", "Stockdale", "AFK")
  )

range(fst[!fst == 0])  # 0.0001216 0.0015010

# end
