


# setwd("H:/My Drive/PWS_Empirical/Scripts")
# microhap_genos<-readRDS("../Data/all_streams_2014_2016_for_Sam_May.rds")

microhap_genos<-readRDS("../Genotypes/All_Streams_14_16_develop/OceanAK_Origin_PostQA/all_streams_2014_2016_for_Sam_May.rds")  # Kyle testing 2024-01-30


library(CKMRsim)
library(tidyverse)
library(adegenet)
library(sequoia)

colnames(microhap_genos)

hist(as.vector(nchar(microhap_genos[1,9:ncol(microhap_genos)]))) #so like 1/3 are microhaps

(ncol(microhap_genos)-8)/2

#Just take the first genotype from each microhap

snp_genos<-microhap_genos
snp_genos[,9:ncol(snp_genos)]<-sapply(snp_genos[,9:ncol(snp_genos)], function(x) substr(x,1,1))
hist(as.vector(nchar(snp_genos[1,9:ncol(snp_genos)]))) #Now none are microhaps

#now this is a singleton snp panel and I can compare it with the microhap one for parentage power

#vignette("CKMRsim-example-1")


##CKMRsim on snps:
snp_genos<-snp_genos %>% dplyr::select(!c(Silly,FishID,CAPTURE_LOCATION,CAPTURE_DATE,
                                          DNA_TRAY_CODE,DNA_TRAY_WELL_CODE,
                                          DNA_TRAY_WELL_POS))


#colnames(snp_genos) <- str_replace(colnames(snp_genos),pattern = "\\.1",replacement = "\\_1")
snp_genos<-snp_genos %>% column_to_rownames("SillySource")

i=1
for (i in seq(1,ncol(snp_genos),by=2)){
  Allele.1<-snp_genos[,i]
  Allele.2<-snp_genos[,i+1]
  alleles<-unique(c(Allele.1,Allele.2))
  alleles<-alleles[which(is.na(alleles)==F)]
  Allele.1=alleles[1]
  Allele.2=alleles[2]
  if(length(alleles)>2){print("problem with column")}
  snp_genos[,i]<-if_else(snp_genos[,i]==Allele.1,1,2)
  snp_genos[,i+1]<-if_else(snp_genos[,i+1]==Allele.1,1,2)
  #snp_genos[,i]<-paste(snp_genos[,i],snp_genos[,i+1],sep="")
  #snp_genos[,i]<-if_else(snp_genos[,i]=="21","12",snp_genos[,i])
  #snp_genos<-snp_genos[,-(i+1)]
  #i=i+2
}

#snp_genos[snp_genos=="NANA"]<-NA
snp_genos<-snp_genos %>% rownames_to_column("SillySource")

# then make some long format genotypes

nc <- ncol(snp_genos)
loci <- str_replace(names(snp_genos)[seq(2, nc, by = 2)], "\\.\\.\\.[0-9]+$", "")

#  reset the locus names
names(snp_genos)[seq(2, nc, by = 2)] <- str_c(loci, "1", sep = ".")
names(snp_genos)[seq(3, nc, by = 2)] <- str_c(loci, "2", sep = ".")

long_genos <- snp_genos %>% #dplyr::select(-pop) %>%
  gather(key = "loc", value = "Allele", -SillySource) %>%
  separate(loc, into = c("Locus", "gene_copy"), sep = "\\.") %>%
  mutate(Allele = as.character(Allele)) %>%
  #mutate(Allele = ifelse(Allele == "0", NA, Allele)) %>%
  rename(Indiv = SillySource)


alle_freqs <- long_genos %>%
  count(Locus, Allele) %>%
  group_by(Locus) %>%
  mutate(Freq = n / sum(n),
         Chrom = "Unk",
         Pos = as.integer(factor(Locus))) %>%
  ungroup() %>%
  dplyr::select(Chrom, Pos, Locus, Allele, Freq) %>%
  arrange(Pos, desc(Freq)) %>%
  mutate(AlleIdx = NA,
         LocIdx = NA) %>%
  filter(!is.na(Allele))

# Kyle - quick look at allele frequencies, assuming that those far away from 0.5 are microhaps?
alle_freqs %>% 
  dplyr::filter(Allele == 1) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Freq)) +
  ggplot2::geom_histogram()

input_AF<-reindex_markers(alle_freqs)

snp_ckmr<-create_ckmr(D = input_AF,
            kappa_matrix = kappas[c("PO", "FS", "HS", "U"), ],
            ge_mod_assumed = ge_model_TGIE,
            ge_mod_true = ge_model_TGIE,
            ge_mod_assumed_pars_list = list(epsilon = 0.005),
            ge_mod_true_pars_list = list(epsilon = 0.005))

#Simulate genotype pairs
snp_Qs <- simulate_Qij(snp_ckmr, 
                       calc_relats = c("PO", "FS", "HS", "U"),
                       sim_relats = c("PO", "FS", "HS", "U"),
                       reps=10000) #number of pairs of relationships

#Extract Log Likelihoods
PO_U_logls <- extract_logls(snp_Qs,
                            numer = c(PO = 1),
                            denom = c(U = 1))

ggplot(PO_U_logls,
       aes(x = logl_ratio, fill = true_relat)) +
  geom_density(alpha = 0.25)

ggplot(PO_U_logls %>% filter(true_relat %in% c("PO", "U")),
       aes(x = logl_ratio, fill = true_relat)) +
  geom_density(alpha = 0.25)





####MICROHAPS
microhap_genos<-microhap_genos %>% dplyr::select(!c(Silly,FishID,CAPTURE_LOCATION,CAPTURE_DATE,
                                          DNA_TRAY_CODE,DNA_TRAY_WELL_CODE,
                                          DNA_TRAY_WELL_POS))


#colnames(microhap_genos) <- str_replace(colnames(microhap_genos),pattern = "\\.1",replacement = "\\_1")
microhap_genos<-microhap_genos %>% column_to_rownames("SillySource")

i=1
for (i in seq(1,ncol(microhap_genos),by=2)){
  Allele.1<-microhap_genos[,i]
  Allele.2<-microhap_genos[,i+1]
  alleles<-unique(c(Allele.1,Allele.2))
  alleles<-alleles[which(is.na(alleles)==F)]
  
  microhap_genos[,i]<-match(microhap_genos[,i],alleles)
  microhap_genos[,i+1]<-match(microhap_genos[,i+1],alleles)
}

#microhap_genos[microhap_genos=="NANA"]<-NA
microhap_genos<-microhap_genos %>% rownames_to_column("SillySource")

# then make some long format genotypes

nc <- ncol(microhap_genos)
loci <- str_replace(names(microhap_genos)[seq(2, nc, by = 2)], "\\.\\.\\.[0-9]+$", "")

#  reset the locus names
names(microhap_genos)[seq(2, nc, by = 2)] <- str_c(loci, "1", sep = ".")
names(microhap_genos)[seq(3, nc, by = 2)] <- str_c(loci, "2", sep = ".")

long_genos_microhap <- microhap_genos %>% #dplyr::select(-pop) %>%
  gather(key = "loc", value = "Allele", -SillySource) %>%
  separate(loc, into = c("Locus", "gene_copy"), sep = "\\.") %>%
  mutate(Allele = as.character(Allele)) %>%
  #mutate(Allele = ifelse(Allele == "0", NA, Allele)) %>%
  rename(Indiv = SillySource)


alle_freqs_microhap <- long_genos_microhap %>%
  count(Locus, Allele) %>%
  group_by(Locus) %>%
  mutate(Freq = n / sum(n),
         Chrom = "Unk",
         Pos = as.integer(factor(Locus))) %>%
  ungroup() %>%
  dplyr::select(Chrom, Pos, Locus, Allele, Freq) %>%
  arrange(Pos, desc(Freq)) %>%
  mutate(AlleIdx = NA,
         LocIdx = NA) %>%
  filter(!is.na(Allele))

input_AF<-reindex_markers(alle_freqs_microhap)

microhap_ckmr<-create_ckmr(D = input_AF,
                      kappa_matrix = kappas[c("PO", "FS", "HS", "U"), ],
                      ge_mod_assumed = ge_model_TGIE,
                      ge_mod_true = ge_model_TGIE,
                      ge_mod_assumed_pars_list = list(epsilon = 0.005),
                      ge_mod_true_pars_list = list(epsilon = 0.005))

#Simulate genotype pairs
microhap_Qs <- simulate_Qij(microhap_ckmr, 
                       calc_relats = c("PO", "FS", "HS", "U"),
                       sim_relats = c("PO", "FS", "HS", "U"),
                       reps=10000) #number of pairs of relationships

#Extract Log Likelihoods
PO_U_logls <- extract_logls(microhap_Qs,
                            numer = c(PO = 1),
                            denom = c(U = 1))

ggplot(PO_U_logls,
       aes(x = logl_ratio, fill = true_relat)) +
  geom_density(alpha = 0.25)

ggplot(PO_U_logls %>% filter(true_relat %in% c("PO", "U")),
       aes(x = logl_ratio, fill = true_relat)) +
  geom_density(alpha = 0.25)



####Run Sequoia for just stockdale 2014_2016 to compare with FRANz using Ped_compare
# PWS_meta_data<-read.csv(file = "../Data/All_Streams_14_16_develop_postQA_OceanAK_paired_2014_2016_HOGAN_STOCK_GILMOUR_PADDY_ERB.csv")
PWS_meta_data<-read.csv(file = "Franz/All_Streams_14_16_develop_postQA_OceanAK_paired_2014_2016_HOGAN_STOCK_GILMOUR_PADDY_ERB.csv")
PWS_meta_data<- PWS_meta_data %>% mutate(SillySource = paste(SILLY,FISHID,sep="_"))

STOCKDALE_LH_DATA<-PWS_meta_data %>% 
  filter(SILLY%in%c("PSTOCK14","PSTOCK16")) %>% 
  select(SillySource,STREAM,BIRTH_YR,DEATH_YR,SEX) %>% 
  filter(SillySource%in%microhap_genos$SillySource) %>% 
  mutate(SEX)

snp_seq<-snp_genos %>% filter(SillySource%in%STOCKDALE_LH_DATA$SillySource)
snp_seq<-sequoia::GenoConvert(InData = snp_seq,InFormat = "double",OutFormat = "seq",Missing = NA,IDcol = 1)

Age_Priors<-sequoia::MakeAgePrior(Discrete = T,MaxAgeParent = 2)

STOCKDALE_LH_DATA<-STOCKDALE_LH_DATA %>% select(SillySource,SEX,BIRTH_YR) %>% 
  rename(ID = SillySource,Sex = SEX, BirthYear=BIRTH_YR) %>% 
  mutate(Sex=ifelse(Sex=="F",1,ifelse(Sex=="M",2,3)))
  
ParOUT<-sequoia::sequoia(GenoM = snp_seq,
                LifeHistData = STOCKDALE_LH_DATA,
                Module = "par",
                SeqList=list("AgePriors" = Age_Priors))

ParOUT<-sequoia::sequoia(GenoM = snp_seq,
                LifeHistData = STOCKDALE_LH_DATA,
                Module = "ped",
                SeqList=ParOUT)
#save(list = "ParOUT",file="../Data/ParOUT_10062022_Full_Ped_Test_Stockdale.Rdata")
load("../Data/ParOUT_10062022_Full_Ped_Test_Stockdale.Rdata")

# FRANz_ped<-read_csv("../Data/parentage_14_16.csv")
FRANz_ped<-read_csv("../Franz/All_Streams_14_16_develop/results/parentage.csv")  # pretty sure this is what I sent Sam May back on 2022-09-12
FRANz_ped<-FRANz_ped %>% rename(id = Offspring, sire = `Parent 1`, dam = `Parent 2`) %>% 
  #mutate(id=PWS_meta_data$SillySource[match(Offspring,PWS_meta_data$franz_id)]) %>% 
  filter(grepl(pattern = "PS",x = id)) %>% 
  select(id,dam,sire) %>% 
  #mutate(id=str_replace(id,pattern = "PSTOCK",replacement = "PS")) %>% #,
  #       dam=str_replace(dam,pattern = "PS16",replacement = "PSTOCK16"),
  #       sire=str_replace(dam,pattern = "PS14",replacement = "PSTOCK14"),
  #       sire=str_replace(dam,pattern = "PS16",replacement = "PSTOCK16"),
  #       dam=str_replace(dam,pattern = "_0",replacement = "_"),
  #       sire=str_replace(dam,pattern = "_0",replacement = "_"))
  as.data.frame() %>% sequoia::PedPolish() %>% 
  mutate(sex=PWS_meta_data$SEX[match(id,PWS_meta_data$franz_id)],
         dam_sex = PWS_meta_data$SEX[match(dam,PWS_meta_data$franz_id)],
         sire_sex = PWS_meta_data$SEX[match(sire,PWS_meta_data$franz_id)])  

for (i in 1:nrow(FRANz_ped)){
  dam <- FRANz_ped$dam[i]
  sire<-FRANz_ped$sire[i]
  if(is.na(FRANz_ped$dam_sex[i])==F){
  if(FRANz_ped$dam_sex[i]=="M"){
    FRANz_ped$sire[i]<-dam
    FRANz_ped$dam[i]<-sire
    FRANz_ped$sire_sex[i]<-"M"
    FRANz_ped$dam_sex[i]<-"F"
  }}
  if(is.na(FRANz_ped$sire_sex[i])==F){
  if(FRANz_ped$sire_sex[i]=="F"){
    FRANz_ped$sire[i]<-dam
    FRANz_ped$dam[i]<-sire
    FRANz_ped$sire_sex[i]<-"M"
    FRANz_ped$dam_sex[i]<-"F"
  }}
  
}


seq_ped <- ParOUT$Pedigree %>% sequoia::PedPolish() %>% 
  mutate(id = ifelse(id%in%ParOUT$DummyIDs$id, id, PWS_meta_data$franz_id[match(id,PWS_meta_data$SillySource)]),
         dam = ifelse(dam%in%ParOUT$DummyIDs$id, dam, PWS_meta_data$franz_id[match(dam,PWS_meta_data$SillySource)]),
         sire = ifelse(sire%in%ParOUT$DummyIDs$id, sire, PWS_meta_data$franz_id[match(sire,PWS_meta_data$SillySource)]))


franz_seq_compare<-sequoia::PedCompare(seq_ped,FRANz_ped)
franz_seq_compare$Mismatch

sequoia::SummarySeq(FRANz_ped)
xxx<-sequoia::SummarySeq(ParOUT$Pedigree)
sequoia::SummarySeq(seq_ped)

franz_seq_compare<-sequoia::PedCompare(seq_ped,FRANz_ped)

sequoia::SummarySeq(franz_seq_compare$ConsensusPed)

