

##################### PART II #####################
##### get the imputation results with Num.tree = 1000 #### 
source("/exeh_4/xyong/project/R_361_lib.R")
# library(magrittr)
system("ls")
library(venn)
rm(list=ls())



#### import the dataset #### 
comorbidity <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/health-outcome-June-4/UKBB-health-outcome-icd10-rm-invalided-date-update-to-20211112-accessed-on-20220218.csv.gz")

comorbidity$diagnoses_date <- ymd(comorbidity$diagnoses_date)


# extract the diagnose category
diag_cat <- unique(comorbidity$diag_icd10)

# import the whole eid in UKBB # 
eid_source <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/UKBB-eid-FU-source-records-till-Aug-30.csv.gz")

## import the ccsr disease category ## 

# load the CCSR comorbidity code ## 
# load("/home/nick/icd10/All_mapped_icd10.Rdata")
load("/exeh_4/nick/icd10/All_mapped_icd10.Rdata")
load("/exeh_4/xyong/project/UKBB/May-26/CCSR-ICD-Dict/CCSR-cat-name.Rdata")
## check the DM CCSR code 
cat(str_sub(END006, start =2, end = 5) %>% unique)


# extract the hepatic failure # 
# DIG018	Hepatic failure	Gallbladder, pancreatic, and liver disease
DIG018 <- diag_cat[grepl(paste0(DIG018, collapse="|"), diag_cat)] # obtain only two records 


# use the following code to extract liver cirrhosis
# ref: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7500192/
# Code info: file:///Users/yongxiang/Desktop/bmjgast-2020-000485supp001.pdf 
# ref to the table 1
# liver cirrhosis: K70.3, K74.4, K74.5, K74.6
# Cirrhosis-related complications: 
# HCC:hepatocellular carcinoma -> C22.0
# Ascites	R18
# Varices   I85.0, I85.9, I86.4, I98.2, I98.3
# SBP: spontaneous bacterial peritonitis -> K65.0, K65.9
# HE: hepatic encephalopathy -> G31.2, G93.4 


liver_cirrhosis_code <- c("K703", "K744", "K745", "K746")
liver_cirrhosis <- comorbidity[diag_icd10 %in% liver_cirrhosis_code, ]

HCC <- comorbidity[diag_icd10 %in% c("C22", "C220"), ]
Ascites <- comorbidity[diag_icd10 %in% c("R18")]
Varices <- comorbidity[diag_icd10 %in% c("I850", "I859", "I864", "I982", "I983")]
SBP <- comorbidity[diag_icd10 %in% c("K650", "K659")]
HE <- comorbidity[diag_icd10 %in% c("G312", "G934")]

# get the first date of diagnoses 
# # cmp the prinary death cause and the diagnoses made in death cause dataset 
# death <- fread( "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/Old-hesin_death_cause_icd10_date-update-to-2021112-access-20221221.csv.gz")

# liver_cirrhosis_cause <- death[cause_icd10 %in% liver_cirrhosis_code]

# eid_liver_cirrhosis_death_cause_only <- setdiff(unique(liver_cirrhosis$eid), liver_cirrhosis_without_death_cause$eid)
# liver_cirrhosis_cause_only <- liver_cirrhosis_cause[eid %in% eid_liver_cirrhosis_death_cause_only]
# table(liver_cirrhosis_cause_only$level)


# with 110 subjects with liver cirrhosis diagnoses in death cause only; primary cause 42 and second death cause 72

## creat the get the first occurrence function #
get_frist_occurrence <- function(x){
    tmp <- data.frame(get(x)) %>% 
    dplyr::filter(source != "death_cause") %>%  
    group_by(eid) %>% summarize(
        first_occurrence = min(diagnoses_date),
        # source = min(source)
    ) %>% data.frame
    setnames(tmp, "first_occurrence", x)
}

## creat the get the first occurrence function #
get_frist_occurrence_with_dc <- function(x){
    tmp <- data.frame(get(x)) %>% 
    # dplyr::filter(source != "death_cause") %>%  
    group_by(eid) %>% summarize(
        first_occurrence = min(diagnoses_date),
        # source = min(source)
    ) %>% data.frame
    setnames(tmp, "first_occurrence", x)
}

liver_cirrhosis_without_dc <- get_frist_occurrence("liver_cirrhosis")
HCC_without_dc <- get_frist_occurrence("HCC")
Ascites_without_dc <- get_frist_occurrence("Ascites")
Varices_without_dc <- get_frist_occurrence("Varices")
SBP_without_dc <- get_frist_occurrence("SBP")
HE_without_dc <- get_frist_occurrence("HE")


liver_cirrhosis_with_dc <- get_frist_occurrence_with_dc("liver_cirrhosis")
HCC_with_dc <- get_frist_occurrence_with_dc("HCC")
Ascites_with_dc <- get_frist_occurrence_with_dc("Ascites")
Varices_with_dc <- get_frist_occurrence_with_dc("Varices")
SBP_with_dc <- get_frist_occurrence_with_dc("SBP")
HE_with_dc <- get_frist_occurrence_with_dc("HE")

# lapply(list(liver_cirrhosis_without_dc, HCC_without_dc, Ascites_without_dc, Varices_without_dc, SBP_without_dc, HE_without_dc), dim)
liver_without_dc <- Reduce(function(x, y) merge(x, y, by = "eid", all = TRUE), list(liver_cirrhosis_without_dc, HCC_without_dc, Ascites_without_dc, Varices_without_dc, SBP_without_dc, HE_without_dc))


## get the liver with dc dataset 
liver_with_dc <- Reduce(function(x, y) merge(x, y, by = "eid", all = TRUE), list(liver_cirrhosis_with_dc, HCC_with_dc, Ascites_with_dc, Varices_with_dc, SBP_with_dc, HE_with_dc))
lapply(list(liver_cirrhosis_with_dc, HCC_with_dc, Ascites_with_dc, Varices_with_dc, SBP_with_dc, HE_with_dc), dim)
liver_with_dc


UKBB_basic <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/UKBB-Whole-basic-demographic-variables.csv.gz")
# update the date of death 
UKBB_basic_death <- merge(UKBB_basic[, -c("date_of_death", "exculded_due_to_death")], death[, c("eid", "date_of_death")], all.x=TRUE) %>% mutate(date_of_death = ymd(date_of_death))

fwrite(UKBB_basic_death, "/exeh_4/xyong/project/UKBB/May-26/raw-data/UKBB-Whole-basic-demographic-variables-4-Aug-2022.csv.gz")
##### obtain the liver cirrhosis with whole UKBB eid #####
liver_with_dc_whole <- merge(UKBB_basic_death, liver_with_dc,  by="eid", all.x =TRUE)
str(liver_with_dc_whole)


##### extract the DM related subjects ### 

# END002	Diabetes mellitus without complication	Diabetes mellitus
# END003	Diabetes mellitus with complication	Diabetes mellitus
# END004	Diabetes mellitus, Type 1	Diabetes mellitus
# END005	Diabetes mellitus, Type 2	Diabetes mellitus
# END006	Diabetes mellitus, due to underlying condition, drug or chemical induced, or other specified type	Diabetes mellitus
# DIG018 <- diag_cat[grepl(paste0(DIG018, collapse="|"), diag_cat)]
DM_without_complication <- comorbidity[diag_icd10 %in% diag_cat[grepl(paste0(END002, collapse="|"), diag_cat)], ]

DM_with_complication <- comorbidity[diag_icd10 %in% diag_cat[grepl(paste0(END003, collapse="|"), diag_cat)],]
T1DM <- comorbidity[diag_icd10 %in% diag_cat[grepl(paste0(END004, collapse="|"), diag_cat)],]
T2DM <- comorbidity[diag_icd10 %in% diag_cat[grepl(paste0(END005, collapse="|"), diag_cat)],]
DM_other_type <- comorbidity[diag_icd10 %in% diag_cat[grepl(paste0(END006, collapse="|"), diag_cat)]]

# lapply(list(get_frist_occurrence_with_dc("DM_with_complication"), get_frist_occurrence_with_dc("DM_without_complication"), get_frist_occurrence_with_dc("T1DM"), get_frist_occurrence_with_dc("T2DM"), get_frist_occurrence_with_dc("DM_other_type")), dim)

DM <- list(get_frist_occurrence_with_dc("DM_with_complication"), get_frist_occurrence_with_dc("DM_without_complication"), get_frist_occurrence_with_dc("T1DM"), get_frist_occurrence_with_dc("T2DM"), get_frist_occurrence_with_dc("DM_other_type")) 
DM <- Reduce(function(x, y) merge(x, y, by = "eid", all = TRUE), DM)

## merge DM liver_cirrhosis ukbb_basic 
res <- Reduce(function(x, y) merge(x, y, by = "eid", all = TRUE), list(UKBB_basic_death, liver_with_dc, DM))
## output these results
fwrite(res, "/exeh_4/xyong/project/Liver_cirrhosis/UKBB-liver-cirrhosis-DM-updated-on-2-Aug-2022.csv.gz")


