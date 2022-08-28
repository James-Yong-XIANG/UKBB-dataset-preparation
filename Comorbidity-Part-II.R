### perform the first analysis ### 
source("/exeh_4/xyong/project/R_361_lib.R")
system("ls")
setwd("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data")
list.files()
rm(list=ls())
# import the hesin data ## 
# hesin <- fread("hesin_icd10_psych_diagnoses_date-June-2.csv.gz")
# hesin <- fread("hesin_icd10_diagnoses_date-update-to-20210930-access-July-7-2022.csv.gz")
hesin <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/Old-hesin_icd10_diagnoses_date-update-to-20210930-access-20211221.csv.gz") # till to 20210930
hesin <- hesin[,c("eid","diag_icd10","diagnoses_date")]
hesin$source <- "hesin"
hesin$diagnoses_date <- ymd(hesin$diagnoses_date)
# cat(names(hesin), sep = "\",\"")
hesin$diagnoses_date <- as.character(hesin$diagnoses_date)

## import the gp_clinical_tpp_icd10 ## 
# tpp <- fread("gp_clinical_icd10.txt.gz")
# head(tpp)
# tpp <- fread("gp_clinical_icd10-20220707.txt.gz")
tpp <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_tpp_clinical_icd10-20210908.txt.gz") # till 20210908
tpp <- tpp[,c("eid", "icd10_code", "event_dt")]
setnames(tpp, c("icd10_code", "event_dt"), c("diag_icd10","diagnoses_date"))
tpp$source <- "tpp"
# tpp$diagnoses_date <- dmy(tpp$diagnoses_date)
tpp$diagnoses_date <- as.character(tpp$diagnoses_date)

## import the gp_clinical_emis_icd10 ## 
# emis <- fread("gp_clinical_emis_icd10_diagnoses_date.txt.gz")
# emis <- fread("gp_clinical_emis_icd10_diagnoses_date-20220707.txt.gz")
# fwrite(emis, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_emis_icd10_diagnoses-accessed-20220218-updated-to-20210927.txt.gz")

emis <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_emis_icd10_diagnoses-accessed-20220218-updated-to-20210927.txt.gz") # till to 20210907
emis <- emis[,c("eid", "mapTarget", "event_dt")]
emis$event_dt <- dmy(emis$event_dt)
emis <- emis %>% dplyr::filter(event_dt != "1901-01-01" & event_dt != "1902-02-02" & event_dt != "1903-03-03" & event_dt != "2037-07-07")
# summary(emis$event_dt)
# "1900-01-01" "2008-05-15" "2014-01-20" "2012-04-30" "2018-03-09" "2021-09-27"
setnames(emis, c("mapTarget", "event_dt"), c("diag_icd10","diagnoses_date"))
emis$source <- "emis"
# emis$diagnoses_date <- ymd(emis$diagnoses_date)
emis$diagnoses_date <- as.character(emis$diagnoses_date)


## import the self_report ### 
self_report <- fread("self-report-icd-diagnoses-date-June-3.csv.gz") # updated till to 2019-09-09
self_report <- self_report[,c("eid", "ICD_10", "self_report_date")]
setnames(self_report, c("ICD_10", "self_report_date"), c("diag_icd10","diagnoses_date"))
self_report$source <- "self_report"
self_report$diagnoses_date <- as.character(self_report$diagnoses_date)
# self_report$diagnoses_date <- ymd(self_report$diagnoses_date)
# summary(self_report$diagnoses_date)

#### import death cause data ### 
# death <- fread("hesin_death_cause_icd10_date-June-2.csv.gz")
# death <- fread("hesin_death_cause_icd10_date-update-to-2021112-access-July-7-2022csv.gz")
death <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/Old-hesin_death_cause_icd10_date-update-to-2021112-access-20221221.csv.gz") # updated till to 20211112
death <- death[,c("eid", "cause_icd10", "date_of_death")]
setnames(death, c("cause_icd10", "date_of_death"),c("diag_icd10","diagnoses_date"))
# death$diagnoses_date <- dmy(death$diagnoses_date)
death$diagnoses_date <- as.character(death$diagnoses_date)
death$source <- "death_cause"



health_outcome_icd10 <- rbindlist(list(hesin, tpp, emis, self_report, death)) 
health_outcome_icd10$diagnoses_date <- ymd(health_outcome_icd10$diagnoses_date)

#### remove the 1901-01-01

health_outcome_icd10 <- dplyr::filter(health_outcome_icd10, diagnoses_date != ymd("1900-01-01")) %>% dplyr::filter(diagnoses_date != ymd("2037-07-07") & diagnoses_date != ymd("1901-01-01") & diagnoses_date != ymd("1902-02-02") & diagnoses_date != ymd("1903-03-03") )




# fwrite(health_outcome_icd10, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/health-outcome-June-4/UKBB-health-outcome-icd10-update-to-20211112-accessed-on-20220616.csv.gz")
fwrite(health_outcome_icd10, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/health-outcome-June-4/UKBB-health-outcome-icd10-rm-invalided-date    -update-to-20211112-accessed-on-20220218.csv.gz")




# ##### check the data formate ## 
# tpp_lab <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_tpp_lab_test_res.txt.gz")
# # tpp_lab <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_emis_lab_test_res.txt.gz")
# # tpp_lab <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-gp-record/tpp_emis_atc-29-May.csv.gz")
# tpp_lab <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_tpp_covid-vaccine-first-dose-June-4.csv.gz")


# head(tpp_lab)
# setnames(tpp_lab, "issue_date", "event_dt")
# tpp_lab$event_dt <- dmy(tpp_lab$event_dt)

# tpp_lab <- dplyr::filter(tpp_lab, event_dt != ymd("1900-01-01")) %>% 
#             dplyr::filter(event_dt != ymd("1901-01-01")) %>% 
#             dplyr::filter(event_dt != ymd("1902-02-02")) %>% 
#             dplyr::filter(event_dt != ymd("1903-03-03")) %>% 
#             dplyr::filter(event_dt != ymd("2037-07-07")) 
# summary(tpp_lab$event_dt)
# length(unique(tpp_lab$eid))

# # Other info from the emis 
# # 65F0\.|‭1119305005‬|1119349007|‭1142178009‬|840534001|1144997007|1144998002|840534001|1119350007





# 
