# update the emis code in from 03/03/2021 to 08/06/2021
# GP prescriptions (EMIS, COVID19)	covid19_emis_gp_scripts	85746451	08/06/2021
## import the function ##
source("/exeh_4/xyong/project/R_361_lib.R")
library(XML)
library("methods")
library(data.table)


## download the GP record updated to August 31 2021 -> accessed on Feb-18-2022 ##


getwd()
# using the dmd_bnf_emis_atc_29_May code to decode the atc code in the whole 
dmd_code_bnf_atc <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-2/dmd_bnf_emis_atc_29-May.csv.gz")
setwd("/exeh_4/xyong/project/UKBB/Feb-18-2022/gp-script")
dmd_code_bnf_atc$dmd_code <- as.character(dmd_code_bnf_atc$dmd_code)
# tpp <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/covid19_tpp_gp_scripts.txt.gz")
# emis <- fread("/exeh_4/xyong/project/UKBB/Jun-30/raw-data/covid19_emis_gp_scripts.txt.gz")
# emis <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/Aug-25-update-emis-gp-clinical/covid19_emis_gp_scripts.txt.gz")
tpp_script <- fread("covid19_tpp_gp_scripts.txt.gz")
emis_script <- fread("covid19_emis_gp_scripts.txt.gz")
tpp <- tpp_script
emis <- emis_script

setnames(emis, "code", "dmd_code")
tpp$dmd_code <- as.character(tpp$dmd_code)
tpp_atc <- merge(tpp, dmd_code_bnf_atc)
emis_atc <- merge(emis[,c("eid", "issue_date", "dmd_code")], dmd_code_bnf_atc)

tpp_emis_atc <- rbind(tpp_atc, emis_atc) %>% unique 
tpp_emis_atc$issue_date <- dmy(tpp_emis_atc$issue_date)
# str(tpp_emis_atc)
tpp_emis_atc
summary(tpp_emis_atc$issue_date)
# rm the invalid code with invalided with invaild date
gp_atc <- dplyr::filter(tpp_emis_atc, issue_date != ymd("1900-01-01")) %>% 
            dplyr::filter(issue_date != ymd("1901-01-01")) %>% 
            dplyr::filter(issue_date != ymd("1902-02-02")) %>% 
            dplyr::filter(issue_date != ymd("1903-03-03")) %>% 
            dplyr::filter(issue_date != ymd("2037-07-07")) 
summary(gp_atc$issue_date)
        # Min.      1st Qu.       Median         Mean      3rd Qu.         Max.
# "1950-07-02" "2009-05-21" "2014-06-20" "2013-02-05" "2018-02-06" "2021-04-23"
# fwrite(gp_atc, "/exeh_4/xyong/project/UKBB/Jun-30/gp-atc/tpp_emis_atc-30-Jun.csv.gz")
# update to Aug-10-EMIS May-20-TPP 
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "1950-07-02" "2009-06-29" "2014-08-06" "2013-03-24" "2018-04-05" "2021-07-25"

        Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
"1950-07-02" "2009-08-11" "2014-10-01" "2013-05-18" "2018-06-12" "2021-09-27" 

# fwrite(gp_atc, "/exeh_4/xyong/project/UKBB/Jun-30/gp-atc/tpp_emis_atc-24-Sep.csv.gz")
fwrite(gp_atc, "/exeh_4/xyong/project/UKBB/Jun-30/gp-atc/tpp_emis_atc-updated-till-to-20210927-Feb-21-2022.csv.gz")

