
### perform the first analysis ### 
source("/exeh_4/xyong/project/R_361_lib.R")
##### upload the mapping dataset ##### 
# scp -r /Users/yongxiang/Desktop/primarycare_codings/ xyong@137.189.51.203:/exeh_4/xyong/project/UKBB 
system("ls")
## load all the excel sheet into R #### 
# filename <- "/exeh_4/xyong/project/UKBB/primarycare_codings/all_lkps_maps_v2.xlsx"
### the primarycare_codings data (provided by UKBB) were downloaded from: https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=592 
#  wget  -nd  biobank.ndph.ox.ac.uk/ukb/ukb/auxdata/primarycare_codings.zip
filename <- "/exeh_4/xyong/project/UKBB/Apr-8/primarycare_codings/all_lkps_maps_v2.xlsx"
sheets <- openxlsx::getSheetNames(filename)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename)
names(SheetList) <- sheets
head(SheetList[[5]], 20)

icd9_icd10 <- SheetList[["icd9_icd10"]]
head(SheetList[["Description"]])
head(SheetList[["read_v3_lkp"]])

icd9_icd10 <-  (SheetList[["icd9_icd10"]])
SheetList[["icd9_icd10"]]$ICD9
head(icd9_icd10)
## tidy the results and save to RData files -> easy to load into R ##








### update the previous application number in ukbb. mainly update the datasets accessed at 21-Dec-2022 # 
# last update at Dec-21
setwd("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/hesin/Dec-21")

## get the new working directory # 
# setwd("/exeh_4/xyong/project/UKBB/Jun-16-2022/covid19_data_20220616")
# setwd("/exeh_4/xyong/project/UKBB")
# setwd("/exeh_4/xyong/project/UKBB/May-26/raw-data")

#### load the hesin.txt.gz the main table for the inhospital dataset ## 
# https://biobank.ndph.ox.ac.uk/showcase/showcase/docs/HospitalEpisodeStatistics.pdf
hesin <- fread("hesin.txt.gz") # 3738546 # 3776176 # 3920626

# length(unique(hesin$eid)) # 439461 # 440145 # 442988
# hesin$admidate <- dmy(hesin$admidate)
# max(hesin$admidate)
# summary(hesin$admidate)
#  Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "1980-12-02" "2007-01-10" "2012-11-26" "2011-07-18" "2017-03-03" "2020-12-31"; NA -  "248474"

        # Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "1980-12-02" "2007-02-05" "2013-01-03" "2011-08-22" "2017-04-14" "2021-02-28" 
        # NA's     "250765" 

        # Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "1980-12-02" "2007-05-21" "2013-05-17" "2012-01-02" "2017-09-25" "2021-09-30" 
        # NA's    # "259162" 

hesin$disdate <- dmy(hesin$disdate)
summary(hesin$disdate) # "1981-01-01" "2006-09-13" "2012-08-15" "2011-04-20" "2016-12-19" "2020-12-31"; NA -    "502029" 
# "1981-01-01" "2006-10-06" "2012-09-17" "2011-05-22" "2017-01-30" "2021-02-28" 
        # NA's     "510441" 


#### load the diagnoses dataset ## 
diagnoses <- fread("hesin_diag.txt.gz") # [1] 14006184        8 # 14262530
head(diagnoses)
length(unique(sort(diagnoses$eid))) # 438758 # 439442 # 442287
# a <- table(diagnoses$diag_icd9) %>% as.data.frame %>% arrange(desc(Freq))
# head(a,10)
table(diagnoses$level) # "1 Primary/main diagnosis 2 Secondary diagnosis 3 External cause"
#       1        2        3 
#  3749387 10155845   357298 
# 3893842 10968953   367050 -> 21-Dec-2022

# dim(diagnoses)
summary(diagnoses$diag_icd10_nb) ## no use 
table(diagnoses$diag_icd10_nb)
summary(diagnoses$diag_icd9_nb) ## no use 



diag_icd10 <- table(diagnoses$diag_icd10) %>% as.data.table
# fwrite(diag_icd10, "diag_icd10.txt", sep="\t")

diag_without_any_record <- diagnoses[which(diagnoses$diag_icd9 == "" & diagnoses$diag_icd10 == ""),] # empty means the whole dataset both at least both one diagnoses terms. 

diag_without_icd10 <- diagnoses[which( diagnoses$diag_icd10 == ""),] # have 72232 term have icd9 but no icd10. 
# ## ### #### -> means we nned to convert the icd9 code to icd10 ### 


diag_with_overlap <- diagnoses[which(diagnoses$diag_icd9 != "" & diagnoses$diag_icd10 != ""),] ## means they do not have overlap diagnoses terms 


##### map the missing NA in diag_icd10 ##### 
# diag_without_icd10 <- diagnoses[which( diagnoses$diag_icd10 == ""),]
# diagnoses_NA <- dplyr::filter(diagnoses, diag_icd10 =="") # 72235 







##### the icd10_NA_UNDEF are mapped by using the icd_map ###### 

icd10_matched <- dplyr::filter(diagnoses, diag_icd10 !="")

##### map the NA of diag_icd10 using the icd9_icd10 ##
icd10_NA <- merge(as.data.frame(diag_without_icd10), as.data.frame(icd9_icd10), by.x= "diag_icd9", by.y="ICD9") # with 2630 UNDEF in ICD10

icd10_NA_matched_1 <- dplyr::filter(icd10_NA, ICD10!="UNDEF")
head(icd10_NA_matched_1)
icd10_NA_matched_1 <- icd10_NA_matched_1[,c("eid","ins_index","arr_index","level","diag_icd9","diag_icd9_nb","ICD10","diag_icd10_nb")]
# diag_icd10
setnames(icd10_NA_matched_1, "ICD10", "diag_icd10")
# diagnoses_icd10[14274034,7] == ""


icd10_NA_UNDEF <- dplyr::filter(icd10_NA, ICD10=="UNDEF") # after checking, some UNDEF icd9 are still valuable to be mapped.

##### https://www.nber.org/research/data/icd-9-cm-and-icd-10-cm-and-icd-10-pcs-crosswalk-or-general-equivalence-mappings
# get the update version of icd9 to icd 10 #### 
# /exeh_4/xyong/project/UKBB/May-26/dmd-code/ICD9-ICD10/icd10toicd9gem.csv.gz
icd_map <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/ICD9-ICD10/icd10toicd9gem.csv.gz")
head(icd_map)
icd_map <- icd_map[,c("icd9cm", "icd10cm")]
colnames(icd_map) <- c("icd9", "icd10")
icd_map <- icd_map %>% distinct(icd9, .keep_all = TRUE)
## try the icd_map ## 
icd10_NA_UNDEF_sec <- merge(icd10_NA_UNDEF, icd_map, by.x= "diag_icd9", by.y="icd9" ) # 9023
icd10_NA_UNDEF_sec <-  icd10_NA_UNDEF_sec[,c("eid","ins_index","arr_index","level","diag_icd9","diag_icd9_nb","icd10","diag_icd10_nb")]
setnames(icd10_NA_UNDEF_sec, "icd10", "diag_icd10")

### combine all records with icd10 code 
diagnoses_icd10 <- rbindlist(list(icd10_matched, icd10_NA_matched_1, icd10_NA_UNDEF_sec)) %>% distinct()
diagnoses_icd10 <-  diagnoses_icd10[,c("eid","ins_index","arr_index","level","diag_icd10")]

# check the missing value in the diagnoses dataset 
prop_miss_case(diagnoses_icd10) # 0
diagnoses_icd10[diag_icd10=="",] # empty
# fwrite(diagnoses_icd10, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/hesin_diagnoses_icd10_June-1.txt.gz")
 

###### diagnoses_icd10 within inhospital record ## 

# hesin$epistart <- dmy(hesin$epistart) 
for (i in c("epistart", "admidate", "epiend", "disdate")){
    hesin[[i]] <- dmy(hesin[[i]])
}

# check whether the epistart date is identical to the admidata # 
identical(hesin$epistart, hesin$admidate)
hesin <- hesin[!which(is.na(hesin$epistart) & is.na(hesin$admidate)),] # rm 42 terms of no date of epistart and admidate # 42 rows without epistart, admidata, epiend, and disdate, hence these 42 rows are removed. 

# use the eid and ins_index to get the date of each icd10 
hesin_icd10  <- merge(diagnoses_icd10, hesin, by =c("eid","ins_index"))

# length(unique(hesin$eid))  # 440145
# length(unique(diagnoses_icd10$eid)) # 439235
# length(unique(hesin_icd10$eid)) # 439235
# hesin_icd10$diagnoses_date <- ifelse(is.na(hesin_icd10$epistart), hesin_icd10$admidate, hesin_icd10$epistart)

hesin_icd10$diagnoses_date <- hesin_icd10$epistart
# if the diagnoses_date is missing, use the admidate instead.
hesin_icd10[is.na(diagnoses_date),"diagnoses_date"] <- hesin_icd10[is.na(diagnoses_date),"admidate"] 

# reformate the column. 
hesin_icd10 <- hesin_icd10[, c("eid","ins_index","arr_index","level","diag_icd10","diagnoses_date","dsource","source","epistart","epiend","epidur","bedyear","epistat","epitype","epiorder","spell_index","spell_seq","spelbgin","spelend","speldur","pctcode","gpprpct","category","elecdate","elecdur","admidate","admimeth_uni","admimeth","admisorc_uni","admisorc","firstreg","classpat_uni","classpat","intmanag_uni","intmanag","mainspef_uni","mainspef","tretspef_uni","tretspef","operstat","disdate","dismeth_uni","dismeth","disdest_uni","disdest","carersi")]





# fwrite(hesin_icd10, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/hesin_icd10_diagnoses_date-update-to-20210930-access-July-7-2022.csv.gz")
fwrite(hesin_icd10, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/Old-hesin_icd10_diagnoses_date-update-to-20210930-access-20211221.csv.gz")




### DID NOT UPDATED ### 

### ### DON'T RUN ### ###  ### ### DON'T RUN ### ### ### ### DON'T RUN ### ### 
#### load the critical dataset #### 
critical <- fread("hesin_critical.txt.gz")
length(unique(critical$eid)) # 22395
dim(critical) # "2021-02-27" # [1] 32047    29
max(critical$ccstartdate) # "2021-02-27"
critical_icd10 <- merge(diagnoses_icd10, critical, by =c("eid","ins_index", "arr_index"), all.y=TRUE) #  32047    31
head(critical_icd10)
a <- critical_icd10[which(is.na(critical_icd10$diag_icd10)),]
b <- merge(diagnoses_icd10[,-c("arr_index")], a[,-c("diag_icd10", "level")], by =c("eid","ins_index"))
head(b)

length(unique(a$eid))
length(unique(b$eid))

critical_icd10_date  <- rbind(critical_icd10[!which(is.na(critical_icd10$diag_icd10)),], b)
# which(critical_icd10$diag_icd10=="J1282")
# • Encounter for screening for COVID-19 (Z11.52)
# • Contact with and (suspected) exposure to COVID-19 (Z20.822)
# • Personal history of COVID-19 (Z86.16)
# • Multisystem inflammatory syndrome (MIS) (M35.81)
# • Other specified systemic involvement of connective tissue (M35.89)
# • Pneumonia due to coronavirus disease 2019 (J12.82)

for (i in c("ccstartdate", "ccdisdate", "ccdisrdydate")){
    critical_icd10_date[[i]] <- dmy(critical_icd10_date[[i]])
}
summary(critical_icd10_date$ccstartdate)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2008-05-01" "2014-03-18" "2016-10-10" "2016-07-22" "2018-12-19" "2021-02-27" 
# "2008-05-01" "2014-04-01" "2016-10-24" "2016-08-07" "2019-01-14" "2021-03-30" 
critical_care <- dplyr::filter(critical_icd10_date, critical_icd10_date$ccstartdate > dmy("1/1/2020"))
summary(critical_care$ccstartdate)

length(unique(sort(critical_care$eid))) ## 2845 ## in this care # 3045

fwrite(critical_icd10_date, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/hesin_critical_icd10_date_update-to-20210330-access-July-7-2022.csv.gz")



#### load the psych dataset ### 
psych <- fread("hesin_psych.txt.gz")
# dim(psych)
table(psych$admistat)
#    0       1       2       8       9 
#   14862    4898     316 3213325   86232 
# https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=229
# 0	No known previous psychiatric episodes
# 1	One or more previous psychiatric episodes with this Health Care Provider
# 2	One or more previous psychiatric episodes with another Health Care Provider
# 8	Not applicable
# 9	Not known

table(psych$mentcat)
# table(psych$leglstat)
#  1  2  5  9 
#  8  1  1 27 
# https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=228
# 1	Mental illness
# 2	Mental impairment
# 3	Severe mental impairment
# 4	Psychopathic disorder
# 5	Other
# 8	Not applicable
# 9	Not known

#### merge the psych data into the hesin_icd10 ###
hesin_icd10_psych <- merge(hesin_icd10, psych, by =c("eid","ins_index"), all = T)
head(hesin_icd10_psych)
summary(hesin_icd10_psych$diagnoses_date)
fwrite(hesin_icd10_psych, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/hesin_icd10_psych_diagnoses_date-update-to-20210930-access-July-7-2022")


### ### DON'T RUN ### ###  ### ### DON'T RUN ### ### ### ### DON'T RUN ### ### 
# DON'T RUN ABOVE CODE # 
### ### DON'T RUN ### ###  ### ### DON'T RUN ### ### ### ### DON'T RUN ### ### 




##### update the death records #### 


death <- fread("death.txt.gz")
death_cause <- fread("death_cause.txt.gz")
death_date <- merge(death, death_cause, by = c("eid", "ins_index"), all.x = TRUE)
length(unique(death$eid)) #37897;
length(unique(death_date$eid))
death_date$date_of_death <- dmy(death_date$date_of_death)
summary(death_date$date_of_death)
# "2006-05-10" "2014-09-24" "2017-11-05" "2017-02-26" "2020-03-05" "2021-11-12"
fwrite(death_date, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/Old-hesin_death_cause_icd10_date-update-to-2021112-access-20221221.csv.gz")
# list.files("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/")


##### update the self-report data to ICD10 code #### 
# this part can be updated when we extract the new version main dataset ## 

#### map the self-report date to ICD10 ## 
self <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/Self-report-And-Death/Self_report_non_cancer.csv.gz")
self_date <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/Self-report-And-Death/Self_report_non_cancer_date.csv.gz")

self_long <- melt(self, id.vars=c("eid")) %>% drop_na(value)
self_date_long <- melt(self_date, id.vars=c("eid")) %>% drop_na(value)
self_date_long$variable <- gsub("20008-", "20002-",self_date_long$variable)
colnames(self_date_long) <- c("eid", "variable", "report_date")
self_code_date <- merge(self_long, self_date_long, by = c("eid", "variable"))
max(self_code_date$report_date) # 2019.67

# discard the invalided date value
#  -1 (Date uncertain or unknown) -3 (Preferred not to answer)

##### replaced all the -1 and -3 in each instance to the max date of each instance #### 
self_code_date$variable <- str_sub(str_split_fixed(self_code_date$variable, "-", 2)[,2], end=1)
table(self_code_date$variable)
    #  0      1      2      3 
# 934827  55704 112218   2122 
# 0 - 2010.7
# 1 - 2013.4
# 2 - 2019.67
# 3 - 2019.67

##### formate the imcomplete date into a specific date #### 
self_list <- split(self_code_date, self_code_date$variable)
self_list[[1]]$report_date <- gsub('-[1-3]', "2010.7", self_list[[1]]$report_date)
self_list[[2]]$report_date <- gsub('-[1-3]', "2013.4", self_list[[2]]$report_date)
self_list[[3]]$report_date <- gsub('-[1-3]', "2019.67", self_list[[3]]$report_date)
self_list[[4]]$report_date <- gsub('-[1-3]', "2019.67", self_list[[4]]$report_date)
self_df <- rbindlist(self_list)
self_df$report_date <- as.numeric(self_df$report_date)
self_df$report_date <- as.character(self_df$report_date)

self_df$year <- str_sub(self_df$report_date, end=4)
self_df$mon <- ifelse(str_sub(self_df$report_date, start=6, end=6)==0, str_sub(self_df$report_date, start=7, end=7), str_sub(self_df$report_date, start=6, end=6))
self_df$day <- str_sub(self_df$report_date, start=7, end=7)
self_df[self_df$day=="",7] <- 1
self_df[self_df$mon=="",6] <- 1
self_df$self_report_date <- dmy(paste(self_df$day,self_df$mon, self_df$year, sep="-"))
# rmove the terms with below 1930 # 
self_df[self_df$self_code_date < dmy("31/12/1929"),] # empty 

#### map the self_df to icd10 ## 
self_code <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/self-report-to-icd10.tsv.gz")
setnames(self_code, "meaning", "ICD_10")
self_icd <- merge(self_df, self_code, by.x="value", by.y="coding",all.x = TRUE)


a <- table(self_icd[is.na(self_icd$ICD_10),]$value) %>% sort %>% as.matrix
b <- cbind(rownames(a), a) %>% as.data.frame
# head(b)
# colnames(b) <- c("coding", "freq")
# # 1482    2416
# # 1385    2597
# # 1651    2614
# # 1196    2760
# # 1202    3092
# # 1353    3156
# # 1078    3645
# # 1374    3774
# # 1242    4079
# # 1297    4198
# # 1406    4315
# # 1197    4402
# # 1415    4511
# # 1436    4655
# # 1386    5512
# # 1113    7289
# # 1287    8163
# # 1294    9036
# # 99999  29640
# # 1065  145310
# sum(tail(a, 20))
## mapping http://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=6 to icd 10 ##
coding6 <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/self-reported-coding-extra.tsv.gz")
icd_ukbb <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/ICD10-coding-UKBB.tsv.gz")
icd_ukbb <- dplyr::filter(icd_ukbb, selectable =="Y")
icd_ukbb

# # need_to_mapped <- coding6[which(coding %in% rownames(a)),]
# need_to_mapped <- merge(b, coding6, ) %>% arrange(desc(freq))
# grep(need_to_mapped[,2], icd_ukbb$meaning, value=TRUE)
# grep("panic attacks", icd_ukbb$meaning, value=TRUE)
# need_to_mapped$freq <- as.numeric(as.character(need_to_mapped$freq))
# need_to_mapped <- arrange(need_to_mapped, desc(freq))

# library("xlsx")
# # Write the first data set in a new workbook
# write.xlsx(need_to_mapped, file = "need_to_map_self_report_code_icd10.xlsx",
#       sheetName = "self_report", append = FALSE)

extra_icd <- read.xlsx("/exeh_4/xyong/project/UKBB/May-26/raw-data/need_to_map_self_report_code_icd10.xlsx") %>% drop_na("ICD-10") ### drop the 99999
setnames(extra_icd, "ICD-10", "ICD_10")
self_code <- rbind(self_code, extra_icd[,c("coding","ICD_10")])

self_icd <- merge(self_df, self_code, by.x="value", by.y="coding")
# fwrite(self_icd, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/self-report-icd-diagnoses-date-June-3.csv.gz")



self_icd <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/self-report-icd-diagnoses-date-June-3.csv.gz")




##### extract the diagnoses code in the GP clinical records #####
#### load the primary clinical dataset ### 
# import the updated version of gp records # updated
setwd("/exeh_4/xyong/project/UKBB/Feb-18-2022/gp-script")


gp_clinical_emis <- fread("covid19_emis_gp_clinical.txt.gz")
gp_clinical_tpp <- fread("covid19_tpp_gp_clinical.txt.gz") ### using the read_ctv3 code 



read_v2_icd10 <- SheetList[["read_v2_icd10"]]; head(read_v2_icd10)
read_ctv3_icd10 <- SheetList[["read_ctv3_icd10"]];head(read_ctv3_icd10)



## get the primary care diagnoses in the gp clinical dataset ## 
gp_clinical_emis_icd10 <- merge(gp_clinical_emis, read_v2_icd10, by.x = "code", by.y = "read_code")
head(gp_clinical_emis_icd10); dim(gp_clinical_emis_icd10) # 0 
## to ctv3 ## 
gp_clinical_emis_icd10_ctv3 <- merge(gp_clinical_emis, read_ctv3_icd10, by.x = "code", by.y = "read_code")
head(gp_clinical_emis_icd10_ctv3); dim(gp_clinical_emis_icd10_ctv3) # 0 
#### the emis do not have the icd 10 code extracted ##### 






##### map to the gp-tpp ## 
gp_clinical_tpp_icd10 <- merge(gp_clinical_tpp, read_v2_icd10, by.x = "code", by.y = "read_code")
head(gp_clinical_tpp_icd10); dim(gp_clinical_tpp_icd10) # 2614889 7
## to ctv3 ## 

gp_clinical_tpp_icd10_ctv3 <- merge(gp_clinical_tpp, read_ctv3_icd10, by.x = "code", by.y = "read_code")
head(gp_clinical_tpp_icd10_ctv3); dim(gp_clinical_tpp_icd10_ctv3) # 28702161       11



gp_clinical_icd10 <- rbindlist(list(gp_clinical_tpp_icd10, gp_clinical_tpp_icd10_ctv3), fill=T)
gp_clinical_icd10 <- gp_clinical_icd10[,c("code","eid","event_dt","code_type","value", "icd10_code")]
length(unique(gp_clinical_icd10$eid)) #185119 #189196 # 189200



#### to get the eariler diagnoses dataset ## 
str(gp_clinical_icd10)
gp_clinical_icd10$event_dt <- dmy(gp_clinical_icd10$event_dt)

## Exclude records with event dates with special values namely: 1901-01-01: "Code has event date before participant's date of birth"
gp_clinical_icd10 <- gp_clinical_icd10 %>% arrange(eid) %>% dplyr::filter(event_dt != "1901-01-01" & event_dt != "1902-02-02" & event_dt != "1903-03-03" & event_dt != "2037-07-07")

summary(gp_clinical_icd10$event_dt)
# "1939-01-01" "2004-11-16" "2010-05-24" "2009-01-09" "2015-08-04" "2021-09-08" 

# fwrite(gp_clinical_icd10, "/exeh_4/xyong/project/UKBB/first-occurrence/raw-icd10/gp_clinical_icd10.txt.gz")
# fwrite(gp_clinical_icd10, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/raw-icd10/gp_clinical_icd10.txt.gz")
# fwrite(gp_clinical_icd10, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_icd10-20220707.txt.gz")
fwrite(gp_clinical_icd10, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_tpp_clinical_icd10-20210908.txt.gz")




##### test for the extraction of the frist diagnoses date in gp_tpp_clinical code ## 

# gp_clinical_icd10_first_diag <- gp_clinical_icd10  %>% 
#   group_by(eid, icd10_code) %>% 
#   summarize(first_diag = min(event_dt)) %>% 
#   arrange(first_diag, .by_group = TRUE) %>%
#   as.data.frame
# head(gp_clinical_icd10_first_diag, 100)
# # fwrite(gp_clinical_icd10_first_diag, "/exeh_4/xyong/project/UKBB/gp_clinical_icd10_first_diag-20220707.csv.gz")

# # cat(names(gp_clinical_tpp_icd10_ctv3), sep="\",\"")





















# https://www.omicsclass.com/article/960
# system("ls")
# /exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/gp_wide/GP-ALL ## with issue date info
# /exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/gp_wide/GP-ATC-DATE-Long ## with the count and first_date, latest_date count. 
# GP_ATC <- fread("/exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/gp_wide/GP-ATC-Wide") ## this the wide info. 
# /exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/GP-DMD-ATC-Sep.atc.csv.gz

# load("/exeh_4/xyong/so/COVID-new/Covid_new/nov-3/df.ah.RData")
# fwrite(df.ah$df.h,"/exeh_4/xyong/so/COVID-new/Covid_new/nov-3/Ori_sglm/df.h")

# https://www.omicsclass.com/article/960 # pls check the dplyr filter and select
# #筛选任何变量＞150的样本
# filter_all(mtcars, any_vars(. > 150)) 
# #筛选变量以“d”结尾，并且变量 "%%2" 等于0
# filter_at(mtcars, vars(starts_with("d")), any_vars((. %% 2) == 0)) 
# # 筛选变量向下取整 == 原变量数值, 并且这部分变量的数值！= 0 的样本集
# filter_if(mtcars, ~ all(floor(.) == .), all_vars(. != 0))



# 
# NOMED_CONCEPTID_NEW


##### get the EMIS clinical local code from emis dataset ##### 
# /exeh_4/xyong/project/UKBB/May-26/raw-data/EMIS_local_clinical codes_coding7689.tsv
emis_local_code <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/EMIS_local_clinical codes_coding7689.tsv")
emis_local_code

head(gp_clinical_emis)
emis3 <- gp_clinical_emis[code_type=="3",] #38804
emis5 <- gp_clinical_emis[code_type=="5",] #4494166

# fwrite(emis3, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_emis_allergy.txt.gz")
# fwrite(emis5, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_emis_test_request.txt.gz")
fwrite(emis3, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/Old-gp_clinical_emis_allergy-20221221.txt.gz")
fwrite(emis5, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/Old-gp_clinical_emis_test_request-20221221.txt.gz")








# get the code of the emis clinical diagonses code 
emis2 <- gp_clinical_emis[code_type=="2",] # 223137610 #103985026
emis2





table(gp_clinical_emis$code_type)
# 29192607 + 14651601   +   4871   +   5661  + 2171766
# 46026506/278302718 
# 9052686/278302718

# 104110970 / 223137610
# 104110970 + 120387815

# test <- emis2[value<0,]
# test[code == '840539006',]  # U071 # COVID-19, virus identified -- with laboratory testing confirmation -- confirmed


gp_clinical_emis_lab_snomed <-  fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_emis_lab_test_res.txt.gz") # 104110970


##### ###### ##### ###### ##### ###### ##### ###### ##### ###### ##### ###### 
# Code that map snomed ct ot icd10 code # 
### try to get the snomed ct to icd 10 ## 
    # snomed <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/snomed/zder2_iisssciRefset_ICD-104thEdition5CharExtendedMapFull_GB1000000_20181001.txt")
    # snomed_extend <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/snomed/zder2_iisssciRefset_ICD-104thEditionExtendedMapFull_GB1000000_20181001.txt")
    # snomed_opcs <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/snomed/xder2_iisssciRefset_OPCS-4.8ExtendedMapFull_GB_20210324.txt")
    # head(snomed_opcs)
    # snomed <- snomed[,c("referencedComponentId", "mapTarget", "mapAdvice")]
    # snomed_extend <- snomed_extend[,c("referencedComponentId", "mapTarget", "mapAdvice")]
    # snomed_opcs <- snomed_opcs[,c("referencedComponentId", "mapTarget", "mapAdvice")]
    # snomed_icd10 <- rbindlist(list(snomed, snomed_extend, snomed_opcs)) %>% unique
    # fwrite(snomed_icd10, "/exeh_4/xyong/project/UKBB/May-26/dmd-code/snomed_ct_to_icd10_opcs.txt.gz")
##### ###### ##### ###### ##### ###### ##### ###### ##### ###### ##### ###### 

#### try to extracted the ICD 10 code in emis #### 
anyNA(gp_clinical_emis_lab_snomed$SNOMED_DESCRIPTION_ID)
# str(gp_clinical_emis_lab_snomed)
#### get the code for lab test results ############ 
gp_clinical_emis_lab_snomed$code <- as.character(gp_clinical_emis_lab_snomed$code)
emis_lab <- merge(emis2, gp_clinical_emis_lab_snomed[,1:7], all.x = TRUE)
emis_lab_matched <- emis_lab[!is.na(emis_lab$SNOMED_DESCRIPTION_ID),]






#### we will extract the diagnoses code from the rest of code in emis. ####
emis_unmatch <- emis_lab[is.na(emis_lab$SNOMED_DESCRIPTION_ID),] # 128321267

snomed_icd10 <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/snomed_ct_to_icd10_opcs.txt.gz")
snomed_icd10 <- snomed_icd10[mapTarget != "#NIS",]


# the code for the covid # 
snomed_covid <- data.frame(referencedComponentId = c("840539006", "840546002"), mapTarget = c("U071","Z208"),  mapAdvice= c("COVID-19, virus identified", "Exposure to SARS-CoV-2"))
snomed_icd10$referencedComponentId <- as.character(snomed_icd10$referencedComponentId)
# # snomed_covid[[3]] <- as.character(snomed_covid[[3]])
snomed_covid_icd10 <- rbind(snomed_icd10, snomed_covid) %>% unique
snomed_covid_icd10 <- snomed_covid_icd10  %>% dplyr::distinct(referencedComponentId,.keep_all = TRUE)



emis_unmatch_icd10 <- merge(emis_unmatch, snomed_covid_icd10, by.x ="code", by.y = "referencedComponentId", all.x=T)
emis_unmatch_icd10_matched <- emis_unmatch_icd10[!is.na(emis_unmatch_icd10$mapTarget),] #47086885

fwrite(emis_unmatch_icd10_matched, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_emis_icd10_diagnoses-accessed-20220218-updated-to-20211112.txt.gz")


























# #### cmp the previous and current results of emis code # 
# # "/exeh_4/xyong/project/UKBB/May-26/raw-data"
# gp_clinical_emis_old <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/Aug-25-update-emis-gp-clinical/covid19_emis_gp_clinical.txt.gz")

# emis2_old <- gp_clinical_emis_old[code_type=="2",] # 223137610 #103985026

# emis_lab_old <- merge(emis2_old, gp_clinical_emis_lab_snomed[,1:7], all.x = TRUE)

# emis_lab_matched_old <- emis_lab[!is.na(emis_lab$SNOMED_DESCRIPTION_ID),] #empty 
# emis_unmatch_old <- emis_lab[is.na(emis_lab$SNOMED_DESCRIPTION_ID),] # 128321267 #103985026

# snomed_icd10 <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/snomed_ct_to_icd10_opcs.txt.gz")
# head(snomed_icd10)
# snomed_icd10 <- snomed_icd10[mapTarget != "#NIS",]
# snomed_covid <- data.frame(referencedComponentId = c("840539006", "840546002"), mapTarget = c("U071","Z208"),  mapAdvice= c("COVID-19, virus identified", "Exposure to SARS-CoV-2"))
# snomed_icd10$referencedComponentId <- as.character(snomed_icd10$referencedComponentId)
# # # snomed_covid[[3]] <- as.character(snomed_covid[[3]])
# snomed_covid_icd10 <- rbind(snomed_icd10, snomed_covid) %>% unique
# snomed_covid_icd10 <- snomed_covid_icd10  %>% dplyr::distinct(referencedComponentId,.keep_all = TRUE)

# emis_unmatch_icd10_old <- merge(emis_unmatch_old, snomed_covid_icd10, by.x ="code", by.y = "referencedComponentId", all.x=T)

# emis_unmatch_icd10_matched_old <- emis_unmatch_icd10_old[!is.na(emis_unmatch_icd10_old$mapTarget),] #47086885


# # tmp <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_emis_no_matched.txt.gz")
# # dim(tmp)
# # useless code in snomed_covid_icd10
# #EPO     3318   Eponym
# #HLT   551664   High level concept
# #NC  69819568   Not classifiable
# #NEW    94440   New concept still to be mapped
# #PT     40709   Partial term
# #NI4                                Not in ICD-10 Fourth Edition
# #AC                                      Awaiting clarification
# #NC MAP SOURCE CONCEPT CANNOT BE CLASSIFIED WITH AVAILABLE DAT
# dplyr::filter(snomed_covid_icd10[,2:3], grepl("#",mapTarget)) %>% unique


# # snomed_covid_icd10[mapTarget=="#EPO",]


# fwrite(emis_unmatch_icd10_unmatched, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_emis_no_matched.txt.gz")

# emis_unmatch_icd10_unmatched <- emis_unmatch_icd10[is.na(emis_unmatch_icd10$mapTarget),]

# emis_unmatch_icd10_unmatched

# head(emis_unmatch_icd10)
# anyNA(emis_unmatch_icd10$mapTarget)
# table(is.na(emis_unmatch_icd10$mapTarget) )


# emis_no_matched <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_emis_no_matched.txt.gz")
# emis_no_matched[code=="1119305005",]
# emis_no_matched[code=="1119305007",]

# # 1144997007
# emis_no_matched[code=="1119303003",]

# gp_clinical_emis[code=="840534001",]
# gp_clinical_emis[code=="1119305005",]
# gp_clinical_emis[code=="1142178009",]

# gp_clinical_emis[code=="65F0.",]
# gp_clinical_emis[code=="EMISNQCO303",]

# grep -E "V1|1155866009|28531000087107|29061000087103|1142182006|1144997007|1144998002|1145034006|1145035007" table_of_emis_code.csv 

# grep -E "189486241000119100|292508471000119105|674814021000119106|688232241000119100|880529761000119102|882784691000119100|138389411000119105|1240581000000104|1240561000000108|1240541000000107|1240531000000103|1240411000000107" table_of_emis_code.csv




# gp_clinical_emis[code=="840539006",] # U071
# head(gp_clinical_emis)

# # 840539006 Disease caused by severe acute respiratory syndrome coronavirus 2 (disorder)	COVID-19	U07.1	COVID-19, virus identified
# # 840546002 Exposure to severe acute respiratory syndrome coronavirus 2 (event)	Exposure to SARS-CoV-2	Z20.8	Contact with and exposure to other communicable disease



# ##### test the tpp rest code ### 

# gp_clinical_icd10 <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_icd10.txt.gz")
# # fwrite(gp_clinical_tpp_lab, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_tpp_lab_test_res.txt.gz")
# gp_clinical_tpp_lab <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_tpp_lab_test_res.txt.gz")
# gp_clinical_icd10$event_dt <- ymd(gp_clinical_icd10$event_dt)
# gp_clinical_tpp_lab$event_dt <- dmy(gp_clinical_tpp_lab$event_dt)
# gp_clinical_tpp$event_dt <- dmy(gp_clinical_tpp$event_dt)
# setnames(gp_clinical_tpp_lab, "SNOMED_TERM", "icd10_code")

# tpp_mapped <- rbind(gp_clinical_icd10[,c("eid", "event_dt", "code_type", "code", "value", "icd10_code")], gp_clinical_tpp_lab[,c("eid", "event_dt", "code_type", "code", "value","icd10_code")]) %>% unique

# fwrite(tpp_mapped, "tpp_matched.csv.gz")

# tpp_all <- merge(gp_clinical_tpp, tpp_mapped, all.x=TRUE)
# gp_clinical_tpp
# tpp_mapped
# table(tpp_all_umatched$icd10_code)


# tpp_all_umatched

# tpp_all_umatched[code=="1119350007",]



# table(is.na(tpp_all$icd10_code))
# tpp_all_umatched <- tpp_all[is.na(tpp_all$icd10_code),]

# unique(tpp_all_umatched)

# fwrite(tpp_all_umatched, "tpp_all_umatched.csv.gz")

# ##### covid related record #### 
# first_dose <- tpp_all_umatched[code=="Y29e7",] # 146462
# fwrite(first_dose, "/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/gp_clinical_tpp_covid-vaccine-first-dose-June-4.csv.gz")

# second_dose <- tpp_all_umatched[code=="Y29e8",] # 25925
# # Y2a0e
# tpp_all_umatched[code=="Y2a0e",] 
# length(unique(second_dose$eid))
