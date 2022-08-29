source("/exeh_4/xyong/project/R_361_lib.R")
system("ls")
library(venn)
rm(list=ls())
# setwd("/exeh_4/xyong/project/UKBB/May-26/raw-data")
# setwd("/exeh_4/xyong/project/UKBB/May-26/raw-data/covid-test-Jun-30")
# setwd("/exeh_4/xyong/project/UKBB/May-26/raw-data/covid-test-Jul-15")
# setwd("/exeh_4/xyong/project/UKBB/May-26/raw-data/covid-test-Aug-26")
# setwd("/exeh_4/xyong/project/UKBB/May-26/raw-data/covid-test-Aug-26/Aug-27")
setwd("/exeh_4/xyong/project/UKBB/May-26/raw-data/covid-test-Aug-26/Dec-22")

# load the covid-19 results of being tested # 
tmp <- list.files();tmp
tmp <- grep("result", tmp, value=TRUE)
tested <- lapply(tmp, fread)
names(tested) <- tmp

    # tested[["covid19_result_england.txt.gz"]]
    # a <- table(tested[["covid19_result_england.txt.gz"]][,c(6,9)])
    # dim(a)
    # a <- as.matrix(a)
    # a
    # prop_pos <- tapply(tested[["covid19_result_england.txt.gz"]][[6]], tested[["covid19_result_england.txt.gz"]][[9]], mean)
    # prop_pos <- round(b, 2)
    # total <- a[1,] + a[2,]
    # c <- rbind(a,total)
    # c <- rbind(c, prop_pos)

    # table(tested[["covid19_result_england.txt.gz"]]$reqorg) # 1 44304
    # table(tested[["covid19_result_scotland.txt.gz"]]$factype) # 3 11527
    # table(tested[["covid19_result_wales.txt.gz"]]$pattype) # 7 7864
    # table(tested[[4]]$reqorg) # 1 39056


    # table(tested[["covid19_result_england.txt.gz"]]$origin) # 1 110707 

    # tested[["covid19_result_england.txt.gz"]][eid=="1003661",]
    # tested[["covid19_result_wales.txt.gz"]][eid=="1003661",]

    # table(tested[["covid19_result_wales.txt.gz"]]$eid %in% tested[["covid19_result_england.txt.gz"]]$eid) # FALSE  TRUE 6928  1981

    # lapply(tested, names)
# names(tested)

# merge all the tested subjects into one single file ##### 
#### make sure the inpatient test in scotland with factype == 3 #### 
tested[["covid19_result_scotland.txt.gz"]]$origin <- ifelse(tested[["covid19_result_scotland.txt.gz"]]$factype == 3, 1, 0)
## convert the tested subjects within wales into origin 1(A&E), 6(ICU) and 7(in hospital) -> origin 1 ## 
tested[["covid19_result_wales.txt.gz"]]$origin <- ifelse(tested[["covid19_result_wales.txt.gz"]]$pattype == 1|tested[["covid19_result_wales.txt.gz"]]$pattype == 6|tested[["covid19_result_wales.txt.gz"]]$pattype == 7, 1, 0)
# tested[["covid19_result_wales.txt.gz"]][tested[["covid19_result_wales.txt.gz"]]$pattype == 6,]
table(tested[["covid19_result_wales.txt.gz"]]$origin)
# just keep the c("eid", "specdate", "origin", "result")
tested_select <- lapply(tested, function(x){x <- x[, c("eid", "specdate", "origin", "result")]}) %>% rbindlist %>% unique
tested_select
table(tested_select[,3:4]) 
length(unique(tested_select$eid)) # 94968 -June-14 # 117671 -> 13-July
                                  # 120932 -> Aug-28 
                                  # 168361 -> update till to Oct-18 -> Dec-22



    #       result
    # origin      0      1
    #      0  61300  16319
    #      1 121289   6223

# The infection update to Aug-28
    #  result
# origin      0      1
    #  0  95164  18106
    #  1 168158   6583

    # a <- tested_select[origin==0 & result ==1,]
    # b <- tested_select[origin==1,]

    # # table(a$eid %in% tested_select$eid)
    # length(unique(a$eid)) #14703
    # length(unique(b$eid))
# head(tested_select)


# summary info update till to Oct-18 
    #   result
# origin      0      1
    #  0 162442  30674
    #  1 223509   8263




tested_select$specdate <- dmy(tested_select$specdate)
tested_select <- arrange(tested_select, specdate)
tested_select
summary(tested_select$specdate)

        # Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-01-13" "2020-12-20" "2021-05-01" "2021-04-05" "2021-08-08" "2021-10-18" 



########### ########### ########### ########### ########### ########### 
########### Treat the test after the end of FU date as the censoring  ###################
########### Trunct the test after the end of FU date ###################

# tested_select[specdate >= ymd("2021/05/07"),] # 71499
# tested_select <- tested_select[specdate < ymd("2021/05/07"),]

# the primary hosipitalization record were set at the Sep 30 # Hence the test after Sep 30 were left as untested 
test_after_end_FU_date <- tested_select[specdate > ymd("2021/09/30"),] #  2957
# test_after_end_FU_date[duplicated()]
# duplicated(test_after_end_FU_date$eid)
table(test_after_end_FU_date[,3:4]) 
tested_select <- tested_select[specdate <= ymd("2021/09/30"),]


summary(tested_select$specdate)
# "2020-01-13" "2020-12-11" "2021-04-18" "2021-03-24" "2021-07-23" "2021-09-30"

###### get the max_spec_date ##### 
max_spec_date <- tested_select[,c("eid", "specdate")] %>% arrange(desc(specdate)) %>% distinct(eid, .keep_all=TRUE) %>% rename(,max_spec_date = "specdate")
max_spec_date
# fwrite(max_spec_date,  "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-Cohort-info-max-spec-date-Jul-12.csv.gz")
# fwrite(max_spec_date,  "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-Cohort-info-max-spec-date-Aug-26.csv.gz")
# fwrite(max_spec_date,  "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-Cohort-info-max-spec-date-Aug-28.csv.gz")





summary(tested_select$specdate)
        # Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-01-13" "2020-10-14" "2021-01-09" "2020-12-27" "2021-03-25" "2021-06-14" 
# "2020-01-13" "2020-10-27" "2021-01-31" "2021-01-17" "2021-04-27" "2021-07-12" -> Jul-12
# "2020-01-13" "2020-11-01" "2021-02-08" "2021-01-24" "2021-05-06" "2021-08-18" -> Aug-25
# "2020-01-13" "2020-11-01" "2021-02-08" "2021-01-24" "2021-05-06" "2021-08-18"  -> Aug-18
# "2020-01-13" "2020-12-11" "2021-04-18" "2021-03-24" "2021-07-23" "2021-09-30" -> Dec-22
# tested_select[tested_select$eid=="4388657",]

# Pls update this parts when new diagnoses dataset were avaliable ##
# Pls update this parts when new diagnoses dataset were avaliable ## 

# extracted the diagnoses in the clinical and hesin data ## 
# extracted the diagnoses in the clinical and hesin data ## 

# Pls update this parts when new diagnoses dataset were avaliable ## 
# Pls update this parts when new diagnoses dataset were avaliable ## 


dia <- fread("/exeh_4/xyong/project/UKBB/May-26/ukbb-clinical-data/health-outcome-June-4/UKBB-health-outcome-icd10-rm-invalid-code-date.csv.gz")
# https://www.who.int/standards/classifications/classification-of-diseases/emergency-use-icd-codes-for-covid-19-disease-outbreak
# U071 
covid_dia <- subset(dia, diag_icd10=="U071" | diag_icd10 == "U072") %>% unique
covid_dia$diagnoses_date <- ymd(covid_dia$diagnoses_date)
# exculded the U071/U072 in the old version of code #### before the 2020-01-13
# https://www.oecd-ilibrary.org/docserver/9789264270985-24-en.pdf?expires=1625150334&id=id&accname=ocid177302&checksum=202CE471A2A0BDBC43F1835299BE7849
covid_dia <- covid_dia[diagnoses_date >= ymd("2020/01/13"),]
summary(covid_dia$diagnoses_date)
        # Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-01-20" "2020-05-03" "2020-11-11" "2020-09-27" "2021-01-08" "2021-03-20" 
covid_dia[eid=="4388657",]
covid_dia

table(covid_dia[,c("diag_icd10", "source")]) 
# diag_icd10 death_cause emis hesin
    #   U071        1158    2  6869
    #   U072          23   15   651
table(covid_dia[,"diag_icd10"]) # 
# U071 U072 
# 8026  678 
covid_dia[source=="death_cause",]$eid
covid_dia <- covid_dia %>% mutate(
    specdate = diagnoses_date,
    origin = 1,
    result = case_when(
        diag_icd10 == "U071"  ~ "1",
        diag_icd10 == "U072"  ~ "0"
    )
)
# fwrite(covid_dia, "/exeh_4/xyong/project/UKBB/May-26/raw-data/covid-test-Jul-15/covid-dia-inhospital.csv.gz")


### use the inhospital diagnoses of U071 version updated Jul-15 ##### 
covid_dia <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/covid-test-Jul-15/covid-dia-inhospital.csv.gz")
covid_dia
covid_dia$specdate <- ymd(covid_dia$specdate)
covid_dia_res <- covid_dia[,c("eid", "specdate", "origin", "result")]
tested_hos <- rbind(tested_select, covid_dia_res) %>% unique


tested_hos$result <- as.numeric(tested_hos$result)
anyNA(tested_hos$result)
# 281810 - 274986

### treat the na in result to NA ### 
# tested_hos[is.na(result),"result"] <- 0
death_due_to_covid <- covid_dia[diag_icd10=="U071"&source=="death_cause",]
#### get the death_record ## 
death_record <- dia[, c("eid", "diagnoses_date", "source")] %>% dplyr::filter(source == "death_cause") %>% unique
### merge the death_record with being tested res with the tested_hos ### 
tested_hos_death <- merge(tested_hos, death_record[,c("eid", "diagnoses_date")], all.x = TRUE)

str(tested_hos_death)
tested_hos_death$diagnoses_date <- ymd(tested_hos_death$diagnoses_date)
tested_hos_death[!is.na(diagnoses_date),]


## mutate and creat the survival of status at being tested time #### 
tested_hos_death_1 <-  tested_hos_death %>% mutate(
    survival_at_tested = case_when(
        is.na(diagnoses_date) ~ "alive",
        # specdate < diagnoses_date ~ "alive",
        result == 0 & specdate < diagnoses_date ~ "alive",
        result == 0 & specdate >= diagnoses_date ~ "death_U072",
        result == 1 & diagnoses_date - specdate <= 30 ~ "death_U071", ## subjects (without U071 death record) dead within 1 month after the ve-pos diagnoses were classfied into death due to covid group 
        result == 1 & diagnoses_date - specdate > 30 ~ "alive",
        # specdate >= diagnoses_date ~ "death" # dead due to the U071 (with the death record were classfied into the death due to covid group)
    ) #,
#     death_due_to_covid = case_when(
#         eid %in% death_due_to_covid$eid ~ "death_U071"
#     )
)

# table(is.na(tested_hos_death$diagnoses_date))
setnames(tested_hos_death_1, "diagnoses_date", "death_date") # the death_date is the diagnoses date with U071 within the death record 
tested_hos_death_1[!is.na(tested_hos_death_1$result1),]
table(tested_hos_death_1$survival_at_tested)

length(unique(tested_hos_death_1[result==1,]$eid)) # ve-pos: 17840  19284 -> Jul-12
length(unique(tested_hos_death_1[result==0,]$eid)) # ve-neg: 82483  104845 -> Jul-12
tested_hos_death_1

a <- tested_hos_death_1 # %>% group_by(eid)
b <- a %>% mutate(
    result1 = case_when(
        survival_at_tested == "death_U071" ~ 3,
        # death_due_to_covid == "death_U071" ~ 3,
        survival_at_tested != "death_U071" ~  result + origin 
    )
)
b[is.na(b$result1),]

table(b[survival_at_tested=="death", 'result'])
c <- b[survival_at_tested=="death" & result == 0, ]
table(c$death_due_to_covid)
### for subject with death with U071 record but the test results at the death date equal to 0 treat as 1 ### because the some subjects with neg-test record but death due to U071 ##### 
# b[survival_at_tested=="death", 'result'] <- 1
# sum(table(b$result1))
# table(b[survival_at_tested == "death",]$result1)
b1 <- b  %>% 
    mutate(
        group_at_test = case_when(
            result1 == 3 ~ "ve-pos-death",
            result1 == 2 ~ "ve-pos-inpatient",
            result1 == 1 & result == 1 ~ "ve-pos-outpatient",
            result1 == 1 & result == 0 ~ "ve-neg",
            result1 == 0 ~ "ve-neg"
            ),
        result2 = case_when(
            group_at_test == "ve-pos-death" ~ 4,
            group_at_test == "ve-pos-inpatient" ~ 3,
            group_at_test == "ve-pos-outpatient" ~ 2,
            group_at_test == "ve-neg" ~ 1
        )
    ) 
table(b1$result2)
table(b$result1) %>% sum



covid_cat <-    b1 %>% group_by(eid) %>% summarize(
        chort_cat = case_when(
            max(result2) == 4 ~ "ve-pos-death",
            max(result2) == 3 ~ "ve-pos-inpatient",
            max(result2) == 2 ~ "ve-pos-outpatient",
            max(result2) == 1 ~ "ve-neg"
        )
    ) %>% data.frame
table(covid_cat$chort_cat)
dim(covid_cat)

b1_chort <- merge(b1, covid_cat, all.x=TRUE)
head(b1_chort)
table(b1_chort[,c("group_at_test", "chort_cat")])
# fwrite(b1_chort, "/exeh_4/xyong/project/UKBB/May-26/covid-cohort/COVID-19-tested-basic-info-26-May.csv.gz")
# fwrite(b1_chort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-tested-basic-info-Jun-30.csv.gz")


b1_chort <-  b1_chort %>% dplyr::filter(
    !((eid =="1910846" & specdate == "2020-01-30") | 
    (eid=="5951815" & specdate == "2020-01-13") | 
    (eid=="2031111" & specdate == "2020-01-20"))
)
# fwrite(b1_chort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-tested-basic-info-JuL-15.csv.gz")
# fwrite(b1_chort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-tested-basic-info-Aug-27.csv.gz")
# fwrite(b1_chort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-tested-basic-info-test-after-7-May-were-censored-Aug-27.csv.gz")

# fwrite(b1_chort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-tested-basic-info-test-after-25-July-were-censored-Sep-24.csv.gz")
fwrite(b1_chort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-tested-basic-info-test-after-30-Sep-were-censored-Dec-22.csv.gz")




##### create the index_date for the cox_model and time_varying model ##### 

ve_pos <- b1_chort[which(b1_chort$chort_cat != "ve-neg" & b1_chort$result == 1),] %>% 
            group_by(eid) %>% 
            summarize(
                index_date_cox = min(specdate),
                index_date_time = max(specdate)
            ) %>% data.frame %>% unique

    # ve_pos1 <- b1_chort[which(b1_chort$chort_cat != "ve-neg" ),] %>% 
    #             group_by(eid) %>% 
    #             summarize(
    #                 index_date = min(specdate)
    #             ) %>% data.frame %>% unique
    # dim(ve_pos1)
    # setdiff(ve_pos1$eid, ve_pos$eid)
    # b1_chort[eid=="1144793", ]
ve_neg <- b1_chort[which(b1_chort$chort_cat == "ve-neg"),] %>% 
            group_by(eid) %>% 
            summarize(
                index_date_cox = max(specdate),
                index_date_time = max(specdate)
            ) %>% data.frame %>% unique

ve <- rbind(ve_pos, ve_neg)
table(covid_cat$chort_cat)
table(covid_cat$chort_cat) %>% sum # 107647 # 117901 -> Jul 12
        #    ve-neg      ve-pos-death  ve-pos-inpatient ve-pos-outpatient 
            # 77136              1492              4238             12344 
          #  ve-neg      ve-pos-death  ve-pos-inpatient ve-pos-outpatient 
            # 89172              1496              4371             12608 
        #   101594              1253              4524             13791 
dim(covid_cat) # 95210  # 107647    121162
covid_cat_index <- merge(ve, covid_cat) %>% arrange(desc(index_date_time))
summary(covid_cat_index$index_date)
head(covid_cat_index)
dim(covid_cat_index)
##### import the data for all the covid misc ### 
misc <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/covid19_misc.txt.gz")
## exculded the death subjects without U071/U072 diagnoses ###
death <- fread("/exeh_4/xyong/project/UKBB/May-26/raw-data/death.txt.gz")
death_without_U071 <- setdiff(death$eid, covid_dia$eid)
alive_untested <- setdiff(misc$eid, death_without_U071)
untested <- data.frame(eid= setdiff(alive_untested, covid_cat_index$eid),
                        # index_date = ymd("2021/05/06"), 
                        # index_date = ymd("2021/06/14"), 
                        # index_date_cox = ymd("2021/08/18"),
                        index_date_cox = ymd("2021/09/30"),
                        index_date_time = ymd("2021/09/30"),
                        chort_cat = "untested")
covid_cohort <- rbind(covid_cat_index, untested)
setnames(covid_cohort, "chort_cat", "cohort_cat")
head(covid_cohort)
# dim(covid_cohort)

table(covid_cohort$cohort_cat) # 460229 #  460929  -> Jul 12
# untested            ve-neg      ve-pos-death  ve-pos-inpatient ve-pos-outpatient 
# 365019             77136              1492              4238      12344
# Jun-30
# 352973             89172              1496              4371      12608 
# Jul-12
# 343028             98617              1253              4474      13557

# till Sep-30
# 302550            131177              1262              5554      21553 
#    1492         +     4238    +  12344
#    1496       +       4371     + 12608 
# str(untested)
    # str(covid_cat_index)

# 362172           80158              1251            4365        12376


    # # length(covid_cohort[covid_cohort==1,]$eid)
    # input  <-list(death = unique(a$eid), death_but_alive = unique(b$eid))
    #     # ve_neg=unique(covid_cohort[covid_cohort==0,]$eid), ve_pos_out=unique(covid_cohort[covid_cohort==1,]$eid), ve_pos_both_io=unique(covid_cohort[covid_cohort==2,]$eid),ve_pos_in=unique(covid_cohort[covid_cohort==3,]$eid), ve_pos_death=unique(covid_cohort[covid_cohort==4,]$eid))


    # venn(input)
    # # pdf('rplot.pdf',width=6, height=3)
    # # manhattan(gwasResults, chr="CHR", bp="BP", snp="SNP", p="P" )
    # dev.off()



    # # table(unique(sort(tested_hos_death$eid)) %in% unique(dia[source == "death_cause", ]$eid))
    # # FALSE  TRUE 
    # # 91335  3881


    # tested_hos
    # str(tested_select)
    # %>% unique



    # tmp <- tested_select[eid=="5951815", ]
    #     #         eid   specdate origin result
    #     #  1: 5951815 2020-01-13      0      1
    #     #  2: 5951815 2020-08-04      1      0
    #     #  3: 5951815 2020-11-12      1      0
    #     #  4: 5951815 2020-11-18      1      0
    #     #  5: 5951815 2020-11-19      1      0
    #     #  6: 5951815 2020-11-25      1      0
    #     #  7: 5951815 2020-12-18      0      1
    #     #  8: 5951815 2020-12-19      1      1
    #     #  9: 5951815 2021-01-03      1      1
    #     # 10: 5951815 2021-01-06      1      1
    #     # 11: 5951815 2021-01-10      0      1
    #     # 12: 5951815 2021-01-16      1      1
    #     # 13: 5951815 2021-01-18      1      1
    #     # 14: 5951815 2021-01-21      1      1
    #     # 15: 5951815 2021-01-25      1      1
    #     # 16: 5951815 2021-01-26      1      1
    #     # 17: 5951815 2021-01-29      1      1
    #     # 18: 5951815 2021-02-08      0      1
    #     # 19: 5951815 2021-02-15      0      1
    # tmp

    # ve_pos <- tested_select[result =="1", ]
    # summary(ve_pos$specdate)
    # # for subjects with in and out hospital record, the earliest postive date were treated as the diagnoses date. 
    # ve_neg <- tested_select[result =="0", ]
    # length(unique(ve_pos$eid)) # 17581
    # length(unique(ve_pos[origin==1,]$eid)) # 4698
    # length(unique(ve_pos[origin==0,]$eid)) # 14703
    # ve_pos_in <- ve_pos[origin=="1",]
    # ve_pos_out <- ve_pos[origin=="0",]



    # ## get ve_pos and outpatient ## 
    # ve_pos_out_def <- ve_pos_out[which(ve_pos_out$eid %in% setdiff(ve_pos_out$eid, ve_pos_in$eid)),] # 12883
    # ve_pos_in_def <- ve_pos_in[which(ve_pos_in$eid %in% setdiff(ve_pos_in$eid, ve_pos_out$eid)),] # 2878
    # overlap #  1820
    # ve_pos_in_def
    # # 12883 + 2878 + 1820
    # overlap_eid <- intersect(ve_pos_in$eid, ve_pos_out$eid) #  1820
    # overlap <- ve_pos[which(ve_pos$eid %in% overlap_eid),]
    # overlap <- arrange(overlap, origin) %>% arrange(eid)

    # ve_neg 
    # ve_neg_def <-  ve_neg[which(ve_neg$eid %in% setdiff(ve_neg$eid, ve_pos$eid)), ] 
    # # length(unique(ve_neg_def$eid))


    # length(unique(ve_pos_in_def$eid))
    # ve_pos_out_def <- arrange(ve_pos_out_def, specdate)
    # a <- ve_pos_out_def[duplicated(ve_pos_out_def$eid),]
    # ve_pos_out_def$origin

    # ## creat covid-19 cat ## tested_subjects # 94968
    # # 0 -> ve_neg_def # 77387
    # # 1 -> ve_pos_outpatient # 12883
    # # 2 -> ve_pos_with_in_and_out_record # 1820
    # # 3 -> ve_pos_inpatient # 2878
    # #### define the covid-19 cat ## 
    # ve_neg_def$covid_cat <- 0
    # ve_pos_out_def$covid_cat <- 1 
    # overlap$covid_cat <- 2
    # ve_pos_in_def$covid_cat <- 3
    # covid_cat <- rbindlist(list(ve_neg_def, ve_pos_out_def, overlap, ve_pos_in_def))
    # covid_cat
# fwrite(covid_cohort, "/exeh_4/xyong/project/UKBB/May-26/covid-cohort/COVID-19-Cohort-info-26-May.csv.gz")
# fwrite(covid_cohort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-Cohort-info-Jun-30.csv.gz")
# fwrite(covid_cohort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-Cohort-info-Jul-12.csv.gz")
# fwrite(covid_cohort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-Cohort-info-Aug-26.csv.gz")
# a <- dplyr::filter(covid_cohort, cohort_cat=="untested")
# covid_cohort <- covid_cohort %>% mutate(
#     index_date_cox = case_when(
#         cohort_cat == "untested" ~ ymd("2021/08/18"),
#         cohort_cat != "untested" ~ ymd(index_date_cox),
#     ) ,
#     index_date_time = case_when(
#         cohort_cat == "untested" ~ ymd("2021/08/18"),
#         cohort_cat != "untested" ~ ymd(index_date_time),
#     )      
# )

# fwrite(covid_cohort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-Cohort-info-test-after-7-May-were-censored-Aug-27.csv.gz")


# fwrite(covid_cohort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-Cohort-info-test-after-25-July-were-censored-Sep-24.csv.gz")
fwrite(covid_cohort, "/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-Cohort-info-test-after-30-Sep-were-censored-Dec-22.csv.gz")

head(covid_cohort)

covid_cohort
# summary(covid_cohort$index_date_time)


# tmp <- fread("/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-19-Cohort-info-test-after-25-July-were-censored-Sep-24.csv.gz")

# summary(tmp$index_date_cox)


# head(covid_cohort)



### plot the overlap test in the overall being tested pop #### 
# length(covid_cohort[covid_cohort==1,]$eid)
# pdf("/exeh_4/xyong/project/UKBB/May-26/covid-cohort/COVID-cohort.pdf")
# pdf("/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-cohort.pdf")
# pdf("/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-cohort-Jul-12.pdf")
pdf("/exeh_4/xyong/project/UKBB/Jun-30/covid-cohort/COVID-cohort-Aug-26.pdf")


input  <-list(untested=unique(covid_cohort[covid_cohort$cohort_cat=="untested",]$eid), ve_neg=unique(covid_cohort[covid_cohort$cohort_cat=="ve-neg",]$eid), ve_pos_out=unique(covid_cohort[covid_cohort$cohort_cat=="ve-pos-outpatient",]$eid), ve_pos_in=unique(covid_cohort[covid_cohort$cohort_cat=="ve-pos-inpatient",]$eid), ve_pos_death=unique(covid_cohort[covid_cohort$cohort_cat=="ve-pos-death",]$eid))


venn(input, zcol = "#ffdd77, #bb2020, #1188cc, blue, red")
# pdf('rplot.pdf',width=6, height=3)
# manhattan(gwasResults, chr="CHR", bp="BP", snp="SNP", p="P" )
dev.off()
