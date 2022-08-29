## import the function ##
source("/exeh_4/xyong/project/R_361_lib.R")
library(XML)
library("methods")
library(data.table)


### the description of the dmd-code ## 
# *xsd - just the description of the amp/ampp/vmp/vmpp
# lookup_v2_3.xsd ingredient_v2_3.xsd # do not have any info 


tmp <- list.files()
tmp
# f_amp2_3051120.xml
# f_ampp2_3051120.xml
# f_gtin2_0051120.xml
# f_ingredient2_3051120.xml
# f_lookup2_3051120.xml
# f_vmp2_3051120.xml
# f_vmpp2_3051120.xml
# f_vtm2_3051120.xml

### parse the AMP dataset ###
result <- xmlParse(file = "/exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/dmd_nov9/f_amp2_3051120.xml")

amp2 <- xmlParse("f_amp2_3200521.xml") 
amp <- xmlRoot(amp2)
str(amp)
AMP <- xmlToList(amp)
APID_VPID <- lapply(AMP[[2]], function(x) as.data.table(as.list(x)))
APID_VPID_NM <- rbindlist(APID_VPID, fill=TRUE) 
APID_VPID_NM_DESC <- APID_VPID_NM[,c("APID","VPID","NM","DESC")] # 143083
dim(APID_VPID_NM_DESC) #144614 
fwrite(APID_VPID_NM_DESC,"/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/AMP-APID_VPID_NM_DESC.csv.gz")

# get the code with AMP-APID-ISID
AMP[[3]][1]
APID_ISID <- lapply(AMP[[3]], function(x) as.data.table(as.list(x))) %>% rbindlist(fill=T)
fwrite(APID_ISID, "/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/AMP-AMP_APID_ISID.csv.gz" )

####### get the ampp dataset ### 
rampp <- xmlParse(file = "f_vmp2_3200521.xml")
AMPP <- xmlToList(rampp)
test <- lapply(AMPP[[2]], function(x) as.data.table(as.list(x)))
test_rbind <- rbindlist(test, fill=TRUE); 
head(test_rbind)
VPID_VTMID_NM <- test_rbind[,c("VPID","VTMID","NM")]
fwrite(VPID_VTMID_NM,"/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/VMP-VPID-VTMID-NM.csv.gz")


# # xmldataframe <- xmlToDataFrame("/exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/dmd_nov9/f_amp2_3051120.xml")
    # # xmldataframe <- xmlToDataFrame("/home/xyong/uk_biobank/data_coding_4_20003/dmd/f_amp2_3100920.xml")
    # rootnode <- xmlRoot(result)
    # str(rootnode[2])
    # AMP <- xmlToList(rootnode)
    # # AMP[[2]] have the APID-VPID-NM-DESC dataset 
    # APID_VPID <- lapply(AMP[[2]], function(x) as.data.table(as.list(x)))
    # APID_VPID_NM <- rbindlist(APID_VPID, fill=TRUE) 
    # APID_VPID_NM_DESC <- APID_VPID_NM[,c("APID","VPID","NM","DESC")] # 143083
    # fwrite(APID_VPID_NM_DESC,"/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/APID_VPID_NM_DESC")

    # AMP[[3]][1]
    # APID_ISID <- lapply(AMP[[3]], function(x) as.data.table(as.list(x))) %>% rbindlist(fill=T)
    # fwrite(APID_ISID, "/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/AMP_APID_ISID" )
    # ####### get the ampp dataset ### 
    # rampp <- xmlParse(file = "/exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/dmd_nov9/f_vmp2_3051120.xml")
    # AMPP <- xmlToList(rampp)
    # test <- lapply(AMPP[[2]], function(x) as.data.table(as.list(x)))
    # test_rbind <- rbindlist(test, fill=TRUE); test_rbind
    # APPID_VPPID_APID_NM <- test_rbind[,c("APPID","VPPID","APID","NM")]
# fwrite(APPID_VPPID_APID_NM,"/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/AMPP_APPID-VPPID-APID-NM")




###### get the f_vmp2_3051120.xml #### 
rvmp <- xmlParse(file = "f_ampp2_3200521.xml")
VMP <- xmlToList(rvmp)

test <- lapply(VMP[[2]], function(x) as.data.table(as.list(x)))
test_rbind <- rbindlist(test, fill=TRUE); test_rbind
head(test_rbind)

VMP_APPID_VPPID_APID_NM <- test_rbind[,c("APPID","VPPID","APID","NM")]
fwrite(VMP_APPID_VPPID_APID_NM, "/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/AMPP_APPID_VPPID_APID_NM.csv.gz")


######## get the VMPP f_vmpp2_3051120.xml
rvmpp <- xmlParse(file = "f_vmpp2_3200521.xml")
VMPP <- xmlToList(rvmpp)
length(VMPP)
VMPP[[2]][1]
test <- lapply(VMPP[[2]], function(x) as.data.table(as.list(x)))
test_rbind <- rbindlist(test, fill=TRUE); test_rbind
head(test_rbind,2)
VMPP_VPPID_VPID_NM <- fwrite(test_rbind[,c("VPPID","VPID","NM")],"/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/VMPP_VPPID_VPID_NM.csv.gz")




# # f_gtin2_0051120.xml # AMPPID and GTINDATA data >>> no use for the decode dmd_code

    # # f_ingredient2_3051120.xml
    # gtin <- xmlParse(file="/exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/dmd_nov9/f_ingredient2_3051120.xml")
    # GTIN <- xmlToList(gtin)
    # length(GTIN)
    # test <- lapply(GTIN, function(x) as.data.table(as.list(x)))
    # test_rbind <- rbindlist(test, fill=TRUE); test_rbind
    # ISID_NM <- test_rbind[,c("ISID","NM")]
    # fwrite(ISID_NM,"/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/ISID-NM")

    # # f_lookup2_3051120.xml   ## no use 
    # # f_vtm2_3051120.xml

    # gtin <- xmlParse(file="/exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/dmd_nov9/f_vtm2_3051120.xml")
    # GTIN <- xmlToList(gtin)
    # length(GTIN)
    # test <- lapply(GTIN, function(x) as.data.table(as.list(x)))
    # test_rbind <- rbindlist(test, fill=TRUE); test_rbind
    # VTMID_NM <- test_rbind[,c("VTMID", "NM")]
# fwrite(VTMID_NM, "/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/VTMID-NM")


# /exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/dmds_nov9 
# the bnf.xml have the atc code # 
system("ls dmd_nov9")
bnf <- xmlParse(file="/exeh_4/xyong/project/UKBB/May-26/dmd-code/f_bnf1_0200521.xml")
bnf_list <- xmlToList(bnf)
str(bnf_list)
bnf_list[[3]]
bnftable <- lapply(bnf_list[[2]], function(x) as.data.table(as.list(x)))
bnftable_fill <- rbindlist(bnftable, fill=TRUE) # the bnftable_fill have the VPID to ATC code
bnftable
head(bnftable_fill)
dim(bnftable_fill)
str(bnftable_fill)
bnf_no_NA <- bnftable_fill[!bnftable_fill$BNF=="NA",]
bnf_no_NA # 5232
bnf_no_NA_ATC <- bnf_no_NA[!bnf_no_NA$ATC == "n/a", ] 
## to get the ATC and BNF ## 
bnf_no_NA_ATC <- bnf_no_NA_ATC[,c("ATC", "BNF")] %>% unique
bnf_no_NA_ATC # 1784
nchar(bnf_no_NA_ATC[2,2])

bnf_no_NA_ATC[BNF == "10010100",]
table(bnf_no_NA_ATC$BNF) %>% sort %>% as.matrix %>% head

fwrite(bnftable_fill, "/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/VPID-BNF-ATC.csv.gz")
bnftable_fill <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/VPID-BNF-ATC.csv.gz")

bnf_atc

VPID_ATC <- bnftable_fill[!bnftable_fill$ATC=="n/a",]
VPID_ATC


###### map all the dmd_code to ATC code ### 
VPID - ATC 
APID - VPID - ATC
APPID - APID - VPID - ATC
VPPID - VPID - ATC 
####### map all the dmd_code to ATC code #### 
rm(list=ls())
setwd("/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1")
tmp <- list.files();tmp

bnftable_fill <- fread("/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-1/VPID-BNF-ATC.csv.gz")
vpid_atc <- bnftable_fill[!bnftable_fill$ATC=="n/a",]
vpid_atc <- vpid_atc[,c("VPID", "ATC", "BNF")]
vpid_atc


# AMP-AMP_APID_ISID.csv.gz no use 
amp <- fread("AMP-APID_VPID_NM_DESC.csv.gz")
amp_apid_atc <- merge(amp, vpid_atc, all = T)
head(amp_apid_atc)


vmp <- fread("VMP-VPID-VTMID-NM.csv.gz")
vmp_apid_atc <- merge(vmp, vpid_atc, by= "VPID", all=T)
# vmp_apid_atc[is.na(vmp_apid_atc$ATC)]
head(vmp_apid_atc)
vmp_apid_atc

ampp <- fread("AMPP_APPID_VPPID_APID_NM.csv.gz")
ampp_amp_apid_atc <- merge(ampp, amp_apid_atc, by="APID", all=T)
head(ampp_amp_apid_atc)
setnames(ampp_amp_apid_atc, "NM.x", "NM")

vmpp <- fread("VMPP_VPPID_VPID_NM.csv.gz")
vmpp_vpid_atc <- merge(vmpp, vpid_atc, by="VPID", all=T)

vmpp_vpid_atc
a <- vpid_atc$VPID
b <- vmpp_vpid_atc$VPID
table(a %in% b) #  TRUE  13040 
table(b %in% a) 
# FALSE  TRUE 
# 14187 18823
##### Hence using the vpid in vmpp_vipd_atc stands for the vpid in vmpp_vpid_atc


# b(which(b %in% a)==FALSE)
all_map <- list(vpid = vmp_apid_atc[,c("VPID", "NM", "ATC", "BNF")], amp = amp_apid_atc[,c("APID", "NM", "ATC", "BNF")], vmp = vmp_apid_atc[,c("VTMID", "NM", "ATC", "BNF")], ampp = ampp_amp_apid_atc[,c("APPID", "NM", "ATC", "BNF")], ampp1 = ampp_amp_apid_atc[,c("VPPID", "NM", "ATC", "BNF")], vmpp = vmpp_vpid_atc[,c("VPPID", "NM", "ATC", "BNF")])

lapply(all_map, names)
save(all_map, file= "All-list-vpid-apid-vtmid-appid-vppid-vppid.RData")
all_map1 <- lapply(all_map, function(x){
    colnames(x) <- c("dmd_code", "NM", "ATC", "BNF")
    x})
all_map <- rbindlist(all_map1, fill=TRUE)
all_map <- unique(all_map)
dim(all_map)
a <- head(all_map)
dim(all_map)
apply(all_map, 2 ,function(x){
    prop_miss_case(as.data.frame(x))   
})
save(all_map, file= "All-df-dmd_code-NM-ATC-BNF.RData")
# /exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-2
dmd_code_atc <- all_map[,c("dmd_code", "ATC")] %>% drop_na(ATC) %>% unique() 
dmd_code_atc
table(all_map$BNF %in% all_map$dmd_code) # FALSE

bnf_atc <- all_map[,c("BNF", "ATC")] %>% drop_na(ATC) %>% unique() 
dim(bnf_atc)
head(bnf_atc)
setnames(bnf_atc, "BNF", "dmd_code")
dmd_code_bnf_atc <- rbind(dmd_code_atc, bnf_atc) %>% drop_na(dmd_code) %>% unique

dim(dmd_code_bnf_atc)

apply(dmd_code_bnf_atc, 2 ,function(x){
    prop_miss_case(as.data.frame(x))   
})

head(dmd_code_bnf_atc)
fwrite(dmd_code_bnf_atc, "/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-2/dmd_code_bnf_atc_29-May.csv.gz")

emis_local <- fread("/exeh_4/xyong/so/COVID-new/Covid_new/nov-3/gpatc/EMIS-Local-ATC.csv")
emis_local
setnames(emis_local, c("coding", "atc"),c("dmd_code", "ATC"))
dmd_code_bnf_atc$dmd_code <- as.character(dmd_code_bnf_atc$dmd_code)
dmd_bnf_emis_atc <- rbind(dmd_code_bnf_atc, emis_local[,c("dmd_code", "ATC")]) %>% drop_na(dmd_code) %>% unique
head(dmd_bnf_emis_atc)
fwrite(dmd_code_bnf_atc, "/exeh_4/xyong/project/UKBB/May-26/dmd-code/tidyed-2/dmd_bnf_emis_atc_29-May.csv.gz")
