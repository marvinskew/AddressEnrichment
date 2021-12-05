
##############################################################################################################
# Script Name: 1_pull_data.R
#
# Purpose: This updates the R data version of the negative labs dataset and codes various indicator variables
#          needed for the project to run properly. This is needed to find new records needing enrichment.
#          All the data cleaning of the negative labs dataset prior to enrichment is performed by this script.
#
# Date Created :07/06/2021
##############################################################################################################

options(digit= 20)
pkges<-c("tidyverse","data.table","lubridate", "devtools","forcats", "gridExtra","readxl",
         "stringr","tibble", "tidyverse", "readr")
for(pkge in pkges){
  if(!pkge %in% installed.packages())install.packages(pkge, dependencies = TRUE)
}
lapply(pkges, require, character.only= TRUE)

start_time1= proc.time()

#set the working directory
setwd(working_dir)

#read in relevant data files
testingSite_Addr<- read_xlsx(paste0(testingSiteAddr_fpath,"/",testSiteAddr_fileName), sheet= 3)
fips_county_to_code<-read.csv(paste0(file_path,"/",fips_fileName), sep=",", header= T)
city_county_crswalk<- read_xlsx(paste0(getwd(),"/","data","/",city_to_cnty_xwalk_fileName), sheet= 5)
wa_fips_code<- read.csv(paste0(wa_fips_filepath,"/",wa_fips_codes), sep=",", header= T)

wa_fips_code=wa_fips_code %>%
               dplyr::mutate(countyName= toupper(str_trim(gsub("County", "", County), side="both")))
#DOH_FULL_{current_date}.csv
listdir_path<-list.files(doh_path, pattern="^DOH_FULL_" ,full.names=TRUE)%>%
  enframe(name= NULL)%>%
  bind_cols(pmap_df(., file.info))%>%
  filter(mtime== max(mtime))%>%
  pull(value)
nl <- enrich_nl_db_check_csv(listdir_path)

nl_vars <- c('neg_pid','WELRS_OBX_ID', 'MSH_ID','EXTERNAL_ID', 'FIRST_NAME','WELRS_ASSIGN_ADDR_TYPE', "WELRS_ASSIGNED_COUNTY",
             'LAST_NAME','PATIENT_MIDNAME', 'PATIENT_DATE_OF_BIRTH','PATIENT_ADMINISTRATIVE_SEX', 'PATIENT_PHONE_NUM',
             'PATIENT_ADDRESS_1', 'PATIENT_ADDRESS_2', 'PATIENT_ADDRESS_CITY','PATIENT_ADDRESS_STATE', 'PATIENT_ADDRESS_ZIP',
             'PATIENT_ADDRESS_COUNTY','ACCOUNTABLE_COUNTY', 'PROCESSED_DATE','isin_WDRS')
#nl[ ,colnames(nl) %in% nl_vars]
nl<-nl%>%
     dplyr::mutate(WELRS_OBX_ID= as.character(WELRS_OBX_ID),
                  MSH_ID= as.character(MSH_ID))
nl$ORIG_ACCOUNTABLE_COUNTY<- nl$ACCOUNTABLE_COUNTY

#Testing Site Addresses
testingSite_Addr<-testingSite_Addr%>%
              dplyr::mutate(REPORTING_CITY= c("Bellevue", "Auburn", "Seattle", "Seattle", "Seattle", "Monroe"))

county_to_code= fips_county_to_code %>%
  dplyr::mutate(Description3= str_trim(toupper(Description3), side= "left"),
                 fips_code= ifelse(substr(ReferenceCode,1,2)=="53",
                                           paste0("WA-",substr(ReferenceCode,4,5)), ReferenceCode),
                fips_code= ifelse(substr(fips_code,1,2) %in% c("WA", "53"), fips_code, ""))%>%
          distinct()

city_county_crswalk<-city_county_crswalk%>%
                         dplyr::mutate(PLACE_NAME= toupper(PLACE_NAME),
                                       COUNTY_NAME= toupper(COUNTY_NAME))
county_City_crswalk_count<-data.frame(city_county_crswalk) %>%
                                dplyr::mutate(COUNTY_NAME= toupper(COUNTY_NAME),
                                               PLACE_NAME= toupper(PLACE_NAME))%>%
                                dplyr::group_by(PLACE_NAME) %>%
                                dplyr::summarise(NUM_COUNTIES_PER_CITY= n())%>%
                                dplyr::select(PLACE_NAME, NUM_COUNTIES_PER_CITY)%>%
                                dplyr::arrange(desc(NUM_COUNTIES_PER_CITY))

keeps= c("COUNTY_NAME", "COUNTYFP", "PLACE_NAME")
merged_count_ccty_crswalk= county_City_crswalk_count %>%
  inner_join(city_county_crswalk[,colnames(city_county_crswalk) %in% keeps],
             by= "PLACE_NAME")
countyByCity_1To1<- merged_count_ccty_crswalk%>%
  dplyr::filter(NUM_COUNTIES_PER_CITY==1)%>%
  distinct()
countyByCity_manyTo1<- merged_count_ccty_crswalk%>%
  dplyr::filter(NUM_COUNTIES_PER_CITY>=2) %>%
  distinct()

#WA CITIES TO WA COUNTY CROSSWALK \MAPPING
nl<- nl%>%
  dplyr::left_join(countyByCity_1To1, by= c("PATIENT_ADDRESS_CITY"="PLACE_NAME"))%>%
  dplyr::mutate(PATIENT_ADDRESS_COUNTY= ifelse(PATIENT_ADDRESS_COUNTY=="", COUNTY_NAME, PATIENT_ADDRESS_COUNTY)) %>%
  dplyr::select(-c("NUM_COUNTIES_PER_CITY", "COUNTY_NAME" ,"COUNTYFP"))%>%
  distinct()

#Code bad address, bad name, and bad dob variables
#Also, convert character variables to uppercase
nl_mod <- nl %>%
  dplyr::mutate_if(is.character, toupper) %>%
  dplyr::mutate(PATIENT_DATE_OF_BIRTH = as.Date(PATIENT_DATE_OF_BIRTH, format= "%m/%d/%Y"),
         PROCESSED_DATE = as.Date(PROCESSED_DATE, format = "%m/%d/%Y"),
         bad_name = id_bad_name(FIRST_NAME, LAST_NAME),
         bad_name = ifelse(LAST_NAME== 'DOE' &
                             is.na(PATIENT_DATE_OF_BIRTH), #Pattern unique to NL
                           1, bad_name),
         bad_dob = id_bad_dob(DOB = PATIENT_DATE_OF_BIRTH),
         bad_address = check_bad_2Address(ADDRESS_LINE_1 = PATIENT_ADDRESS_1,
                                          ADDRESS_LINE_2 = PATIENT_ADDRESS_2,
                                          ADDRESS_CITY = PATIENT_ADDRESS_CITY,
                                          ADDRESS_STATE = PATIENT_ADDRESS_STATE,
                                          ADDRESS_ZIP = PATIENT_ADDRESS_ZIP),
         bad_address = ifelse(WELRS_ASSIGN_ADDR_TYPE=="PATIENT_ADDR_WELRS", bad_address, 1),
         ORIG_PATIENT_ADDRESS_COUNTY= PATIENT_ADDRESS_COUNTY,
         MSH_ID_WELRS_OBX_ID= paste0(MSH_ID,"_",WELRS_OBX_ID),
         PATIENT_ADDRESS_1_mod= trimws(toupper(PATIENT_ADDRESS_1),which="both"),
         PATIENT_ADDRESS_2_mod= trimws(toupper(PATIENT_ADDRESS_2),which="both"),
         PATIENT_ADDRESS_CITY_mod= trimws(toupper(PATIENT_ADDRESS_CITY),which="both"),
         PATIENT_ADDRESS_1_mod= replaceNESW_withNA1(PATIENT_ADDRESS_1_mod),
         PATIENT_ADDRESS_1_mod= replaceNESW_withNA2(PATIENT_ADDRESS_1_mod),
         PATIENT_ADDRESS_1_mod= replaceNESW_withNA3(PATIENT_ADDRESS_1_mod),
         PATIENT_ADDRESS_1_mod= replaceNESW_withNA4(PATIENT_ADDRESS_1_mod),
         PATIENT_ADDRESS_1_mod= replaceNESW_withNA5(PATIENT_ADDRESS_1_mod),
         PATIENT_ADDRESS_1_mod= replaceNESW_withNA6(PATIENT_ADDRESS_1_mod),
         PATIENT_ADDRESS_1_mod= replaceNESW_withNA7(PATIENT_ADDRESS_1_mod),
         PATIENT_ADDRESS_1_mod= replaceNESW_withNA8(PATIENT_ADDRESS_1_mod),
         PATIENT_ADDRESS_2_mod= replaceNESW_withNA1(PATIENT_ADDRESS_2_mod),
         PATIENT_ADDRESS_2_mod= replaceNESW_withNA2(PATIENT_ADDRESS_2_mod),
         PATIENT_ADDRESS_2_mod= replaceNESW_withNA3(PATIENT_ADDRESS_2_mod),
         PATIENT_ADDRESS_2_mod= replaceNESW_withNA4(PATIENT_ADDRESS_2_mod),
         PATIENT_ADDRESS_2_mod= replaceNESW_withNA5(PATIENT_ADDRESS_2_mod),
         PATIENT_ADDRESS_2_mod= replaceNESW_withNA6(PATIENT_ADDRESS_2_mod),
         PATIENT_ADDRESS_2_mod= replaceNESW_withNA7(PATIENT_ADDRESS_2_mod),
         PATIENT_ADDRESS_2_mod= replaceNESW_withNA8(PATIENT_ADDRESS_2_mod),
         PATIENT_ADDRESS_COUNTY= ifelse(is.na(PATIENT_ADDRESS_COUNTY),"", PATIENT_ADDRESS_COUNTY)
    )

testingSite_Addr<- data.frame(testingSite_Addr)%>%
  dplyr::mutate(REPORTING_ADDRESS_modified= trimws(toupper(REPORTING_ADDRESS),which="both"),
                REPORTING_ADDRESS_modified= replaceNESW_withNA1(REPORTING_ADDRESS_modified),
                REPORTING_ADDRESS_modified= replaceNESW_withNA2(REPORTING_ADDRESS_modified),
                REPORTING_ADDRESS_modified= replaceNESW_withNA3(REPORTING_ADDRESS_modified),
                REPORTING_ADDRESS_modified= replaceNESW_withNA4(REPORTING_ADDRESS_modified),
                REPORTING_ADDRESS_modified= replaceNESW_withNA5(REPORTING_ADDRESS_modified),
                REPORTING_ADDRESS_modified= replaceNESW_withNA6(REPORTING_ADDRESS_modified),
                REPORTING_ADDRESS_modified= replaceNESW_withNA7(REPORTING_ADDRESS_modified),
                REPORTING_ADDRESS_modified= replaceNESW_withNA8(REPORTING_ADDRESS_modified)
  )%>%
  dplyr::select(Yr2020,Yr2021, REPORTING_ADDRESS,  REPORTING_ADDRESS_modified, REPORTING_ZIPCODE, REPORTING_CITY)

#WA COUNTIES DESCRIPTION TO FIPS\REFERENCE CODES
nl_mod<- nl_mod[ ,!colnames(nl_mod) %in% c("ACCOUNTABLE_COUNTY")] %>%
  dplyr::mutate(MODIFIED_WELRS_COUNTY= wa_county_to_fips_codes(WELRS_ASSIGNED_COUNTY),
                ACCOUNTABLE_COUNTY= ifelse(str_trim(toupper(WELRS_ASSIGN_ADDR_TYPE),side="both") == "PATIENT_ADDR_WELRS" &
                                             ORIG_ACCOUNTABLE_COUNTY== "", MODIFIED_WELRS_COUNTY, ORIG_ACCOUNTABLE_COUNTY)) %>%
  dplyr::select(-c("MODIFIED_WELRS_COUNTY",  "ORIG_PATIENT_ADDRESS_COUNTY", "MSH_ID_WELRS_OBX_ID"))%>%
  distinct()

nl_mod<- nl_mod%>%
  dplyr::mutate(bad_address= ifelse((PATIENT_ADDRESS_1_mod %in% testingSite_Addr$REPORTING_ADDRESS_modified |
                   PATIENT_ADDRESS_2_mod %in% testingSite_Addr$REPORTING_ADDRESS_modified), 1, bad_address)
          )

drops= c("PATIENT_ADDRESS_1_mod", "PATIENT_ADDRESS_2_mod", "PATIENT_ADDRESS_CITY_mod")
nl_mod<- nl_mod[, !colnames(nl_mod) %in% drops]

end_time1= proc.time()
run_time1= end_time1 - start_time1

