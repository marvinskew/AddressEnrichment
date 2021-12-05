################################################################################
# Script Name: 3_iis_match_and_enrich.R
#
# Purpose: To perform the IIS match for negative COVID labs and pull address info
#          in where it is missing in current NL data.
#
# Author: Marvin Akuffo, Jon Brown
#
# Date Created :07/06/2021
#
# Email: marvin.akuffo@doh.wa.gov
################################################################################

start_time3= proc.time()

#Setting path of enriched file (we call it multiple times)
iis_path<-list.files(iis_lib, pattern="^iisf" ,full.names=TRUE)%>%
   enframe(name= NULL)%>%
   bind_cols(pmap_df(., file.info))%>%
   filter(mtime== max(mtime))%>%
   pull(value)

iis_mod <- haven::read_sas(iis_path)
iis_mod$DOBD<-as.Date(iis_mod$DOBD)
####################################
# Run exact match
####################################
#IIS data (good address or phone only)

iis_mod<-iis_mod %>%
          dplyr::mutate(bad_address= id_bad_address(STREET_ADDRESS, CITY, STATE, ZIP),
                        bad_dob = id_bad_dob(DOB =DOBD),
                        bad_name = id_bad_name(FNAMED, LNAMED),
                        bad_name = ifelse(LNAMED== 'DOE' & is.na(DOBD),1,bad_name))
iis_for_match <- filter(iis_mod,
                        (bad_address == 0) & (bad_name == 0 & bad_dob == 0)) %>%
  select(SIIS_ID, FNAMED, LNAMED, MNAMED, DOBD)

#ID records that do not need further enrichment
enrich_comp <- filter(enriched,
                        !(is.na(ENRICH_ADD_SRC) == 1 | ENRICH_ADD_SRC == "")) %>%
  select(MSH_ID, WELRS_OBX_ID)

#NL data in need of enrichment (phone or address missing, good name for matching)
  #Also, exclude records with complete enrichment info
nl_for_match <- dplyr::filter(nl_mod, (bad_address == 1) & (bad_name == 0 & bad_dob == 0)) %>%
  select(MSH_ID, WELRS_OBX_ID, FIRST_NAME, LAST_NAME, PATIENT_MIDNAME,PATIENT_DATE_OF_BIRTH) %>%
  anti_join(enrich_comp, by = c('WELRS_OBX_ID', 'MSH_ID'))


#Perform exact matching
matches <- inner_join(iis_for_match,
                      nl_for_match,
                      by = c('FNAMED' = "FIRST_NAME",
                             "LNAMED" = "LAST_NAME",
                             "DOBD" = "PATIENT_DATE_OF_BIRTH"),
                      keep = TRUE) %>%
  left_join(nl_for_match, by=c("MSH_ID" ,"WELRS_OBX_ID" ) ) %>%
  arrange(LNAMED, FNAMED)

#Get match IDs for later steps
match_ids <- select(matches, MSH_ID, WELRS_OBX_ID, SIIS_ID) %>%
  distinct()

####################################
# ADDRESS ENRICHMENT
####################################
#Get IIS addresses for merging
iis_address <- filter(iis_mod, bad_address == 0) %>%
  select(SIIS_ID,
         STREET_ADDRESS,
         CITY, STATE, ZIP,
         COUNTY)

#Merge IIS Address to NL records with bad Address
enrich_add <- filter(nl_mod, bad_address == 1) %>%
  anti_join(filter(enrich_temp_1,
                   !(is.na(ENRICH_ADD_SRC) == 1 | ENRICH_ADD_SRC == "")),
            by  = c('WELRS_OBX_ID', 'MSH_ID')) %>%
  inner_join(match_ids, by= c('WELRS_OBX_ID', 'MSH_ID')) %>%
  inner_join(iis_address, by=c("SIIS_ID")) %>%
  mutate(ENRICH_ADD_SRC = "IIS",
         STREET2 = NA_character_)

#Run custom function to create updated enriched data frame
  #Also running function to fix missing/FIPS coded county
enrich_temp_2 <- add_enriched_address(enrich_df = enrich_temp_1,
                                      new_record_df = enrich_add,
                                      ENRICH_ADD_SRC = "ENRICH_ADD_SRC",
                                      ENRICH_ADD_1 = 'STREET_ADDRESS',
                                      ENRICH_ADD_2 = 'STREET2',
                                      ENRICH_ADD_CITY = 'CITY',
                                      ENRICH_ADD_STATE = 'STATE',
                                      ENRICH_ADD_ZIP = 'ZIP',
                                      ENRICH_ADD_COUNTY = 'COUNTY') %>%
  fix_enriched_county() %>%
  enrich_db_dedup()

####################################
# Save output
####################################
saveRDS(enrich_temp_2, file = enriched_path)

end_time3= proc.time()
run_time3= end_time3 - start_time3
