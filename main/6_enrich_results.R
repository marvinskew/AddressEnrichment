
##############################################################################################################
# Script Name: enriched_results.R
#
# Purpose: The final output is an updated "DOH_FULL.csv". The following fields will be in the updated DOH_FULL.csv
#                 1)	Message Identifier (MSH_ID): One of two key variables for linking to NL dataset.
#                 2)	WELRS OBX Identifier (WELRS_OBX_ID): One of two key variables for linking to NL dataset.
#                 3)	Enriched address source (ENRICH_ADD_SRC): Identifies the source of information for an enriched address source.
#                 4)	Enriched address line #1 (ENRICH_ADD_1)
#                 5)	Enriched address line #2 (ENRICH_ADD_2)
#                 6)	Enriched address city (ENRICH_ADD_CITY)
#                 7)	Enriched address state (ENRICH_ADD_STATE)
#                 8)	Enriched address zip code (ENRICH_ADD_ZIP)
#                 9)	Enriched address county (ENRICH_ADD_COUNTY)
#
# Author: Marvin Akuffo, Jon Brown
#
# Date Created :07/06/2021
#
# Email: marvin.akuffo@doh.wa.gov
##############################################################################################################

start_time6= proc.time()
outfile_path= paste0(outfile1, outfile2)

#Load enriched DB
# enriched_path<-list.files(fpath, pattern="^covid19_neglab_enriched$" ,full.names=TRUE)%>%
#     enframe(name= NULL)%>%
#     bind_cols(pmap_df(., file.info))%>%
#     filter(mtime== max(mtime))%>%
#     pull(value)
enriched <- enrich_nl_db_check(enriched_path)

#Add enriched data to the negative labs dataset
all_together <- nl_mod %>%
    full_join(enriched, by=c("WELRS_OBX_ID", "MSH_ID"))

start_time6= proc.time()
all_together2 <- all_together %>%
   dplyr::mutate(ENRICH_ADD_COUNTY= stringr::str_trim(ENRICH_ADD_COUNTY, side= "both"),
                ENRICH_ADD_COUNTY= ifelse(ENRICH_ADD_COUNTY %in% city_county_crswalk$COUNTY_NAME,
                                          ENRICH_ADD_COUNTY, ""),
                enrich_add_stat = case_when(bad_address == 0 ~ 0,
                                     ENRICH_ADD_SRC == 'IIS' ~ 1,
                                     ENRICH_ADD_SRC == 'NL' ~ 2,
                                     ENRICH_ADD_SRC == 'RHINO' ~ 3,
                                     TRUE ~ 4),
                enrich_add_stat = ordered(enrich_add_stat,
                                   labels = c('Enrichment not needed',
                                              'IIS Enriched',
                                              'NL Enriched',
                                              'RHINO Enriched',
                                              'Unable to enrich')))

enriched_neglab_fips= all_together2%>%
  dplyr::mutate(PAT_ADDR_COUNTY_ENHANCED=ifelse((PATIENT_ADDRESS_COUNTY== "") & (ACCOUNTABLE_COUNTY== "") & (PATIENT_ADDRESS_STATE %in% c("WA", "")) ,
                                                   ENRICH_ADD_COUNTY, PATIENT_ADDRESS_COUNTY),
                PATIENT_ADDRESS_1=ifelse(PATIENT_ADDRESS_1== "", ENRICH_ADD_1, PATIENT_ADDRESS_1),
                PATIENT_ADDRESS_CITY=ifelse(PATIENT_ADDRESS_CITY== "", ENRICH_ADD_CITY, PATIENT_ADDRESS_CITY),
                PATIENT_ADDRESS_STATE=ifelse(PATIENT_ADDRESS_STATE== "", ENRICH_ADD_STATE, PATIENT_ADDRESS_STATE),
                PATIENT_ADDRESS_ZIP=ifelse(PATIENT_ADDRESS_ZIP== "", ENRICH_ADD_ZIP, PATIENT_ADDRESS_ZIP)
  )%>%
  dplyr::full_join(wa_fips_code, by=c("PAT_ADDR_COUNTY_ENHANCED"="countyName"), keep = TRUE)%>%
  dplyr::filter(WELRS_OBX_ID!="")%>%
  dplyr::mutate(code_char= ifelse(is.na(Code), "", Code),
                ENCHANCED_ACC_COUNTY= ifelse(code_char=="" & ACCOUNTABLE_COUNTY!="", ACCOUNTABLE_COUNTY, code_char),
                ENCHANCED_ACC_COUNTY= ifelse(ACCOUNTABLE_COUNTY!="", ACCOUNTABLE_COUNTY, ENCHANCED_ACC_COUNTY),
                ENCHANCED_ACC_COUNTY=ifelse(!PATIENT_ADDRESS_STATE %in% c("WA", "", "0") & ENRICH_ADD_SRC %in% c("IIS","RHINO","NL") &
                                      (ACCOUNTABLE_COUNTY!="") & (ACCOUNTABLE_COUNTY!="WA-99"), nl_mod$ORIG_ACCOUNTABLE_COUNTY,ENCHANCED_ACC_COUNTY),
        )%>%
  dplyr::select(-c("Code","code_char", "countyName", "ACH", "Road.to.recovery.region","County"))

enriched_neglab_fips= enriched_neglab_fips[,!colnames(enriched_neglab_fips) %in% c("ACCOUNTABLE_COUNTY")] %>%
                             dplyr::rename(ACCOUNTABLE_COUNTY= ENCHANCED_ACC_COUNTY)

abridged_vars<- c("WELRS_OBX_ID","MSH_ID","ENRICH_ADD_1", "ENRICH_ADD_2", "ENRICH_ADD_CITY", "ENRICH_ADD_STATE","ENRICH_ADD_ZIP",
                  "ENRICH_ADD_COUNTY","enrich_add_stat")
enrich_vars<-c("MSH_ID", "WELRS_OBX_ID", "ENRICH_ADD_SRC", "ENRICH_ADD_1", "ENRICH_ADD_2","ENRICH_ADD_CITY", "ENRICH_ADD_STATE",
               "ENRICH_ADD_ZIP", "ENRICH_ADD_COUNTY")
drops_vars<-c("ENRICH_ADD_1", "ENRICH_ADD_2","ENRICH_ADD_CITY", "ENRICH_ADD_STATE", "ENRICH_ADD_ZIP", "ENRICH_ADD_COUNTY")

neglab_enriched_abridged= enriched_neglab_fips[, colnames(enriched_neglab_fips) %in% abridged_vars]
doh_full= enriched_neglab_fips[,!colnames(enriched_neglab_fips) %in% drops_vars]
covid19_enrich= enriched_neglab_fips[, colnames(enriched_neglab_fips) %in% enrich_vars]

save(doh_full, file= paste0(outfile_path,"/","covid19_neglab_enriched",gsub("-","_",Sys.Date()),".RData"))
save(neglab_enriched_abridged, file= paste0(outfile_path,"/","neglab_enriched_abridged",gsub("-","_",Sys.Date()),".RData"))
saveRDS(covid19_enrich, file=paste0(fpath,"covid19_neglab_enriched", ".RDS"))

end_time6= proc.time()
run_time6= end_time6 - start_time6

# write.csv(neglab_enriched_abridged, paste0(outfile_path,"/","neglab_enriched_abridged",gsub("-","_",Sys.Date()),".csv"))
# write.csv(enriched_neglab_fips, paste0(outfile_path,"/","covid19_neglab_enriched",gsub("-","_",Sys.Date()),".csv"))
#
#




