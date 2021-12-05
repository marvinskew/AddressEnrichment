################################################################################
# Script Name: 4_rhino_roster_gen.R
#
# Purpose: To perform the IIS match for neg COVID labs and pull address info
#          in where it is missing in current NL data.
#
# Author: Marvin Akuffo, Jon Brown
#
# Date Created :07/06/2021
#
# Email: marvin.akuffo@doh.wa.gov
################################################################################

start_time4= proc.time()
#Only want records processed within one month of today
#NOTE: Can be TOGGLED
cutoff_date <- Sys.Date() %m+% months(-1)

#Setting path of enriched file (we call it multiple times)
# enriched_path<-list.files(fpath, pattern="^covid19_neglab_enriched.rds$" ,full.names=TRUE)%>%
#   enframe(name= NULL)%>%
#   bind_cols(pmap_df(., file.info))%>%
#   filter(mtime== max(mtime))%>%
#   pull(value)
enriched <- enrich_nl_db_check(enriched_path) %>%
                   select(MSH_ID, WELRS_OBX_ID, ENRICH_ADD_SRC)

####################################
# Pull in NL data in need of enrichment
####################################
#ID records that do not need further enrichment
enrich_comp <- dplyr::filter(enriched,
                        !(is.na(ENRICH_ADD_SRC) == 1 | ENRICH_ADD_SRC == "")) %>%
  select(MSH_ID, WELRS_OBX_ID)

#NL data in need of enrichment (address missing, good name for matching)
#Be sure to exclude records not in need of enrichment!
nl_for_match <- filter(nl_mod, (bad_address == 1) & (bad_name == 0 & bad_dob == 0)) %>%
  anti_join(enrich_comp, by = c('WELRS_OBX_ID', 'MSH_ID')) %>%
  mutate(match_id = paste(MSH_ID, WELRS_OBX_ID, sep = "|")) %>%
  left_join(enriched, by= 'WELRS_OBX_ID') %>%
  mutate(enrich_stat = case_when(is.na(ENRICH_ADD_SRC) == 0 ~
                                   'All fields enriched',
                                 is.na(ENRICH_ADD_SRC) == 1 ~
                                   'Address enrichment needed',
                                  TRUE ~ 'Address enrichment needed')
         ) %>%
  ##filter(PROCESSED_DATE >= cutoff_date) %>%
  select(match_id, FIRST_NAME, LAST_NAME, PATIENT_MIDNAME, PATIENT_DATE_OF_BIRTH, enrich_stat)


#Save output file
saveRDS(nl_for_match, paste0("./data/neglab_roster_rhino_", Sys.Date()))
write.csv(nl_for_match, paste0("./data/neglab_roster_rhino_",gsub("-","_",Sys.Date()),".csv"))

end_time4= proc.time()
run_time4= end_time4 - start_time4


