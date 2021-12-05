###########################################################################################
# Script Name : 2_nl_match_and_enrich.R
#
# Purpose: To match bad addresses in neglabs to good address/phone in the same negative PID
#
# Author: Marvin Akuffo, Jon Brown
#
# Date Created :07/06/2021
#
# Email: marvin.akuffo@doh.wa.gov
###########################################################################################

start_time2= proc.time()

#Load NL COVIDS data
enriched_path<-list.files(fpath, pattern="^covid19_neglab_enriched.rds$" ,full.names=TRUE)%>%
   enframe(name= NULL)%>%
   bind_cols(pmap_df(., file.info))%>%
   filter(mtime== max(mtime))%>%
   pull(value)
enriched<- enrich_nl_db_check(enriched_path)

####################################
# Search for other lab records with good address in NL data
####################################
#ID bad addresses that have not already been enriched
bad_address <- filter(nl_mod, bad_address == 1) %>%
  anti_join(filter(enriched,
                   !(is.na(ENRICH_ADD_SRC) == 1 | ENRICH_ADD_SRC == "")),
            by  = c('WELRS_OBX_ID', 'MSH_ID')) %>%
  select(WELRS_OBX_ID, MSH_ID, neg_pid)

#ID good addresses
#These are other labs from the same NEG_PID as a record that is missing address

good_address <- filter(nl_mod, bad_address == 0) %>%
  select(-WELRS_OBX_ID, -MSH_ID) %>%
  inner_join(bad_address) %>%
  arrange(neg_pid, desc(PROCESSED_DATE), desc(MSH_ID), desc(WELRS_OBX_ID)) %>%
  group_by(neg_pid, WELRS_OBX_ID, MSH_ID) %>%
  mutate(rn = row_number(),
         ENRICH_ADD_SRC = 'NL') %>%
  ungroup() %>%
  filter(rn == 1)

#Run custom function to create updated enriched data frame
enrich_temp_1 <- add_enriched_address(enrich_df = enriched,
                            new_record_df = good_address,
                            ENRICH_ADD_SRC = "ENRICH_ADD_SRC",
                            ENRICH_ADD_1 = 'PATIENT_ADDRESS_1',
                            ENRICH_ADD_2 = 'PATIENT_ADDRESS_2',
                            ENRICH_ADD_CITY = 'PATIENT_ADDRESS_CITY',
                            ENRICH_ADD_STATE = 'PATIENT_ADDRESS_STATE',
                            ENRICH_ADD_ZIP = 'PATIENT_ADDRESS_ZIP',
                            ENRICH_ADD_COUNTY = 'ACCOUNTABLE_COUNTY')

end_time2= proc.time()
run_time2= end_time2 - start_time2
