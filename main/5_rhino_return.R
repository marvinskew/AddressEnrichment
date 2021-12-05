################################################################################
# Script Name: 5_rhino_return.R
#
# Purpose: To use the return file from the RHINO team
#
# Author: Marvin Akuffo, Jon Brown
#
# Date Created :07/06/2021
#
# Email: marvin.akuffo@doh.wa.gov
################################################################################

start_time5= proc.time()
#Find all CSV files from the RHINO team and the folder with the correct date
rhino_list <- list.files(paste0(rhino_file_path,
                                    rhino_return_date, "/"),
                         pattern = "*.csv",
                         full.names = TRUE)

#Read in all of the RHINO CSVs at once
rhino_return <- do.call(rbind,
                        lapply(rhino_list, function(x) data.table::fread(x)))

#Setting path of enriched file (we call it multiple times)
#enriched_path <- './data/covid19_neglab_enriched'
#enriched <- enrich_nl_db_check(enriched_path)

####################################
# PREPARE RHINO DATA
####################################
#RHINO State/Cty are in FIPS format. Need character.
#Read in a FIPS/Place name crosswalk
fips <- readxl::read_xlsx('./data/US_FIPS_Codes.xlsx')

#RHINO data (good address only)
rhino_mod <- rhino_return %>%
  separate(col = 'match_id', into = c('MSH_ID', 'WELRS_OBX_ID'), sep = "\\|") %>%
  select(MSH_ID, WELRS_OBX_ID,
         PatAddrStreet, PatAddrStreetOther,
         PtCity, PtCounty, PtState, PtZipcode,
         PtPhone,
         FN, LN, dob) %>%
  left_join(select(fips, PtState = StateFIPS, StateAbbr) %>% distinct()) %>%
  mutate(bad_address= check_bad_2Address(ADDRESS_LINE_1 = PatAddrStreet,
                                          ADDRESS_LINE_2 = PatAddrStreetOther,
                                          ADDRESS_CITY = PtCity,
                                          ADDRESS_STATE = StateAbbr,
                                          ADDRESS_ZIP = PtZipcode)

   ) %>%
  filter(bad_address == 0)

#ID records that do not need further enrichment
enrich_comp <- filter(enriched,
                        !(is.na(ENRICH_ADD_SRC) == 1 | ENRICH_ADD_SRC == "")) %>%
  select(MSH_ID, WELRS_OBX_ID)

#Now, the RHINO records to keep are the ones that don't match the
#list of records that need no further enrichment
rhino_to_enrich <- rhino_mod %>%
  anti_join(enrich_comp, by = c('WELRS_OBX_ID'))

####################################
# ADDRESS ENRICHMENT
####################################
#Get IIS address for merging
rhino_address <- filter(rhino_to_enrich, bad_address == 0) %>%
  select(MSH_ID, WELRS_OBX_ID, PatAddrStreet, PatAddrStreetOther,
         PtCity, PtCounty, StateAbbr, PtZipcode) %>%
  mutate(ENRICH_ADD_SRC = 'RHINO') %>%
  anti_join(filter(enriched,
                   !(is.na(ENRICH_ADD_SRC) == 1 | ENRICH_ADD_SRC == "")),
            by = c('WELRS_OBX_ID'))

#Run custom function to create updated enriched data frame
  #Also running function to fix missing/FIPS coded county
enrich_temp_2 <- add_enriched_address(enrich_df = enrich_temp_1,
                                      new_record_df = rhino_address,
                                      ENRICH_ADD_SRC = "ENRICH_ADD_SRC",
                                      ENRICH_ADD_1 = 'PatAddrStreet',
                                      ENRICH_ADD_2 = 'PatAddrStreetOther',
                                      ENRICH_ADD_CITY = 'PtCity',
                                      ENRICH_ADD_STATE = 'StateAbbr',
                                      ENRICH_ADD_ZIP = 'PtZipcode',
                                      ENRICH_ADD_COUNTY = 'PtCounty') %>%
  mutate_if(is.character, toupper) %>%
  fix_enriched_county() %>%
  enrich_db_dedup()

####################################
# Save output
####################################
saveRDS(enrich_temp_2, file = enriched_path)

end_time5= proc.time()
run_time5= end_time5 - start_time5
