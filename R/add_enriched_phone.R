
#' add_enriched_phone
#'
#' @description Takes a dataframe with phone information to be added
#' to the negative labs enriched data set. It checks whether these records either
#' a) have no enriched data yet on file b) have enriched address, but not phone
#' or c) already have an enriched phone number on file. In case c, the function
#' will throw an error and will not successfully complete.
#'
#' @param enrich_df The working copy of the enriched dataframe in the R working environment
#' @param new_record_df The dataframe with addresses to be added
#' @param ENRICH_PHONE_SRC For new records, specify the column name that will be used for enriched phone column ENRICH_PHONE_SRC
#' @param ENRICH_PHONE_NUM For new records, specify the column name that will be used for enriched phone column ENRICH_PHONE_NUM
#'
#' @return An R dataframe object with an updated version of the enriched data file
#' @import dplyr
#' @import magrittr
#' @export
#'
add_enriched_phone <- function(enrich_df,
                                 new_record_df,
                                 ENRICH_PHONE_SRC,
                                 ENRICH_PHONE_NUM){

  #Specify columns needed/mapping to enriched DB names
  cols <- c('WELRS_OBX_ID',
            'MSH_ID',
            'ENRICH_PHONE_SRC' = ENRICH_PHONE_SRC,
            'ENRICH_PHONE_NUM' = ENRICH_PHONE_NUM)

  #New phone to be added
  new_phone <- dplyr::select(new_record_df, all_of(cols))

  #Records in enriched database that are not being modified
  enriched_no_change <- enrich_df %>%
    dplyr::anti_join(new_phone,
                     by = c('WELRS_OBX_ID', 'MSH_ID'))

  #Records in enriched database that are being modified
  #Keep only the phone number and identifier records
  enriched_mod_1 <- enrich_df %>%
    dplyr::anti_join(enriched_no_change,
                     by = c('WELRS_OBX_ID', 'MSH_ID')) %>%
    dplyr::mutate(conflict = case_when(ENRICH_PHONE_SRC == '' |
                                         is.na(ENRICH_PHONE_SRC) ~ 0,
                                       TRUE ~ 1))

  #Identify records that already have enriched phone
  conflicts <- dplyr::filter(enriched_mod_1, conflict == 1) %>%
    dplyr::select(MSH_ID, WELRS_OBX_ID)

  #Number of rows in conflict
  conflict_count <- nrow(conflicts)

  #Check for conflicts, throw error message and stop process if so
  if(conflict_count >= 1){
    cat(paste0("ERROR: You are attempting to add phone data to ",
               conflict_count,
               " records that already have enriched phone data.\n",
               "The process will not continue.\n",
               "Some examples are printed below.\n"))
    print(conflicts[1:10, ])
    stop('Process terminated')
  }

  #No conflicts, proceed to make updated data set
  enriched_mod_2 <- dplyr::select(enriched_mod_1,
                                  WELRS_OBX_ID, MSH_ID,
                                  ENRICH_ADD_SRC,
                                  ENRICH_ADD_1, ENRICH_ADD_2,
                                  ENRICH_ADD_CITY, ENRICH_ADD_STATE,
                                  ENRICH_ADD_ZIP, ENRICH_ADD_COUNTY) %>%
    dplyr::left_join(new_phone, by = c('WELRS_OBX_ID', 'MSH_ID')) %>%
    dplyr::select(all_of(colnames(enrich_df)))

  #These records have not had any enriched data (phone or address) yet
  enriched_new_data <- new_phone %>%
    dplyr::anti_join(select(enriched_mod_2, WELRS_OBX_ID, MSH_ID)) %>%
    dplyr::mutate(ENRICH_ADD_SRC = NA_character_,
                  ENRICH_ADD_1 = NA_character_,
                  ENRICH_ADD_2 = NA_character_,
                  ENRICH_ADD_CITY = NA_character_,
                  ENRICH_ADD_STATE = NA_character_,
                  ENRICH_ADD_ZIP = NA_character_,
                  ENRICH_ADD_COUNTY = NA_character_) %>%
    dplyr::select(all_of(colnames(enrich_df)))


  #Combine our 3 enriched data frames (no change, some change, new records)
  #into a single DF for output
  out <- rbind(enriched_no_change,
               enriched_mod_2,
               enriched_new_data) %>%
    dplyr::arrange(MSH_ID, WELRS_OBX_ID)

  #Make a message before output
  cat(paste0('Enriched NL dataset updated with new phone data.\n',
             "New records: ", nrow(enriched_new_data),
             "\nModified records: ", nrow(enriched_mod_2),
             "\nUnchanged records: ", nrow(enriched_no_change),
             "\n"))
  return(out)

}
