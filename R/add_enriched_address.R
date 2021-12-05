

#' add_enriched_address
#'
#' @description Takes a dataframe with address information to be added
#' to the negative labs enriched data set. It checks whether these records either
#' a) have no enriched data yet on file b) have enriched phone, but not address
#' or c) already have an enriched address on file. In case c, the function will
#' throw an error and will not successfully complete
#'
#' @param enrich_df The working copy of the enriched dataframe in the R working environment
#' @param new_record_df The dataframe with addresses to be added
#' @param ENRICH_ADD_SRC For new records, specify the column name that will be used for enriched address column ENRICH_ADD_SRC
#' @param ENRICH_ADD_1 For new records, specify the column name that will be used for enriched address column ENRICH_ADD_1
#' @param ENRICH_ADD_2 For new records, specify the column name that will be used for enriched address column ENRICH_ADD_2
#' @param ENRICH_ADD_CITY For new records, specify the column name that will be used for enriched address column ENRICH_ADD_CITY
#' @param ENRICH_ADD_STATE For new records, specify the column name that will be used for enriched address column ENRICH_ADD_STATE
#' @param ENRICH_ADD_ZIP For new records, specify the column name that will be used for enriched address column ENRICH_ADD_ZIP
#' @param ENRICH_ADD_COUNTY For new records, specify the column name that will be used for enriched address column ENRICH_ADD_COUNTY
#'
#' @return An R dataframe object with an updated version of the enriched file
#' @import dplyr
#' @import magrittr
#' @export
#'
add_enriched_address <- function(enrich_df,
                                 new_record_df,
                                 ENRICH_ADD_SRC,
                                 ENRICH_ADD_1,
                                 ENRICH_ADD_2,
                                 ENRICH_ADD_CITY,
                                 ENRICH_ADD_STATE,
                                 ENRICH_ADD_ZIP,
                                 ENRICH_ADD_COUNTY){
  enrich_df<-enriched[,!colnames(enriched) %in% c("ENRICH_PHONE_SRC","ENRICH_PHONE_NUM")]
  
  #Specify columns needed/mapping to enriched DB names
  cols <- c('WELRS_OBX_ID',
            'MSH_ID',
            'ENRICH_ADD_SRC' = ENRICH_ADD_SRC,
            'ENRICH_ADD_1' = ENRICH_ADD_1,
            'ENRICH_ADD_2' = ENRICH_ADD_2,
            'ENRICH_ADD_CITY' = ENRICH_ADD_CITY,
            'ENRICH_ADD_STATE' = ENRICH_ADD_STATE,
            'ENRICH_ADD_ZIP' = ENRICH_ADD_ZIP,
            'ENRICH_ADD_COUNTY' = ENRICH_ADD_COUNTY)
 
  #New addresses to be added
  new_address <- dplyr::select(new_record_df, all_of(cols))
  
  #Records in enriched database that are not being modified
  enriched_no_change <- enrich_df %>%
    dplyr::anti_join(new_address, by = c('WELRS_OBX_ID', 'MSH_ID'))
  
  #Records in enriched database that are being modified
  #Keep only the phone number and identifier records
  enriched_mod_1 <- enrich_df %>%
    dplyr::anti_join(enriched_no_change, by = c('WELRS_OBX_ID', 'MSH_ID')) %>%
    dplyr::mutate(conflict = case_when(ENRICH_ADD_SRC == '' |
                                         is.na(ENRICH_ADD_SRC) ~ 0,
                                       TRUE ~ 1))
  
  #Identify records that already have enriched address
  conflicts <- dplyr::filter(enriched_mod_1, conflict == 1) %>%
    dplyr::select(MSH_ID, WELRS_OBX_ID)
  
  #Number of rows in conflict
  conflict_count <- nrow(conflicts)
  
  #Check for conflicts, throw error message and stop process if so
  if(conflict_count >= 1){
    cat(paste0("ERROR: You are attempting to add address data to ",
               conflict_count,
               " records that already have enriched address data.\n",
               "The process will not continue.\n",
               "Some examples are printed below.\n"))
    print(conflicts[1:10, ])
    stop('Process terminated')
  }
  #No conflicts, proceed to make updated data set
   enriched_mod_2 <- dplyr::select(enriched_mod_1, WELRS_OBX_ID, MSH_ID) %>%
      dplyr::left_join(new_address, by = c('WELRS_OBX_ID', 'MSH_ID')) %>%
      dplyr::select(all_of(colnames(enrich_df)))

  #These records have not had any enriched data (phone or address) yet
   enriched_new_data <- new_address %>%
     dplyr::anti_join(select(enriched_mod_2, WELRS_OBX_ID, MSH_ID))%>%
     dplyr::select(all_of(colnames(enrich_df)))
  
  enriched_new_data <- new_address %>%
    dplyr::anti_join(select(enriched_mod_2, WELRS_OBX_ID, MSH_ID))%>%
    dplyr::mutate(enrich_add_stat= "")%>%
    dplyr::select(all_of(colnames(enrich_df)))
    
  
  #Combine our 3 enriched data frames (no change, some change, new records)
  #into a single DF for output
  out <- rbind(enriched_no_change,
               enriched_mod_2,
               enriched_new_data) %>%
    dplyr::arrange(MSH_ID, WELRS_OBX_ID)
  
  #Make message for output
  cat(paste0('Enriched NL dataset updated with new address data.\n',
             "New records: ", nrow(enriched_new_data),
             "\nModified records: ", nrow(enriched_mod_2),
             "\nUnchanged records: ", nrow(enriched_no_change),
             "\n"))
  return(out)
  
}
