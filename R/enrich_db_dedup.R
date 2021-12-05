#' enrich_db_dedup
#'
#' @description Identifies records matched to data more than once and picks one
#' to keep
#'
#' @param df The working copy of the enriched dataframe in the R working environment
#'
#' @return An R dataframe object with deduplicated records
#' @import dplyr
#' @import magrittr
#' @export


enrich_db_dedup <- function(df){
  #Identify records that were matched multiple times (usually from IIS)
  df<-df[, !colnames(df) %in% c("ENRICH_PHONE_SRC", "ENRICH_PHONE_NUM")]
  dups <- df %>%
    dplyr::group_by(MSH_ID, WELRS_OBX_ID) %>%
    dplyr::arrange(MSH_ID, WELRS_OBX_ID) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::filter(n > 1) %>%
    dplyr::left_join(df) %>%
    dplyr::mutate(rn = row_number()) %>%
    dplyr::ungroup()

  #We want to keep the first record for each duplicate match
  dup_to_keep <- dplyr::filter(dups, rn == 1) %>%
    dplyr::select(-n, -rn)

  #Now, the output data will be the enriched records with no duplicates
  #Plus the record we wish to keep for any duplicates
  out <- df %>%
    dplyr::anti_join(dups) %>%
    rbind(dup_to_keep)

  message(nrow(dups), " records had more than one address or phone record.\n",
          "A total of ", nrow(dup_to_keep),
          " distinct enriched records were kept")
  return(out)
}



