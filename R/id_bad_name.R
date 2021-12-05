#' id_bad_name
#'
#' @description For a given set of first and last names, this function performs
#' checks to determine whether any name information might be invalid. For each row,
#' it will return a variable that specifies whether the first/last name are
#' good (value of '0') or bad (value of '1').
#'
#' @param FIRST_NAME Column name for first name.
#' @param LAST_NAME Column name for last name.
#'
#' @return A single vector with a yes/no (0/1) field indicating 'bad' name.
#' @import dplyr
#' @export
#'
id_bad_name <- function(FIRST_NAME, LAST_NAME){
  bad_name <- dplyr::case_when(FIRST_NAME == 'TB' ~ 1,
                               grepl("[1234567890#]", FIRST_NAME,
                                     ignore.case = TRUE) >= 1 ~ 1,
                               grepl('UNKNOWN', FIRST_NAME,
                                     ignore.case = TRUE) >= 1 ~ 1,
                               grepl('ANONYMOUS', FIRST_NAME,
                                     ignore.case = TRUE) >= 1 ~ 1,
                               grepl('REFUSED', FIRST_NAME,
                                     ignore.case = TRUE) >= 1 ~ 1,
                               grepl('EMPLOYEE', FIRST_NAME,
                                     ignore.case = TRUE) >= 1 ~ 1,
                               grepl('UNKNOWN', LAST_NAME,
                                     ignore.case = TRUE) >= 1 ~ 1,
                               grepl('ANONYMOUS', LAST_NAME,
                                     ignore.case = TRUE) >= 1 ~ 1,
                               grepl('REFUSED', LAST_NAME,
                                     ignore.case = TRUE) >= 1 ~ 1,
                               grepl('EMPLOYEE', LAST_NAME,
                                     ignore.case = TRUE) >= 1 ~ 1,
                               LAST_NAME %in% c('ANON', 'UNOS') ~ 1,
                               TRUE ~ 0)
  return(bad_name)
}


