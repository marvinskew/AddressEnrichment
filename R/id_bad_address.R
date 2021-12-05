#' id_bad_address
#'
#' @description For a given set of address fields, this functions performs a series
#' of checks to determine whether an address is valid. For each row,
#' it will return a variable that specifies whether the address fields are good (value of '0')
#' or bad (value of '1').
#'
#' @param ADDRESS_LINE_1 The first line of the address.
#' @param ADDRESS_CITY The city of the address.
#' @param ADDRESS_STATE The state of the address.
#' @param ADDRESS_ZIP The zip code of the address.
#'
#' @return A single vector with a yes/no (0/1) field indicating 'bad' address.
#' @import dplyr
#' @export
#'
id_bad_address <- function(ADDRESS_LINE_1,
                           ADDRESS_CITY,
                           ADDRESS_STATE,
                           ADDRESS_ZIP){
  if(!is.character(ADDRESS_LINE_1)){
    stop("ADDRESS_LINE_1 must be in character format.")
  } else if(!is.character(ADDRESS_CITY)){
    stop("ADDRESS_CITY msut be in character format.")
  } else if(!is.character(ADDRESS_STATE)){
    stop("ADDRESS_STATE msut be in character format.")
  } else if(!is.character(ADDRESS_ZIP)){
    stop("ADDRESS_ZIP msut be in character format.")
  } else{
    bad_address <- dplyr::case_when(is.na(ADDRESS_LINE_1) == 1 |
                                      nchar(ADDRESS_LINE_1) <= 2 ~ 1,
                                    grepl("[A-Z]", ADDRESS_LINE_1,
                                          ignore.case = TRUE) == 0 ~ 1,
                                    grepl("[0-9]", ADDRESS_LINE_1,
                                          ignore.case = TRUE) == 0 ~ 1,
                                    grepl("HOMELESS", ADDRESS_LINE_1,
                                          ignore.case = TRUE) >= 1 ~ 1,
                                    grepl("HOMLESS", ADDRESS_LINE_1,
                                          ignore.case = TRUE) >= 1 ~ 1,
                                    grepl("UNKNOW", ADDRESS_LINE_1,
                                          ignore.case = TRUE) >= 1 ~ 1,
                                    grepl("^000", ADDRESS_LINE_1,
                                          ignore.case = TRUE) >= 1 ~ 1,
                                    is.na(ADDRESS_CITY) == 1 |
                                      nchar(ADDRESS_CITY) <= 2 ~ 1,
                                    grepl("[A-z]", ADDRESS_CITY,
                                          ignore.case = TRUE) == 0 ~ 1,
                                    is.na(ADDRESS_STATE) == 1 |
                                      nchar(ADDRESS_STATE) <= 1 ~ 1,
                                    is.na(ADDRESS_ZIP) == 1 |
                                      nchar(ADDRESS_ZIP) <= 4 ~ 1,
                                    grepl("[A-z]", ADDRESS_STATE,
                                          ignore.case = TRUE) == 0 ~ 1,
                                    grepl("[0-9]", ADDRESS_ZIP,
                                          ignore.case = TRUE) == 0 ~ 1,
                                    nchar(ADDRESS_ZIP) < 5 ~ 1,
                                    TRUE ~ 0)
    return(bad_address)
  }
}





