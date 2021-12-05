#' id_bad_phone
#'
#' @description For a given set of phone numbers, this functions performs a series
#' of checks to determine whether any phone information might be invalid. For each row,
#' it will return a variable that specifies whether the phone number fields are
#' good (value of '0') or bad (value of '1').
#'
#' @param PHONE_NUMBER Column name for phone number.
#'
#' @return A single vector with a yes/no (0/1) field indicating 'bad' phone.
#' @import dplyr
#' @export
#'
id_bad_phone <- function(PHONE_NUMBER){
  if(!is.character(PHONE_NUMBER)){
    stop("PHONE_NUMBER must be in character format.")
  } else{
    bad_phone <- dplyr::case_when(is.na(PHONE_NUMBER) == 1 |
                                    nchar(PHONE_NUMBER) < 9 ~ 1,
                                  grepl("[9]{4,}", PHONE_NUMBER) == 1 ~ 1,
                                  grepl("[0]{4,}", PHONE_NUMBER) == 1 ~ 1,
                                  grepl("[5]{4,}", PHONE_NUMBER) == 1 ~ 1,
                                  grepl("[1]{4,}", PHONE_NUMBER) == 1 ~ 1,
                                  TRUE ~ 0)
    return(bad_phone)
  }

}
