#' id_bad_dob
#'
#' @description For a given date of birth (DOB), this function looks for dates of birth
#' that are a) after the current date or b) before 1/1/1900. For each row,
#' it will return a variable that specifies whether the DOB fields are good (value of '0')
#' or bad (value of '1').
#'
#' @param DOB The column name containing date of birth.
#'
#' @return A single vector with a yes/no (0/1) field indicating 'bad' DOB.
#' @import dplyr
#' @import lubridate
#' @export
#'
id_bad_dob <- function(DOB){
  if(!is.Date(DOB)){
    stop("The date of birth must be a date object.")
  } else{
    bad_dob <- dplyr::case_when(is.na(DOB) ~ 1,
                                DOB > Sys.Date() ~ 1,
                                DOB <= as.Date('1900-01-01') ~ 1,
                                TRUE ~ 0)
    return(bad_dob)
  }
}

