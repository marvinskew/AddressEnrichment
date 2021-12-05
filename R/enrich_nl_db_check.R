
#' create_nl_db
#'
#' @description This function creates an empty dataframe with the enriched dataframe columns
#' in the correct type (e.g. numeric or character) and order (e.g. MSH_ID is the first field).
#' This was mostly a convenience function as the process was under development.
#'
#' @param x This is a dummy variable that does absolutely nothing.
#'
#' @return An R dataframe object with an updated version of the enriched file
#' @export
#'
create_nl_db1 <- function(x=TRUE){
 db <- data.frame(MSH_ID = character(),
                  WELRS_OBX_ID = character(),
                  ENRICH_ADD_SRC = character(),
                  ENRICH_ADD_1 = character(),
                  ENRICH_ADD_2 = character(),
                  ENRICH_ADD_CITY = character(),
                  ENRICH_ADD_STATE = character(),
                  ENRICH_ADD_ZIP = character(),
                  ENRICH_ADD_COUNTY = character(),
                  ENRICH_PHONE_SRC = character(),
                  ENRICH_PHONE_NUM = character())
  return(db)
}


create_nl_db <- function(x=TRUE){
  db <- data.frame(MSH_ID = character(),
                   WELRS_OBX_ID = character(),
                   ENRICH_ADD_SRC = character(),
                   ENRICH_ADD_1 = character(),
                   ENRICH_ADD_2 = character(),
                   ENRICH_ADD_CITY = character(),
                   ENRICH_ADD_STATE = character(),
                   ENRICH_ADD_ZIP = character(),
                   ENRICH_ADD_COUNTY = character())
  return(db)
}


#' enrich_nl_db_check
#'
#' @description This function checks whether the enriched labs dataset already exists
#' at a specified location. If it does exist, the dataframe is loaded. If it does not exist,
#' an empty dataframe is created, saved at the specified path, and loaded to the local environment.
#' This function was mostly created as a convenience during code development.
#'
#' @param dbpath This is the full file path of the enriched file.
#'
#' @return An R dataframe object with an updated version of the enriched file
#' @export
#'
enrich_nl_db_check <- function(dbPath){
  if(file.exists(dbPath)){
    print(paste0("Negative labs DB found at ", dbPath))
  } else{
    print(paste0("NOTE: File not found at ",
                 dbPath,
                 ". An empty file has been created at this location"))
    #saveRDS(create_nl_db(TRUE), dbPath)
    save(create_nl_db(TRUE), dbPath)
  }
  return(readr::read_rds(dbPath))

}


enrich_nl_db_check_csv <- function(dbPath){
  if(file.exists(dbPath)){
    print(paste0("Negative labs DB found at ", dbPath))
  } else{
    print(paste0("NOTE: File not found at ",
                 dbPath,
                 ". An empty file has been created at this location"))
    saveRDS(create_nl_db(TRUE), dbPath)
  }
  return(read.csv(dbPath, sep=",", header= T))
}
