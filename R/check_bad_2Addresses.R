
#rm(list=ls())
options(digit= 20)
pkges<-c("dplyr","sparklyr", "ggplot2","haven","data.table","devtools")
for(pkge in pkges){
  if(!pkge %in% installed.packages())install.packages(pkge, dependencies = TRUE)
}
lapply(pkges, require, character.only= TRUE)

check_bad_2Address<-function(ADDRESS_LINE_1, ADDRESS_LINE_2,
                              ADDRESS_CITY, ADDRESS_STATE, ADDRESS_ZIP){
  if(!is.character(ADDRESS_LINE_1)){
    stop("ADDRESS_LINE_1 must be in character format.")
  } else if(!is.character(ADDRESS_LINE_1)){
    stop("ADDRESS_LINE_2 must be in character format.")
  } else if(!is.character(ADDRESS_CITY)){
    stop("ADDRESS_CITY msut be in character format.")
  } else if(!is.character(ADDRESS_STATE)){
    stop("ADDRESS_STATE msut be in character format.")
  } else if(!is.character(ADDRESS_ZIP)){
    stop("ADDRESS_ZIP msut be in character format.")
  } else{
    bad_add1<-id_bad_address(ADDRESS_LINE_1, ADDRESS_CITY, ADDRESS_STATE, ADDRESS_ZIP)
    bad_add2<-id_bad_address(ADDRESS_LINE_2, ADDRESS_CITY, ADDRESS_STATE, ADDRESS_ZIP)

    result= ifelse(bad_add1==1 & bad_add2==1, 1, 0)
  }
  return(result)
}

