#' fix_enriched_county
#'
#' @description Enriched address information is prone to two common data cleaning
#' issues:
#'
#' First, county is often stored in a FIPS-code type format (e.g. WA-33
#' instead of 'King County'). This function converts all Washington FIPS codes
#' into text format using the US_FIPS_Codes.xlsx file in the data subfolder.
#'
#' Second, county is sometimes still missing in the enriched address. Since most
#' cities are located entirely in one county, we can determine the county from
#' the listed city in most instances. This part of the code uses the
#' sub-est2019_all.csv file in the data subfolder.
#'
#' @param enrich_df The working copy of the enriched dataframe in the R working environment
#'
#' @return An R dataframe object with an updated version of the enriched file
#' @import dplyr
#' @import magrittr
#' @import readxl
#' @import data.table
#' @import stringr
#' @export
#'
fix_enriched_county <- function(enrich_df){
  #Get FIPS codes
  fips <- readxl::read_xlsx('./data/US_FIPS_Codes.xlsx') %>%
    dplyr::select(FIPS5, CountyName, StateAbbr) %>%
    dplyr::filter(StateAbbr=="WA")%>%
    dplyr::mutate(CountyName = toupper(CountyName))
  ####################################
  # Fix missing county cases
  ####################################
  #Restrict to address-enriched records with missing county
  enrich_miss_county <- enrich_df %>%
    dplyr::filter(dplyr::coalesce(ENRICH_ADD_COUNTY, "") == "" &
                    !is.na(ENRICH_ADD_SRC))

  #Pull county codes for all US cities
  #Manipulate strings to pull out CITY and TOWN from the end of each string.
  #Join to FIPS county data to get county names
  #Downloadable from: https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-cities-and-towns.html#ds
  city_county_xwalk <- data.table::fread('./data/sub-est2019_all.csv') %>%
    dplyr::mutate_if(is.character, toupper) %>%
    dplyr::filter(STNAME == "Washington")%>%
    dplyr::filter(SUMLEV %in% c('157', '061') & grepl('COUNTY', NAME) == 0) %>%
    dplyr::mutate(COUNTY = stringr::str_pad(COUNTY, width = 3,
                                            side = 'left', pad = '0'),
                  STATE = stringr::str_pad(STATE, width = 2,
                                           side = 'left', pad = '0'),
                  FIPS5 = paste0(STATE, COUNTY),
                  CITY = gsub(' \\(PT.\\)', "", NAME),
                  CITY = gsub(" CITY$", "", CITY),
                  CITY = gsub(" TOWN$", "", CITY),
                  CITY = gsub(" VILLAGE$", "", CITY),
                  CITY = gsub(" $", "", CITY)) %>%
    dplyr::left_join(fips) %>%
    dplyr::select(NAME, CITY, CountyName, STNAME, StateAbbr)

  #Some cities extend across multiple counties
  #Not a good idea to 'choose one'. So, let's focus on cities
  #fully contained in one county for this process
  city_county_singles <- city_county_xwalk %>%
    dplyr::group_by(STNAME, NAME) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::filter(n == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n) %>%
    dplyr::left_join(city_county_xwalk)

  #Get new county value from steps above
  new_county <- dplyr::inner_join(enrich_miss_county, city_county_singles,
                                  by = c('ENRICH_ADD_CITY' = 'CITY',
                                         'ENRICH_ADD_STATE' = 'StateAbbr')) %>%
    dplyr::select(MSH_ID, WELRS_OBX_ID, CountyName) %>%
    dplyr::right_join(enrich_df, by = c('WELRS_OBX_ID', 'MSH_ID')) %>%
    dplyr::mutate(ENRICH_ADD_COUNTY = coalesce(CountyName, ENRICH_ADD_COUNTY)) %>%
    dplyr::select(-CountyName)

  ####################################
  # Fix numeric county entries
  ####################################
  #Fix cases where county is numeric (e.g. WA-33 instead of King)
  #num_county is an indicator value that the county is in numeric form
  #county_num strips all non-numeric characters from the county field
  #Then, we convert it into FIPS 5-digit code for joining to a crosswalk
  enrich_out <- new_county %>%
    dplyr::mutate(
      num_county = dplyr::case_when(grepl("WA-[0-9]", ENRICH_ADD_COUNTY) >= 1 ~ 1,
                                    grepl("^[0-9]", ENRICH_ADD_COUNTY) >= 1 ~ 2,
                                    TRUE ~ 0),
      county_num = stringr::str_pad(gsub("[^0-9.]", "", ENRICH_ADD_COUNTY), 3, 'left', pad="0"),
      FIPS5 = dplyr::case_when(num_county == 1 ~ paste0('53', county_num),
                               num_county == 2 ~ trimws(ENRICH_ADD_COUNTY),
                               TRUE ~ NA_character_)) %>%
    dplyr::left_join(fips) %>%
    dplyr::mutate(ENRICH_ADD_COUNTY = dplyr::case_when(num_county %in% c(1,2) &
                                                         is.na(CountyName) == 0 ~
                                                         CountyName,
                                                TRUE ~ ENRICH_ADD_COUNTY)) %>%
    dplyr::select(-CountyName, -num_county, -county_num, -FIPS5, -StateAbbr)
  return(enrich_out)
}
