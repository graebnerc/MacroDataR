#' Test uniqueness of data table
#'
#' Tests whether a data.table has unique rows.
#'
#' @param data_table A data frame of data table of which uniqueness should
#'  be tested.
#' @param index_vars Vector of strings, which specify the columns of
#'  data_table according to which uniqueness should be tested
#'  (e.g. country and year).
#' @return TRUE if data_table is unique, FALSE and a warning if it is not.
#' @family update_dataset_helpers
#' @import data.table
#' @export
test_uniqueness <- function(data_table, index_vars, print_pos=TRUE){
  data_table <- data.table::as.data.table(data_table)
  if (nrow(data_table)!=data.table::uniqueN(data_table, by = index_vars)){
    warning(paste0("Rows in the data.table: ", nrow(data_table),
                   ", rows in the unique data.table:",
                   data.table::uniqueN(data_table, by = index_vars)))
    return(FALSE)
  } else {
    if (print_pos){
      print(paste0("No duplicates in ", as.list(sys.call()[[2]])))
    }
    return(TRUE)
  }
}

#' Unfactor a factor
#'
#' Transforms a factor into an integer
#'
#' Transforms a factor into an integer by first transforming it into character
#'
#' @param x An input that is potentially a factor
#' @return x as an integer
#' @family update_dataset_helpers
unfactor <- function(x){
  y <- as.integer(as.character(x))
  return(y)
}

#' Get countries
#'
#' Returns a pre-specified country list
#'
#' @param countries_to_get A code for a pre-specified country list
#' @return A vector with country codes
#' @family update_dataset_helpers
#' @export
get_countries <- function(countries_to_get){

  pre_spec_country_lists <- list(
    "EU" = c("AUT", "BEL", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA",
             "GBR", "GRC", "HUN", "IRL", "ITA", "LUX", "LVA", "NLD", "POL",
             "PRT", "SVK", "SVN", "SWE")
  )

  if (length(countries_to_get)==1 &
      countries_to_get %in% names(pre_spec_country_lists)
  ){
    return(pre_spec_country_lists[[countries_to_get]])
  } else{
    warning(
      paste0("No pre-spec country list found. Use country codes provided: ",
             countries_to_get)
    )
    return(countries_to_get)
  }
}
