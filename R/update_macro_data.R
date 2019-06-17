#' Update data
#'
#' \code{update_macro_data} function updates the data.
#'
#'  \code{update_macro_data} function updates the data set. To function properly it
#'  requires access to the internet, but an offline update is possible with
#'  \code{download_data=FALSE}. The data can also be updated selectively, but
#'  this functionality is not well tested.
#'
#' @param download_data Should all data be downloaded anew (if true), or should
#'     previously downloaded data be used (if false)?
#' @param vars Specifies the variables to be updated as strings. Default is all.
#' @param countries The iso3c codes of the countries to be considered. Uses
#'  \code{\link{get_countries}}, so pre-defined lists there can be used.
#' @param start_year The first year to be considered (as numeric).
#' @param end_year The last year to be considered (as numeric).
#' @return Returns the updated data set.
#' @seealso \code{\link{get_countries}} to see which pre-defined country sets
#'  are available.
#' @family update_dataset
#' @import data.table
#' @export
update_macro_data <- function(
  download_data=FALSE,
  vars="all",
  countries="EU",
  start_year=1962,
  end_year=2018
){
  data_download <- download_data
  print(paste0("Downloading data: ", data_download))

  countries_used <- get_countries(countries) # countries_considered

  ameco_data <- get_ameco(data_download,
                          countries_used,
                          start_year, end_year)

  barro_lee_data <- get_barro_lee(data_download,
                                  countries_used,
                                  start_year, end_year)

  chinn_ito_data <- get_chinn_ito(data_download,
                                  countries_used,
                                  start_year, end_year)

  complexity_data <- get_complexity(data_download,
                                    countries_used,
                                    start_year, end_year)

  eurostat_bond_data_raw <- get_eurostat_bond_data(data_download,
                                                   countries_used,
                                                   start_year, end_year)

  kof_data <- get_kof(data_download,
                      countries_used,
                      start_year, end_year)

  lmf_data <- get_lmf(data_download,
                      countries_used,
                      start_year, end_year)

  oecd_data <- get_oecd_data(data_download,
                             countries_used,
                             start_year, end_year)

  solt_data <- get_solt(data_download,
                        countries_used,
                        start_year, end_year)

  world_bank_data <- get_worldbank(data_download,
                                   countries_used,
                                   start_year, end_year)

  print("Merging data...")
  macro_data <- Reduce(function(...) merge(..., all=TRUE,
                                           by = c("iso3c", "year")
  ),
  list(
    ameco_data,
    barro_lee_data,
    chinn_ito_data,
    complexity_data,
    eurostat_bond_data_raw,
    kof_data,
    lmf_data,
    oecd_data,
    solt_data,
    world_bank_data
  )
  )
  uniqueness_test <- test_uniqueness(macro_data, c("iso3c", "year"))
  if (isTRUE(uniqueness_test)){
    print("Test for duplicates successful.")
  } else {
    stop("DUPLICATES IN macro_data! ABORT!")
  }
  save(macro_data, file = "data/macro_data.rdata")
  macro_data_csv_name <- "data/macro_data.csv"
  data.table::fwrite(macro_data, file = macro_data_csv_name)
  R.utils::gzip(paste0(macro_data_csv_name),
                destname=paste0(macro_data_csv_name, ".gz"),
                overwrite = TRUE)
  print("finished.")
}
