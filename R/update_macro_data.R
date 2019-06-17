#' Update data
#'
#' \code{update_data} function updates the data.
#'
#'  \code{update_data} function updates the data set. To function properly it
#'  requires access to the internet. The data can also be updated selectively.
#'
#' @param updata_full Should all data be downloaded anew (if true), or should
#'     previously downloaded data be used (if false)?
#' @param vars Specifies the variables to be updated as strings. Default is all.
#' @return Returns the updated data set.
#' TODO: Maybe only information?
#' @export
update_macro_data <- function(download_data=FALSE, vars="all", countries="EU",
                              start_year=1962, end_year=2018) {
  data_download <- download_data
  print(paste0("Downloading data: ", data_download))

  countries_used <- get_countries(countries) # countries_considered

  # first_year <- start_year
  # last_year <- end_year

  eurostat_bond_data_raw <- get_eurostat_bond_data(data_download,
                                                   countries_used,
                                                   start_year, end_year)
  oecd_data <- get_oecd_data(data_download,
                             countries_used,
                             start_year, end_year)

  ameco_data <- get_ameco(data_download,
                          countries_used,
                          start_year, end_year)

  world_bank_data <- get_worldbank(data_download,
                                   countries_used,
                                   start_year, end_year)

  lmf_data <- get_lmf(data_download,
                      countries_used,
                      start_year, end_year)

  solt_data <- get_solt(data_download,
                        countries_used,
                        start_year, end_year)

  #-----

  print("Merging data...")
  macro_data <- Reduce(function(...) merge(..., all=TRUE,
                                           by = c("iso3c", "year")),
                       list(wb_data, swiid_raw, ameco_full, oecd_data, lmf,
                            complexity_data, barro_lee, kof, chinn_ito,
                            eurostat_bond_data_raw)
  )
  test_uniqueness(macro_data, c("iso3c", "year"))
  save(macro_data, file = "data/macro_data.rdata")
  macro_data_csv_name <- "data/macro_data.csv"
  data.table::fwrite(macro_data, file = macro_data_csv_name)
  R.utils::gzip(paste0(macro_data_csv_name),
                destname=paste0(macro_data_csv_name, ".gz"),
                overwrite = TRUE)
  print("finished.")
}
