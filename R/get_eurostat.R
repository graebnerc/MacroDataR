#' Get data on bond yields from Eurostat
#'
#' Gets data on bond yields from Eurostat from dataset 'irt_lt_mcby_a'.
#'
#'  @param download_data If TRUE, data will be downloaded from the internet
#'  @param countries_considered Vector of iso3c codes of the countries for
#'   which data should be assembled.
#'  @param first_year First year for which data is to be collected (numeric).
#'  @param last_year Last year for which data is to be collected (numeric).
#'  @family download_helpers
get_eurostat_bond_data <- function(download_data, countries_considered,
                                   first_year, last_year){
  print("Getting eurostat bond data...")
  eurostat_file_name <- "data-raw/eurostat_bond_data.csv"

  if (download_data | !file.exists((paste0(eurostat_file_name, ".gz")))){
    if (!download_data){
      warning("File for Eurostat data does not exist. Download from www...")
    }
    eurostat_id <- "irt_lt_mcby_a"
    eurostat_bond_data_raw <- eurostat::get_eurostat(
      eurostat_id,
      time_format ="num",
      filters = list(
        geo=countrycode::countrycode(countries_considered, "iso3c", "eurostat"),
        time=first_year:last_year)
    )
    eurostat_bond_data_raw <- data.table::as.data.table(eurostat_bond_data_raw)
    eurostat_bond_data_raw <- eurostat_bond_data_raw[, .(
      iso3c=countrycode::countrycode(geo, "eurostat", "iso3c"),
      year=unfactor(time),
      bond_yield=values)]

    test_uniqueness(eurostat_bond_data_raw, c("iso3c", "year"))

    data.table::fwrite(eurostat_bond_data_raw, eurostat_file_name)
    R.utils::gzip(paste0(eurostat_file_name),
                  destname=paste0(eurostat_file_name, ".gz"),
                  overwrite = TRUE, remove = TRUE)
  } else {
    eurostat_bond_data_raw <- data.table::fread(paste0(eurostat_file_name, ".gz"))
  }
  print("finished.")
  return(eurostat_bond_data_raw)
}
