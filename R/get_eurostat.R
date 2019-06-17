get_eurostat_bond_data <- function(download_data, countries_considered,
                                   first_year, last_year){
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
  return(eurostat_bond_data_raw)
}


