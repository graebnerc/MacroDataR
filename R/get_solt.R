#' Get data from SWIID 8 database
#'
#' Downloads data from swiid 8.0 via Harvard Dataverse
#'
#' Gets pre and post tax Ginis from the standardized world income inequality
#'  database provided by Solt.
#'
#' @param download_data If TRUE, data will be downloaded from the internet
#' @param countries_considered Vector of iso3c codes of the countries for
#'  which data should be assembled.
#' @param first_year First year for which data is to be collected (numeric).
#' @param last_year Last year for which data is to be collected (numeric).
#' @family download_helpers
get_solt <- function(download_data, countries_considered,
                     first_year, last_year){
  print("SWIID data...")
  swiid_link <- "https://dataverse.harvard.edu/api/access/datafile/3376371"
  swiid_file <- "data-raw/swiid8_0_summary.csv"
  swiid_origin_zip_file <- "swiid8_0/swiid8_0_summary.csv"

  if (!download_data & file.exists(paste0(swiid_file, ".gz"))){
    swiid_raw <- data.table::fread(paste0(swiid_file, ".gz"))
  } else {
    tmp <- tempfile(fileext = ".zip")
    download.file(swiid_link, tmp,
                  quiet = FALSE)
    unzip(tmp, exdir = "data-raw", files = swiid_origin_zip_file)
    file.rename(paste0("data-raw/", swiid_origin_zip_file), swiid_file)
    unlink(paste0("data-raw/", strsplit(swiid_origin_zip_file, "/")[[1]][[1]]),
           recursive = T)
    swiid_raw <- data.table::fread(swiid_file)
    swiid_raw <- swiid_raw[, country:=countrycode::countrycode(
      country, "country.name", "iso3c", warn = FALSE
    )][!is.na(country), .(iso3c=country, year=as.double(year),
                          gini_post_tax=gini_disp, gini_pre_tax=gini_mkt)]
    swiid_raw <- swiid_raw[iso3c%in%countries_considered]
    swiid_raw <- unique(swiid_raw, by = c("iso3c", "year"))
    stopifnot(test_uniqueness(swiid_raw, c("iso3c", "year")))
    data.table::fwrite(swiid_raw, swiid_file)
    R.utils::gzip(paste0(swiid_file),
                  destname=paste0(swiid_file, ".gz"),
                  overwrite = TRUE)
  }
  print("finished.")
  return(swiid_raw)
}
