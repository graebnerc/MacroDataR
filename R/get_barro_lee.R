#' Download Barro-Lee educational data
#'
#' Get educational data from Barro-Lee dataset
#'
#' @param download_data If TRUE, data will be downloaded from the internet
#' @param countries_considered Vector of iso3c codes of the countries for
#'  which data should be assembled.
#' @param first_year First year for which data is to be collected (numeric).
#' @param last_year Last year for which data is to be collected (numeric).
#' @family download_helpers
get_barro_lee <- function(download_data, countries_considered,
                          first_year, last_year){
  print("Barro-Lee educational data...")
  barro_lee_url <- "http://www.barrolee.com/data/BL_v2.2/BL2013_MF1599_v2.2.csv"
  barro_lee_file <- "data-raw/barro_lee.csv"
  if (download_data | !file.exists((paste0(barro_lee_file, ".gz")))){
    tmp <- tempfile(fileext = ".csv")
    download.file(barro_lee_url, tmp,
                  quiet = FALSE)
    barro_lee_raw <- data.table::fread(tmp)
    barro_lee_raw <- barro_lee_raw[, .(year, WBcode, lsc, lhc, yr_sch)]
    data.table::fwrite(barro_lee_raw, barro_lee_file)
    R.utils::gzip(paste0(barro_lee_file),
                  destname=paste0(barro_lee_file, ".gz"),
                  overwrite = TRUE)
  } else {
    barro_lee_raw <- data.table::fread(paste0(barro_lee_file, ".gz"))
  }
  barro_lee <- barro_lee_raw[!WBcode%in%c("REU", "ROM", "SER"),
                             .(iso3c=countrycode::countrycode(WBcode,
                                                              "wb", "iso3c"),
                               year=as.double(year),
                               school_agv_yrs=as.double(yr_sch),
                               school_share_sec=as.double(lsc),
                               school_share_ter=as.double(lhc))
                             ][
                               iso3c %in% countries_considered
                               ]
  print("finished.")
  return(barro_lee)
}
