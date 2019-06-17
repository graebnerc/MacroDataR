#' Get data on the Chinn-Ito index
#'
#' Downloads data directly from the Chinn-Ito website
#'
#' @param download_data If TRUE, data will be downloaded from the internet
#' @param countries_considered Vector of iso3c codes of the countries for
#'  which data should be assembled.
#' @param first_year First year for which data is to be collected (numeric).
#' @param last_year Last year for which data is to be collected (numeric).
#' @family download_helpers
get_chinn_ito <- function(download_data, countries_considered,
                          first_year, last_year){
  print("Chinn-Ito index...")
  chinn_ito_url <- "http://web.pdx.edu/~ito/kaopen_2016.dta"
  chinn_ito_file <- "data-raw/chinn_ito.csv"

  if (download_data | !file.exists((paste0(chinn_ito_file, ".gz")))){
    tmp <- tempfile(fileext = ".dta")
    download.file(chinn_ito_url, tmp,
                  quiet = FALSE)
    chinn_ito_raw <- data.table::as.data.table(haven::read_dta(tmp))

    chinn_ito_raw <- chinn_ito_raw[ccode %in% countries_considered,
                                   .(ccode, year,
                                     kaopen, # Chinn-Ito index
                                     ka_open # Normalized Chinn-Ito index
                                   )
                                   ]
    data.table::fwrite(chinn_ito_raw, chinn_ito_file)
    R.utils::gzip(paste0(chinn_ito_file),
                  destname=paste0(chinn_ito_file, ".gz"),
                  overwrite = TRUE)
  } else {
    chinn_ito_raw <- data.table::fread(paste0(chinn_ito_file, ".gz"))
  }
  chinn_ito <- chinn_ito_raw[,
                             .(
                               iso3c = ccode,
                               year = as.double(year),
                               chinn_ito = as.double(kaopen),
                               chinn_ito_normed = as.double(ka_open)
                             )]
  print("finished.")

  return(chinn_ito)
}
