#' Get KOF data
#'
#' Download the KOF globalization index
#'
#' Downloads and assembles data from the KOF globalization index project.
#'  Since the KOF institute does not provide for an API the data is downloaded
#'  directly from their homepage. This means that whenever a new version of the
#'  KOF is availbale, this code must be updated. Latest update: May 2019.
get_kof <- function(download_data, countries_considered,
                    first_year, last_year){
  print("KOF Globalization Index...")
  kof_url <- "https://www.ethz.ch/content/dam/ethz/special-interest/dual/kof-dam/documents/Globalization/2018/Data_2018_2.dta"
  kof_file <- "data-raw/kof.csv"

  if (download_data | !file.exists((paste0(kof_file, ".gz")))){
    warning("KOF data is not updated automatically.
            Currently downloading file Data_2018_2.dta (May 2019).")
    tmp <- tempfile(fileext = ".dta")
    download.file(kof_url, tmp,
                  quiet = FALSE)
    kof_raw <- data.table::as.data.table(haven::read_dta(tmp))
    kof_raw <- kof_raw[code %in% countries_considered,
                       .(code, year,
                         KOFGI, # KOF Globalisation Index
                         KOFGIdf, # "KOF Globalisation Index, de facto
                         KOFGIdj, # KOF Globalisation Index, de jure
                         KOFEcGI, # KOF Economic Globalisation Index
                         KOFEcGIdf, # KOF Economic Globalisation Index, de facto
                         KOFEcGIdj, # KOF Economic Globalisation Index, de jure
                         KOFTrGI, # KOF Trade Globalisation Index
                         KOFTrGIdf, # KOF Trade Globalisation Index, de facto
                         KOFTrGIdj, # KOF Trade Globalisation Index, de jure
                         KOFFiGI, # KOF Financial Globalisation Index
                         KOFFiGIdf, # KOF Financial Globalisation Index, de facto
                         KOFFiGIdj # KOF Financial Globalisation Index, de jure
                       )]
    data.table::fwrite(kof_raw, kof_file)
    R.utils::gzip(paste0(kof_file),
                  destname=paste0(kof_file, ".gz"),
                  overwrite = TRUE)
  } else {
    kof_raw <- data.table::fread(paste0(kof_file, ".gz"))
  }

  kof <- kof_raw[, .(
    iso3c = code,
    year = as.double(year),
    kof_G = as.double(KOFGI), # KOF Globalisation Index
    kof_G_df = as.double(KOFGIdf), # "KOF Globalisation Index, de facto
    kof_G_dj = as.double(KOFGIdj), # KOF Globalisation Index, de jure
    kof_EcG = as.double(KOFEcGI), # KOF Economic Globalisation Index
    kof_EcG_df = as.double(KOFEcGIdf), # KOF Economic Globalisation Index, de facto
    kof_EcG_dj = as.double(KOFEcGIdj), # KOF Economic Globalisation Index, de jure
    kof_trade = as.double(KOFTrGI), # KOF Trade Globalisation Index
    kof_trade_df = as.double(KOFTrGIdf), # KOF Trade Globalisation Index, de facto
    kof_trade_dj = as.double(KOFTrGIdj), # KOF Trade Globalisation Index, de jure
    kof_fin = as.double(KOFFiGI), # KOF Financial Globalisation Index
    kof_fin_df = as.double(KOFFiGIdf), # KOF Financial Globalisation Index, de facto
    kof_fin_dj = as.double(KOFFiGIdj) # KOF Financial Globalisation Index, de jure
  )]
  print("finished.")
  return(kof)
}
