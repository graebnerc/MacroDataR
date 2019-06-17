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
