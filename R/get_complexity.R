print("Complexity data...")
complexity_harvard_url <- "https://intl-atlas-downloads.s3.amazonaws.com/country_sitcproductsection_year.csv.zip"
complexity_harv_file_name <- "data-raw/complexity_harv.csv"
complexity_harv_origin_zip_file <- "country_sitcproductsection_year.csv"

if (!download_data & file.exists(paste0(complexity_harv_file_name, ".gz"))){
  complexity_harv <- data.table::fread(
    paste0(complexity_harv_file_name, ".gz")
  )
} else {
  tmp <- tempfile(fileext = ".zip")
  download.file(complexity_harvard_url,
                tmp,
                quiet = FALSE)
  unzip(tmp, exdir = "data-raw", files = complexity_harv_origin_zip_file)
  file.rename(paste0("data-raw/", complexity_harv_origin_zip_file),
              complexity_harv_file_name)
  unlink(paste0("data-raw/", complexity_harv_origin_zip_file),
         recursive = T)
  complexity_harv_raw <- data.table::fread(complexity_harv_file_name)
  complexity_harv <- complexity_harv_raw[
    !is.na(countrycode::countrycode(location_code, "iso3c", "country.name",
                                    warn = FALSE)),
    .(year, hs_eci, hs_coi, sitc_eci, sitc_coi, location_code)]
  complexity_harv <- unique(complexity_harv, by = c("year", "location_code"))
  data.table::fwrite(x = complexity_harv, file = complexity_harv_file_name)
  R.utils::gzip(paste0(complexity_harv_file_name),
                destname=paste0(complexity_harv_file_name, ".gz"),
                overwrite = TRUE)
}

complexity_mit_url <- "https://atlas.media.mit.edu/en/rankings/country/eci/?download=true&download_all=true"
complexity_mit_country_names_url <- "https://atlas.media.mit.edu/static/db/raw/country_names.tsv.bz2"
complexity_mit_file_name <- "data-raw/complexity_mit.csv"
complexity_mit_origin_zip_file <- "country_sitcproductsection_year.csv"

if (!download_data & file.exists(paste0(complexity_harv_file_name, ".gz"))){
  complexity_mit <- data.table::fread(
    paste0(complexity_mit_file_name, ".gz")
  )
} else {
  tmp <- tempfile(fileext = ".csv")
  download.file(complexity_mit_url,
                tmp,
                quiet = FALSE)
  complexity_mit_raw <- data.table::fread(tmp)

  tmp2 <- tempfile(fileext = ".bz2")
  download.file(complexity_mit_country_names_url,
                tmp2,
                quiet = FALSE)
  complexity_mit_country_names <- data.table::fread(tmp2)

  complexity_mit <- data.table::copy(complexity_mit_raw)

  complexity_mit[, iso3c:=toupper(countrycode::countrycode(
    Country, "name", "id_3char",
    custom_dict = as.data.frame(complexity_mit_country_names)))
    ][, c("Country", "Country ID"):=NULL]

  data.table::fwrite(x = complexity_mit, file = complexity_mit_file_name)
  R.utils::gzip(paste0(complexity_mit_file_name),
                destname=paste0(complexity_mit_file_name, ".gz"),
                overwrite = TRUE)
}

complexity_data <- data.table::as.data.table(
  dplyr::full_join(
    complexity_mit[iso3c %in% countries_considered],
    complexity_harv[location_code %in% countries_considered],
    by = c("Year"="year", "iso3c" = "location_code")
  )
)
data.table::setnames(complexity_data,
                     old = c("Year", "ECI", "ECI+", "iso3c", "hs_eci",
                             "hs_coi", "sitc_eci", "sitc_coi"),
                     new = c("year", "eci_mit", "eci_mit_plus", "iso3c",
                             "eci_harv_hs", "eci_harc_coi_hs",
                             "eci_harv_sitc", "eci_harc_coi_sitc"))
print("finished.")
