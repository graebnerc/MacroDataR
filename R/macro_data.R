#' @import data.table

if (!exists("download_data")){
  download_data <- FALSE
}
export_data_source <- "HARV" # "MIT" or "HARV"

countries_considered <- countrycode::countrycode(strsplit(
  "LU, SE, FI, DK, FR, NL, BE, SI, DE, AT, LV, EE, SK, CZ, PL, HU, GB, IE, PT, GR, ES, IT",
  ", ")[[1]], "iso2c", "iso3c")

# World Bank data==============================================================
print("World Bank data...")
# Natural resource rents
# https://data.worldbank.org/indicator/ny.gdp.totl.rt.zs

wb_file_name <- "data-raw/wb_data.csv"
if (download_data){
  wb_raw_data <- data.table::as.data.table(
    WDI::WDI(country = countrycode::countrycode(countries_considered,
                                                "iso3c", "iso2c"),
             indicator = "ny.gdp.totl.rt.zs",
             start = 1962, end = 2016)
    )
  data.table::fwrite(wb_raw_data, wb_file_name)
} else {# TODO Test whether file exists
  if (!file.exists(wb_file_name)){
    warning("File for world bank data does not exist. Download from www...")
    wb_raw_data <- data.table::as.data.table(
      WDI::WDI(country = countries_considered,
               indicator = "ny.gdp.totl.rt.zs",
               start = 1962, end = 2016)
      )
    data.table::fwrite(wb_raw_data, wb_file_name)
  } else {
    wb_raw_data <- data.table::fread(wb_file_name)
  }
}

wb_data <- wb_raw_data[, res_rents:=ny.gdp.totl.rt.zs
                       ][, iso3c:=countrycode::countrycode(iso2c,
                                                           "iso2c", "iso3c")
                         ][, .(iso3c, year,res_rents)]
print("finished.")

# Gini data from Solt==========================================================
print("SWIID data...")
swiid_link <- "https://dataverse.harvard.edu/api/access/datafile/3376371"
swiid_file <- "data-raw/swiid8_0_summary.csv"
swiid_origin_zip_file <- "swiid8_0/swiid8_0_summary.csv"

if (!download_data & file.exists(swiid_file)){
  swiid_raw <- data.table::fread(swiid_file)
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
    country, "country.name", "iso3c"
  )][!is.na(country), .(iso3c=country, year,
                        gini_post_tax=gini_disp, gini_pre_tax=gini_mkt)]
  swiid_raw <- unique(swiid_raw, by = c("iso3c", "year"))
  data.table::fwrite(swiid_raw, swiid_file)
}
print("finished.")

# Merge data===================================================================
print("Merging data...")
macro_data <- Reduce(function(...) merge(..., all=TRUE, by = c("iso3c", "year")),
                     list(wb_data, swiid_raw))
save(macro_data, file = "data/macro_data.rdata")
data.table::fwrite(macro_data, file = "data/macro_data.csv")
print("finished.")
