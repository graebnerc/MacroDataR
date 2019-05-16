#' @import data.table

if (!exists("download_data")){
  download_data <- FALSE
}
export_data_source <- "HARV" # "MIT" or "HARV"

countries_considered <- countrycode::countrycode(strsplit(
  "LU, SE, FI, DK, FR, NL, BE, SI, DE, AT, LV, EE, SK, CZ, PL, HU, GB, IE, PT, GR, ES, IT",
  ", ")[[1]], "iso2c", "iso3c")

first_year <- 1962
last_year <- 2018

# World Bank data==============================================================
print("World Bank data...")
# TODO Add export_GDP
# TODO Download all countries and filter those we do not need
#
wb_vars <- c(
  "ny.gdp.totl.rt.zs", # Natural resource rents: https://data.worldbank.org/indicator/ny.gdp.totl.rt.zs
  "NY.GDP.MKTP.KN", # Real GDP (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KN
  "NY.GDP.MKTP.KD.ZG", # Real GDP growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
  "NY.GDP.PCAP.KN", # Real GDP per capita (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KN
  "NY.GDP.PCAP.KD.ZG", # Real GDP per capita growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD.ZG
  "NY.GDP.MKTP.KD", # Real GDP (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
  "NY.GDP.PCAP.KD", # Real GDP per capita (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
  "NY.GDP.MKTP.CN", # Nominal GDP (current LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.CN
  "NY.GDP.PCAP.CN", # Nominal GDP per capita (current LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.CN
  "NY.GDP.MKTP.CD", # Nominal GDP (current US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
  "NY.GDP.PCAP.CD", # Nominal GDP per capita (current US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
  "NY.GDP.MKTP.PP.KD", # GDP, PPP (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD
  "NY.GDP.PCAP.PP.KD", # GDP, PPP per capita (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
  "NY.GDP.MKTP.PP.CD", # GDP, PPP (current int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD
  "NY.GDP.PCAP.PP.CD" # GDP, PPP per capita (current int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
)

wb_var_names <- c(
  "res_rents", # Natural resource rents: https://data.worldbank.org/indicator/ny.gdp.totl.rt.zs
  "gdp_real_lcu", # Real GDP (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KN
  "gdp_real_lcu_growth", # Real GDP growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
  "gdp_real_pc_lcu", # Real GDP per capita (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KN
  "gdp_real_pc_lcu_growth", # Real GDP per capita growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD.ZG
  "gdp_real_usd", # Real GDP (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
  "gdp_real_pc_usd", # Real GDP per capita (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
  "gdp_nom_lcu", # Nominal GDP (current LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.CN
  "gdp_nom_pc_lcu", # Nominal GDP per capita (current LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.CN
  "gdp_nom_usd", # Nominal GDP (current US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
  "gdp_nom_pc_usd", # Nominal GDP per capita (current US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
  "gdp_real_ppp", # GDP, PPP (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD
  "gdp_real_pc_ppp", # GDP, PPP per capita (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
  "gdp_nom_ppp", # GDP, PPP (current int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD
  "gdp_nom_pc_ppp" # GDP, PPP per capita (current int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
)

wb_file_name <- "data-raw/wb_data.csv"
if (download_data){
  wb_raw_data <- data.table::as.data.table(
    WDI::WDI(country = countrycode::countrycode(countries_considered,
                                                "iso3c", "iso2c"),
             indicator = wb_vars,
             start = first_year, end = last_year)
    )
  data.table::fwrite(wb_raw_data, wb_file_name)
} else {# TODO Test whether file exists
  if (!file.exists(wb_file_name)){
    warning("File for world bank data does not exist. Download from www...")
    wb_raw_data <- data.table::as.data.table(
      WDI::WDI(country = countries_considered,
               indicator = wb_vars,
               start = first_year, end = last_year)
      )
    data.table::fwrite(wb_raw_data, wb_file_name)
  } else {
    wb_raw_data <- data.table::fread(wb_file_name)
  }
}

data.table::setnames(wb_raw_data, old = wb_vars, new = wb_var_names)
wb_data <- wb_raw_data[, iso3c:=countrycode::countrycode(iso2c,
                                                           "iso2c", "iso3c")
                         ][, c("iso2c", "country"):=NULL]
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
  )][!is.na(country), .(iso3c=country, year=as.double(year),
                        gini_post_tax=gini_disp, gini_pre_tax=gini_mkt)]
  swiid_raw <- unique(swiid_raw, by = c("iso3c", "year"))
  data.table::fwrite(swiid_raw, swiid_file)
}
print("finished.")

# AMECO data===================================================================
print("AMECO data...")
ameco_link <- "http://ec.europa.eu/economy_finance/db_indicators/ameco/documents/ameco0.zip"
ameco_file <- "data-raw/ameco/AMECO1.TXT"

if (download_data | !file.exists(ameco_file)){
  tmp <- tempfile(fileext = ".zip")
  download.file(ameco_link, tmp,
                quiet = FALSE)
  if (file.exists(ameco_file)){
    unlink("data-raw/ameco", recursive = TRUE)
  }
  unzip(tmp, exdir = "data-raw/ameco")
}

# unemployment---------------------------------------------------------------
# Remark: two observations exist for 1991 for Germany and West Germany;
# Here the mean is used
print("...ameco01...")
ameco01 <- data.table::fread("data-raw/ameco/AMECO1.TXT",
                             fill = TRUE, header = TRUE,
                             stringsAsFactors = FALSE)
ameco01 <- ameco01[
  TITLE=="Unemployment rate: total :- Member States: definition EUROSTAT"]

ameco01_germany <-data.table::copy(ameco01)
ameco01_germany <- ameco01_germany[COUNTRY %in% c("Germany", "West Germany")]

ameco01_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]

ameco01_germany <- data.table::melt(ameco01_germany, id.vars=c("COUNTRY"),
                                    variable.name="year",
                                    value.name = "unemp_rate")
ameco01_germany[, year:=as.double(as.character(year))]
ameco01_germany[COUNTRY=="West Germany" & year>1990, unemp_rate:=NA]
ameco01_germany <- ameco01_germany[, COUNTRY:=countrycode::countrycode(COUNTRY,
                                                                       "country.name", "iso3c"
)]
ameco01_germany[, unemp_rate:=mean(unemp_rate, na.rm = T), year]
ameco01_germany <- unique(ameco01_germany)

ameco01 <- ameco01[COUNTRY != "DEU"]
ameco01[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
ameco01 <- ameco01[, COUNTRY2:=countrycode::countrycode(COUNTRY,
                                                        "country.name", "iso3c"
)
][!is.na(COUNTRY)]
ameco01 <- data.table::melt(ameco01, id.vars=c("COUNTRY"),
                            variable.name="year",
                            value.name = "unemp_rate")

ameco01 <- rbind(ameco01, ameco01_germany)
ameco01[, unemp_rate:=as.double(unemp_rate)]
if (sum(duplicated(ameco01, by = c("COUNTRY", "year")))>0){
  warning("Duplicated rows in ameco01!")
}

# Harmonised consumer price index (All-items) (2015 = 100)-------------------
print("...ameco02...")
ameco02 <- data.table::fread("data-raw/ameco/AMECO2.TXT",
                             fill = TRUE, header = TRUE)
ameco02 <- ameco02[
  TITLE=="Harmonised consumer price index (All-items)"]
ameco02 <- ameco02[, COUNTRY:=countrycode::countrycode(COUNTRY,
                                                       "country.name", "iso3c"
)
][!is.na(COUNTRY)]
ameco02[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]

ameco02 <- data.table::melt(ameco02, id.vars=c("COUNTRY"),
                            variable.name="year",
                            value.name = "cpi")
if (sum(duplicated(ameco02, by = c("COUNTRY", "year")))>0){
  warning("Duplicated rows in ameco02!")
}

# Capital formation----------------------------------------------------------
# TODO Wir hatten: Real gross fixed capital formation / real net capital stock; aber welche Werte sind das?
# capital_formation_real # 3
# capital_stock_real # 3
# Remark: after 1990 the values for the united Germany are used
print("...ameco03...")
ameco03 <- data.table::fread("data-raw/ameco/AMECO3.TXT",
                             fill = TRUE, header = TRUE)
ameco03 <- ameco03[
  TITLE%in%c("Gross fixed capital formation at current prices: total economy")]
ameco03 <- ameco03[UNIT=="Mrd ECU/EUR"]
ameco03_germany <- ameco03[COUNTRY %in% c("Germany", "West Germany")]
ameco03_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
ameco03_germany <- ameco03_germany[, COUNTRY:=countrycode::countrycode(COUNTRY,
                                                                       "country.name", "iso3c"
)
][!is.na(COUNTRY)]

ameco03_germany <- data.table::melt(ameco03_germany, id.vars=c("COUNTRY"),
                                    variable.name="year",
                                    value.name = "cap_form")
ameco03_germany[, cap_form:=mean(cap_form, na.rm=T), year]

ameco03_germany <- unique(ameco03_germany)

ameco03 <- ameco03[!COUNTRY %in% c("Germany", "West Germany")]
ameco03[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
ameco03 <- ameco03[, COUNTRY:=countrycode::countrycode(COUNTRY,
                                                       "country.name", "iso3c"
)
][!is.na(COUNTRY)]
ameco03 <- data.table::melt(ameco03, id.vars=c("COUNTRY"),
                            variable.name="year",
                            value.name = "cap_form")
ameco03 <- rbind(ameco03, ameco03_germany)
if (sum(duplicated(ameco03, by = c("COUNTRY", "year")))>0){
  warning("Duplicated rows in ameco03!")
}

# GDP growth-------------------------------------------------------------------
# TODO: Einheiten noch fixen, aber vielleicht besser von Weltbank wg coverage
print("..ameco06..")
ameco06 <- data.table::fread("data-raw/ameco/AMECO6.TXT",
                             fill = TRUE, header = TRUE)
gdp_vars <- c(
  "Gross domestic product at current prices",
  "Gross domestic product at 2010 reference levels",
  "Gross domestic product at current prices per head of population",
  "Gross domestic product at 2010 reference levels per head of population"
)
ameco06_GDP <- ameco06[
  TITLE%in%gdp_vars
  ][!COUNTRY%in%c("European Union", "European Union excluding UK",
                  "European Union (15 countries)", "Euro area",
                  "Euro area (12 countries)", "EU15 (including D_W West-Germany)",
                  "EA12 (including D_W West-Germany)")
    ]

# Ameco 7--------------------------------------------------------------------
print("...ameco07...")
ameco07 <- data.table::fread("data-raw/ameco/AMECO7.TXT",
                             fill = TRUE, header = TRUE)

# Wage share-----------------------------------------------------------------
print("...wage share...")
ameco07_wage_share <- ameco07[
  TITLE=="Adjusted wage share: total economy: as percentage of GDP at current prices (Compensation per employee as percentage of GDP at market prices per person employed.)"
  ][!COUNTRY%in%c("European Union", "European Union excluding UK",
                  "European Union (15 countries)", "Euro area",
                  "Euro area (12 countries)", "EU15 (including D_W West-Germany)",
                  "EA12 (including D_W West-Germany)")
    ]

ameco07_wage_share_germany <-data.table::copy(ameco07_wage_share)
ameco07_wage_share_germany <- ameco07_wage_share_germany[
  COUNTRY %in% c("Germany", "West Germany")]

ameco07_wage_share_germany[, c("CODE", "SUB-CHAPTER", "TITLE",
                               "UNIT",  "V67"):=NULL]

ameco07_wage_share_germany <- data.table::melt(
  ameco07_wage_share_germany, id.vars=c("COUNTRY"),
  variable.name="year",
  value.name = "wage_share")

ameco07_wage_share_germany[, year:=as.double(as.character(year))]
ameco07_wage_share_germany[COUNTRY=="West Germany" & year>1990, wage_share:=NA]
ameco07_wage_share_germany <- ameco07_wage_share_germany[, COUNTRY:=countrycode::countrycode(COUNTRY,
                                                                                             "country.name", "iso3c"
)]
ameco07_wage_share_germany[, wage_share:=mean(wage_share, na.rm = T), year]
ameco07_wage_share_germany <- unique(ameco07_wage_share_germany)

ameco07_wage_share[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
ameco07_wage_share <- ameco07_wage_share[, COUNTRY2:=countrycode::countrycode(COUNTRY,
                                                                              "country.name", "iso3c"
)
][!is.na(COUNTRY) & COUNTRY != "DEU"]

ameco07_wage_share <- data.table::melt(ameco07_wage_share,
                                       id.vars=c("COUNTRY"),
                                       variable.name="year",
                                       value.name = "wage_share")

ameco07_wage_share <- rbind(ameco07_wage_share, ameco07_wage_share_germany)
ameco07_wage_share[, wage_share:=as.double(wage_share)]
if (sum(duplicated(ameco01, by = c("COUNTRY", "year")))>0){
  warning("Duplicated rows in ameco07_wage_share!")
}

# RULC-----------------------------------------------------------------------
print("...RULC...")
ameco07_rulc <- ameco07[
  TITLE=="Real unit labour costs: total economy (Ratio of compensation per employee to nominal GDP per person employed.)"
  ][!COUNTRY%in%c("European Union",
                  "European Union excluding UK", "Euro area",
                  "EU15 (including DEL \"linked\" Germany)" ,
                  "EA12 (including DEL \"linked\" Germany)")
    ][ , COUNTRY:=countrycode::countrycode(COUNTRY, "country.name", "iso3c")]
ameco07_rulc[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
ameco07_rulc <- data.table::melt(ameco07_rulc,
                                 id.vars=c("COUNTRY"),
                                 variable.name="year",
                                 value.name = "rulc")
if (sum(duplicated(ameco07_rulc, by = c("COUNTRY", "year")))>0){
  warning("Duplicated rows in ameco07_rulc!")
}

# NULC-----------------------------------------------------------------------
print("...NULC...")
ameco07_nulc <- ameco07[
  TITLE=="Nominal unit labour costs: total economy (Ratio of compensation per employee to real GDP per person employed.)"
  ][!COUNTRY%in%c("European Union",
                  "European Union excluding UK", "Euro area",
                  "EU15 (including DEL \"linked\" Germany)" ,
                  "EA12 (including DEL \"linked\" Germany)")
    ][ , COUNTRY:=countrycode::countrycode(COUNTRY, "country.name", "iso3c")]
ameco07_nulc[, c("CODE", "SUB-CHAPTER", "TITLE"):=NULL]
ameco07_nulc <- data.table::melt(ameco07_nulc,
                                 id.vars=c("COUNTRY", "UNIT"),
                                 variable.name="year",
                                 value.name = "rulc")
ameco07_nulc <- data.table::dcast(ameco07_nulc, COUNTRY+year~UNIT, value.var="rulc")
names(ameco07_nulc) <- c("COUNTRY", "year", "nulc_eur", "nulc_nac")
if (sum(duplicated(ameco07_nulc, by = c("COUNTRY", "year")))>0){
  warning("Duplicated rows in ameco07_nulc!")
}

# Merge all AMECO tables-----------------------------------------------------
print("...merge all AMECO...")
ameco_full <- Reduce(function(...) merge(..., all=TRUE,
                                         by = c("COUNTRY", "year")),
                     list(ameco01, ameco02, ameco03, ameco07_wage_share,
                          ameco07_rulc, ameco07_nulc))
ameco_full <- ameco_full[, .(year=as.double(year), iso3c=COUNTRY, unemp_rate,
                             cpi, cap_form, wage_share, rulc, nulc_eur,
                             nulc_nac)]
print("....finished.")

# Merge data===================================================================
# TODO Testen ob es keine Dopplungen gibt

print("Merging data...")
macro_data <- Reduce(function(...) merge(..., all=TRUE, by = c("iso3c", "year")),
                     list(wb_data, swiid_raw, ameco_full))
save(macro_data, file = "data/macro_data.rdata")
data.table::fwrite(macro_data, file = "data/macro_data.csv")
print("finished.")
