#' @import data.table

if (!exists("download_data")){
  download_data <- FALSE
}
download_data_exports_mit <- FALSE
download_data_exports_harv <- FALSE

countries_considered <- countrycode::countrycode(strsplit(
  "LU, SE, FI, DK, FR, NL, BE, SI, DE, AT, LV, EE, SK, CZ, PL, HU, GB, IE, PT, GR, ES, IT",
  ", ")[[1]], "iso2c", "iso3c")

first_year <- 1962
last_year <- 2018
# OECD data====================================================================

oecd_debt_file_name <- "data-raw/oecd_debt_data.csv"
oecd_debt_vars <- c("DBTS1GDP", "DBTS11GDP", "DBTS12GDP",
                    "DBTS13GDP", "DBTS14_S15GDI")

if (download_data | !file.exists(oecd_debt_file_name)){
  if (!download_data){
    warning(
      warning("File for OECD debt data does not exist. Download from www...")
    )
  }
  filter_list <- list(countries_considered,
                      oecd_debt_vars
  )

  oecd_debt_data_raw <- OECD::get_dataset(dataset = "FIN_IND_FBS",
                                          start_time = first_year,
                                          end_time = last_year,
                                          filter = filter_list)
  oecd_debt_data_raw <- dplyr::select(oecd_debt_data_raw,
                                      -dplyr::one_of(
                                        "TIME_FORMAT", "UNIT", "POWERCODE")
  )
  data.table::fwrite(oecd_debt_data_raw, oecd_debt_file_name)
  oecd_debt_data_raw <- data.table::as.data.table(oecd_debt_data_raw)
} else {
  oecd_debt_data_raw <- data.table::fread(oecd_debt_file_name)
}

oecd_debt_data <- data.table::dcast(oecd_debt_data_raw,
                                    LOCATION + obsTime ~ INDICATOR,
                                    value.var="obsValue")
old_names <- c("LOCATION", "obsTime", oecd_debt_vars)
new_names <- c("iso3c", "year",
               "total_debt_percGDP",
               "debt_corp_nf_percGDP",
               "debt_corp_f_percGDP",
               "debt_gen_gov_percGDP",
               "debt_hh_npish_percGDI")
data.table::setnames(oecd_debt_data, old = old_names, new = new_names)
oecd_debt_data[,
               (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
               .SDcols = setdiff(new_names, "iso3c")
               ]

# OECD: Public debt to GDP-----------------------------------------------------
oecd_pub_debt_file_name <- "data-raw/oecd_pub_debt_data.csv"

if (download_data | !file.exists(oecd_pub_debt_file_name)){
  if (!download_data){
    warning(
      warning("File for OECD public debt data does not exist. Download from www...")
    )
  }
  filter_list <- list(
    countries_considered,
    "DBTS13GDP"
  )

  oecd_pub_debt_data_raw <- OECD::get_dataset(dataset = "NAAG",
                                          start_time = first_year,
                                          end_time = last_year,
                                          filter = filter_list)
  oecd_pub_debt_data_raw <- data.table::as.data.table(oecd_pub_debt_data_raw)
  oecd_pub_debt_data <- oecd_pub_debt_data_raw[, .(LOCATION, obsTime, obsValue)]
  data.table::fwrite(oecd_pub_debt_data, oecd_pub_debt_file_name)
} else {
  oecd_pub_debt_data <- data.table::fread(oecd_pub_debt_file_name)
}

old_names <- c("LOCATION", "obsTime", "obsValue")
new_names <- c("iso3c", "year", "debt_gen_gvt_gross")
data.table::setnames(oecd_pub_debt_data, old = old_names, new = new_names)
oecd_pub_debt_data[,
                   (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
                   .SDcols = setdiff(new_names, "iso3c")
                   ]

# OECD finance data-------------------------------------------------------
oecd_finance_file_name <- "data-raw/oecd_finance_data.csv"

if (download_data | !file.exists(oecd_finance_file_name)){
  if (!download_data){
    warning(
      "File for OECD finance data does not exist. Download from www..."
    )
  }
  filter_list <- list(
    "IRLT",
    countries_considered,
    "A")

  oecd_finance_data_raw <- OECD::get_dataset(dataset = "MEI_FIN",
                                             start_time = first_year,
                                             end_time = last_year,
                                             filter = filter_list)
  oecd_finance_data_raw <- data.table::as.data.table(oecd_finance_data_raw)
  oecd_finance_data <- oecd_finance_data_raw[,
                                             .(LOCATION, obsTime, obsValue)
                                             ]
  data.table::fwrite(oecd_finance_data,
                     oecd_finance_file_name)
} else {
  oecd_finance_data <- data.table::fread(oecd_finance_file_name)
}

old_names <- c("LOCATION", "obsTime", "obsValue")
new_names <- c("iso3c", "year",
               "interest_long_term")
data.table::setnames(oecd_finance_data, old = old_names, new = new_names)
oecd_finance_data[,
                  (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
                  .SDcols = setdiff(new_names, "iso3c")
                  ]

# OECD: Average wages----------------------------------------------------------
oecd_wage_data_raw_file <- "data-raw/oecd_wage_data.csv"

if (download_data | !file.exists(oecd_wage_data_raw_file)){
  if (!download_data){
    warning(
      "File for OECD wage data does not exist. Download from www..."
    )
  }
  filter_list <- list(
    countries_considered,
    c("USDPPP")
  )

  oecd_wage_data_raw <- OECD::get_dataset(dataset = "AV_AN_WAGE",
                                          start_time = first_year,
                                          end_time=last_year,
                                          filter = filter_list)

  oecd_wage_data_raw <- data.table::as.data.table(oecd_wage_data_raw)
  oecd_wage_data <- oecd_wage_data_raw[, .(COUNTRY, obsTime, obsValue)]
  data.table::fwrite(oecd_wage_data,
                     oecd_wage_data_raw_file)
} else {
  oecd_wage_data <- data.table::fread(oecd_wage_data_raw_file)
}

old_names <- c("COUNTRY", "obsTime", "obsValue")
new_names <- c("iso3c", "year",
               "average_wages")
data.table::setnames(oecd_wage_data, old = old_names, new = new_names)
oecd_wage_data[,
                  (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
                  .SDcols = setdiff(new_names, "iso3c")
                  ]


# Merge OECD data--------------------------------------------------------------
oecd_data <- Reduce(function(...) merge(..., all=TRUE,
                                           by = c("iso3c", "year")),
                       list(oecd_finance_data, oecd_debt_data, oecd_wage_data,
                            oecd_pub_debt_data)
  )

# World Bank data==============================================================
print("World Bank data...")
# TODO Add export_GDP
# TODO Download all countries and filter those we do not need
#
wb_vars <- c(
  "BX.KLT.DINV.WD.GD.ZS", # Foreign direct investment, net inflows (% of GDP): https://data.worldbank.org/indicator/BX.KLT.DINV.WD.GD.ZS
  "BM.KLT.DINV.WD.GD.ZS", # Foreign direct investment, net outflows (% of GDP): https://data.worldbank.org/indicator/BM.KLT.DINV.WD.GD.ZS
  "GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP): https://data.worldbank.org/indicator/GC.TAX.TOTL.GD.ZS
  "GC.TAX.INTT.RV.ZS", # Taxes on international trade (% of revenue): https://data.worldbank.org/indicator/GC.TAX.INTT.RV.ZS
  "GC.TAX.YPKG.RV.ZS", # Taxes on income, profits and capital gains (% of revenue): https://data.worldbank.org/indicator/GC.TAX.YPKG.RV.ZS
  "NE.TRD.GNFS.ZS", # Trade is the sum of exports and imports of goods and services measured as a share of gross domestic product: https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS
  "NE.EXP.GNFS.ZS", # Exports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.EXP.GNFS.ZS
  "NE.IMP.GNFS.ZS", # Imports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.IMP.GNFS.ZS
  "GB.XPD.RSDV.GD.ZS", # Research and development expenditure (% of GDP): https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS
  "SP.POP.SCIE.RD.P6", # The number of researchers engaged in Research &Development (R&D), expressed as per million: SP.POP.SCIE.RD.P6
  "VA.EST", # WGI: Voice and Accountability; WGI home: https://info.worldbank.org/governance/wgi/#home API: https://api.worldbank.org/v2/sources/3/indicators
  "RQ.EST", # WGI: Regulatory Quality
  "RL.EST", # WGI: Rule of Law
  "PV.EST", # WGI: Political Stability and Absence of Violence/Terrorism
  "GE.EST", # WGI: Government Effectiveness
  "CC.EST", # WGI: Control of Corruption
  "sl.ind.empl.zs", # Employment in industry (% of total employment): https://data.worldbank.org/indicator/sl.ind.empl.zs
  "SL.AGR.EMPL.ZS", # Employment in agriculture (% of total employment): https://data.worldbank.org/indicator/SL.AGR.EMPL.ZS
  "SL.SRV.EMPL.ZS", # Employment in services (% of total employment): https://data.worldbank.org/indicator/SL.SRV.EMPL.ZS
  "SL.EMP.SELF.ZS", # Self-employed (% of total employment): https://data.worldbank.org/indicator/SL.EMP.SELF.ZS
  "SL.UEM.NEET.ZS", # Share of youth not in education, employment of training (% of youth population): https://data.worldbank.org/indicator/SL.UEM.NEET.ZS
  "NV.IND.TOTL.ZS", # Industry (including construction), value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.TOTL.ZS
  "NV.IND.MANF.ZS", # Manufacturing, value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.MANF.ZS
  "BN.CAB.XOKA.GD.ZS", # Current account balance (% of GDP): https://data.worldbank.org/indicator/BN.CAB.XOKA.GD.ZS
  "SP.POP.TOTL", # Population: https://data.worldbank.org/indicator/SP.POP.TOTL
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
  "fdi_net_inflow_GDP", # Foreign direct investment, net inflows (% of GDP): https://data.worldbank.org/indicator/BX.KLT.DINV.WD.GD.ZS
  "fdi_net_outflow_GDP", # Foreign direct investment, net outflows (% of GDP): https://data.worldbank.org/indicator/BM.KLT.DINV.WD.GD.ZS
  "tax_rev_total_GDP", # Tax revenue (% of GDP): https://data.worldbank.org/indicator/GC.TAX.TOTL.GD.ZS
  "tax_rev_trade_TAX", # Taxes on international trade (% of revenue): https://data.worldbank.org/indicator/GC.TAX.INTT.RV.ZS
  "tax_rev_inc_profits_TAX", # Taxes on income, profits and capital gains (% of revenue): https://data.worldbank.org/indicator/GC.TAX.YPKG.RV.ZS
  "trade_total_GDP", # Trade is the sum of exports and imports of goods and services measured as a share of gross domestic product: https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS
  "trade_exp_GDP", # Exports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.EXP.GNFS.ZS
  "trade_imp_GDP", # Imports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.IMP.GNFS.ZS
  "RD_expend_GDP", # Research and development expenditure (% of GDP): https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS
  "RD_scientists", # The number of researchers engaged in Research &Development (R&D), expressed as per million: SP.POP.SCIE.RD.P6
  "wgi_accountability", # WGI: Voice and Accountability; WGI home: https://info.worldbank.org/governance/wgi/#home API: https://api.worldbank.org/v2/sources/3/indicators
  "wgi_regul_quality", # WGI: Regulatory Quality
  "wgi_rule_of_law", # WGI: Rule of Law
  "wgi_pol_stability", # WGI: Political Stability and Absence of Violence/Terrorism
  "wgi_gov_effectvn", # WGI: Government Effectiveness
  "wgi_control_corrupt", # WGI: Control of Corruption
  "empl_ind", # Employment in industry (% of total employment): https://data.worldbank.org/indicator/sl.ind.empl.zs
  "empl_agr", # Employment in agriculture (% of total employment): https://data.worldbank.org/indicator/SL.AGR.EMPL.ZS
  "empl_serv", # Employment in services (% of total employment): https://data.worldbank.org/indicator/SL.SRV.EMPL.ZS
  "empl_self", # Self-employed (% of total employment): https://data.worldbank.org/indicator/SL.EMP.SELF.ZS
  "unemp_youth_neet", # Share of youth not in education, employment of training (% of youth population): https://data.worldbank.org/indicator/SL.UEM.NEET.ZS
  "VA_industry_gdp", # Industry (including construction), value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.TOTL.ZS
  "VA_manufct_gdp", # Manufacturing, value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.MANF.ZS
  "current_account_GDP_WB", # Current account balance (% of GDP): https://data.worldbank.org/indicator/BN.CAB.XOKA.GD.ZS
  "population", # Population: https://data.worldbank.org/indicator/SP.POP.TOTL
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
    country, "country.name", "iso3c", warn = FALSE
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

aggregates_2be_eliminated <- c(
  "European Union", "European Union excluding UK",
  "European Union (15 countries)", "Euro area",
  "Euro area (12 countries)", "EU15 (including D_W West-Germany)",
  "EA12 (including D_W West-Germany)"
)

# unemployment---------------------------------------------------------------
# Remark: two observations exist for 1991 for Germany and West Germany;
# Here the mean is used
print("...ameco01...")
ameco01 <- data.table::fread("data-raw/ameco/AMECO1.TXT",
                             fill = TRUE, header = TRUE,
                             stringsAsFactors = FALSE)
ameco01 <- ameco01[
  TITLE=="Unemployment rate: total :- Member States: definition EUROSTAT"
  ][
    !COUNTRY %in% aggregates_2be_eliminated
  ]

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

ameco01 <- ameco01[!COUNTRY %in% c("Germany", "West Germany")]
ameco01[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
ameco01 <- ameco01[, COUNTRY2:=countrycode::countrycode(COUNTRY,
                                                        "country.name", "iso3c"
)
][!is.na(COUNTRY)][!is.na(COUNTRY2)][, COUNTRY:=NULL]
data.table::setnames(ameco01, old = "COUNTRY2", new = "COUNTRY")

ameco01 <- data.table::melt(ameco01, id.vars=c("COUNTRY"),
                            variable.name="year",
                            value.name = "unemp_rate")

ameco01 <- rbind(ameco01, ameco01_germany)
ameco01[, unemp_rate:=as.double(as.character(unemp_rate))]
if (sum(duplicated(ameco01, by = c("COUNTRY", "year")))>0){
  warning("Duplicated rows in ameco01!")
}

# Harmonised consumer price index (All-items) (2015 = 100)-------------------
print("...ameco02...")
ameco02 <- data.table::fread("data-raw/ameco/AMECO2.TXT",
                             fill = TRUE, header = TRUE)
ameco02 <- ameco02[
  TITLE=="Harmonised consumer price index (All-items)"][
    !COUNTRY %in% aggregates_2be_eliminated
  ]
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
  TITLE%in%c("Gross fixed capital formation at current prices: total economy")
  ][
    !COUNTRY %in% aggregates_2be_eliminated
  ]
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
  ][
    !COUNTRY %in% aggregates_2be_eliminated
    ]

# Ameco 7--------------------------------------------------------------------
print("...ameco07...")
ameco07 <- data.table::fread("data-raw/ameco/AMECO7.TXT",
                             fill = TRUE, header = TRUE)

# Wage share-----------------------------------------------------------------
print("...wage share...")
ameco07_wage_share <- ameco07[
  TITLE=="Adjusted wage share: total economy: as percentage of GDP at current prices (Compensation per employee as percentage of GDP at market prices per person employed.)"
  ][
    !COUNTRY %in% aggregates_2be_eliminated
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
][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
data.table::setnames(ameco07_wage_share, old = "COUNTRY2", new = "COUNTRY")

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
  ][
    !COUNTRY%in%aggregates_2be_eliminated
    ][
      !COUNTRY %in% c(
        'EU15 (including DEL "linked" Germany)',
        'EA12 (including DEL "linked" Germany)',
        'EU15 (including DEL "linked" Germany)',
        'EA12 (including DEL "linked" Germany)'
      )
    ]

# for: ameco07_rulc
ameco07_rulc[ , COUNTRY:=countrycode::countrycode(
  COUNTRY, "country.name", "iso3c")]
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
  ][
    !COUNTRY%in%aggregates_2be_eliminated
    ][
      !COUNTRY %in% c(
        'EU15 (including DEL "linked" Germany)',
        'EA12 (including DEL "linked" Germany)',
        'EU15 (including DEL "linked" Germany)',
        'EA12 (including DEL "linked" Germany)'
      )
      ]
ameco07_nulc[ , COUNTRY:=countrycode::countrycode(
  COUNTRY, "country.name", "iso3c")]
ameco07_nulc[, c("CODE", "SUB-CHAPTER", "TITLE", "V67"):=NULL]
ameco07_nulc <- data.table::melt(ameco07_nulc,
                                 id.vars=c("COUNTRY", "UNIT"),
                                 variable.name="year",
                                 value.name = "nulc")
ameco07_nulc <- data.table::dcast(ameco07_nulc, COUNTRY+year~UNIT, value.var="nulc")
data.table::setnames(ameco07_nulc,
                     old = c("COUNTRY", "year", "(EUR: 2010 = 100)",
                             "(National currency: 2010 = 100)"),
                     new = c("COUNTRY", "year", "nulc_eur", "nulc_lcu")
                     )
if (sum(duplicated(ameco07_nulc, by = c("COUNTRY", "year")))>0){
  warning("Duplicated rows in ameco07_nulc!")
}

# Current account--------------------------------------------------------------
print("...Current Account...")
ameco10 <- data.table::fread("data-raw/ameco/AMECO10.TXT",
                             fill = TRUE, header = TRUE)
ameco10 <- ameco10[
  TITLE=="Balance on current transactions with the rest of the world (National accounts)" &
    UNIT=="(Percentage of gross domestic product at current prices)"
  ][
    !COUNTRY%in%aggregates_2be_eliminated
    ]

ameco10_germany <-data.table::copy(ameco10)
ameco10_germany <- ameco10_germany[
  COUNTRY %in% c("Germany", "West Germany")]

ameco10_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]

ameco10_germany <- data.table::melt(
  ameco10_germany, id.vars=c("COUNTRY"),
  variable.name="year",
  value.name = "current_account_GDP_ameco")

ameco10_germany[, year:=as.double(as.character(year))]
ameco10_germany[COUNTRY=="West Germany" & year>1990, current_account_GDP_ameco:=NA]
ameco10_germany <- ameco10_germany[, COUNTRY:=countrycode::countrycode(
  COUNTRY, "country.name", "iso3c"
  )]
ameco10_germany[, current_account_GDP_ameco:=mean(current_account_GDP_ameco, na.rm = T), year]
ameco10_germany <- unique(ameco10_germany)

ameco10[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
ameco10 <- ameco10[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
)
][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
data.table::setnames(ameco10, old = "COUNTRY2", new = "COUNTRY")

ameco10 <- data.table::melt(ameco10,
                                       id.vars=c("COUNTRY"),
                                       variable.name="year",
                                       value.name = "current_account_GDP_ameco")

ameco10 <- rbind(ameco10, ameco10_germany)
ameco10[, current_account_GDP_ameco:=as.double(current_account_GDP_ameco)]
if (sum(duplicated(ameco10, by = c("COUNTRY", "year")))>0){
  warning("Duplicated rows in ameco10!")
}

# Merge all AMECO tables-----------------------------------------------------
print("...merge all AMECO...")
ameco_full <- Reduce(function(...) merge(..., all=TRUE,
                                         by = c("COUNTRY", "year")),
                     list(ameco01, ameco02, ameco03, ameco07_wage_share,
                          ameco07_rulc, ameco07_nulc, ameco10))
ameco_full <- ameco_full[, .(year=as.double(as.character(year)),
                             iso3c=COUNTRY,
                             cap_form,
                             cpi,
                             current_account_GDP_ameco,
                             nulc_eur,
                             nulc_lcu,
                             rulc,
                             unemp_rate,
                             wage_share)
                         ]
print("....finished.")
# TODO check for duplicates

# Add Barro Lee education data=================================================
print("Barro-Lee educational data...")
barro_lee_url <- "http://www.barrolee.com/data/BL_v2.2/BL2013_MF1599_v2.2.csv"
barro_lee_file <- "data-raw/barro_lee.csv"
if (download_data | !file.exists(barro_lee_file)){
  tmp <- tempfile(fileext = ".csv")
  download.file(barro_lee_url, tmp,
                quiet = FALSE)
  barro_lee_raw <- data.table::fread(tmp)
  barro_lee_raw <- barro_lee_raw[, .(year, WBcode, lsc, lhc, yr_sch)]
  data.table::fwrite(barro_lee_raw, barro_lee_file)
} else {
  barro_lee_raw <- data.table::fread(barro_lee_file)
}
barro_lee <- barro_lee_raw[, .(iso3c=countrycode::countrycode(WBcode,
                                                              "wb", "iso3c"),
                               year=as.double(year),
                               school_agv_yrs=as.double(yr_sch),
                               school_share_sec=as.double(lsc),
                               school_share_sec=as.double(lhc))
                           ][
                             iso3c %in% countries_considered
                             ]
print("finished.")

# Complexity data==============================================================
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

# # Get export data from MIT===================================================
# # https://atlas.media.mit.edu/en/resources/data/
# if (download_data_exports_mit==TRUE){
#   export_data_mit_file_name <- "data/mit_export_data.fst" # TODO in zip ändern
#   if (update_data){
#     web_link_mit <- "https://atlas.media.mit.edu/static/db/raw/year_origin_sitc_rev2.tsv.bz2"
#     web_link_countries <- "https://atlas.media.mit.edu/static/db/raw/country_names.tsv.bz2"
#     mit_country_names <- as.data.frame(fread(web_link_countries))
#     export_data_raw <- fread(web_link_mit,
#                              colClasses = c("double", rep("character", 2),
#                                             rep("double", 4)),
#                              select = c("year", "origin", "sitc",
#                                         "export_val"))
#     export_data_raw[, location_code:=countrycode(
#       countrycode(origin, "id_3char", "name",
#                   custom_dict = mit_country_names),
#       "country.name", "iso3c")] # TODO Fix location code
#     export_data_raw <- export_data_raw[location_code %in% countrycode(
#       countries_considered, "iso2c", "iso3c"),
#       .(year, export_value=export_val, location_code, sitc_product_code=sitc)]
#     fst::write.fst(x = export_data_raw,
#                    path = export_data_mit_file_name, compress = 100)
#   } else{
#     export_data_raw <- fst::read.fst(export_data_mit_file_name,
#                                      as.data.table = T)
#   }
# }
#
# # Get export data from Harvard=================================================
# # http://atlas.cid.harvard.edu/downloads
# if (download_data_exports_harv==TRUE){
#   export_data_file_name <- "data/hrvd_complexity_atlas.fst" # TODO in zip umwandeln
#   if (update_data){
#     web_link <- "https://intl-atlas-downloads.s3.amazonaws.com/country_sitcproduct4digit_year.csv.zip"
#     export_data_raw <- fread(cmd = paste0("curl ", web_link, " | funzip"),
#                              colClasses = c(rep("double", 11),
#                                             rep("character", 4)),
#                              select = c("year", "export_value",
#                                         "location_code", "sitc_product_code"))
#     export_data_raw <- export_data_raw[location_code%in%countrycode(
#       countries_considered, "iso2c", "iso3c")]
#     fst::write.fst(x = export_data_raw,
#                    path = export_data_file_name, compress = 100)
#   } else{
#     export_data_raw <- fst::read.fst(export_data_file_name,
#                                      as.data.table = T)
#   }
# }
# export_data_raw[, year:=as.double(year)
#                 ][, export_value:=as.double(export_value)
#                   ][, total_exports:=sum(export_value, na.rm = T),
#                     .(location_code, year)]
# TODO Das noch kombinieren und Exportdatan separat speichern
# weiter unten dann nur Komplexitätswerte nehmen
# Die aber vielleicht sowieso immer erheben


# Merge data===================================================================
# TODO Testen ob es keine Dopplungen gibt

print("Merging data...")
macro_data <- Reduce(function(...) merge(..., all=TRUE,
                                         by = c("iso3c", "year")),
                     list(wb_data, swiid_raw, ameco_full, oecd_data,
                          complexity_data, barro_lee)
                     )
save(macro_data, file = "data/macro_data.rdata")
data.table::fwrite(macro_data, file = "data/macro_data.csv")
print("finished.")
