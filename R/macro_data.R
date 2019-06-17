# #' @import data.table
#'
#' #' Test uniqueness of data table
#' #'
#' #' Tests whether a data.table has unique rows.
#' #'
#' #' @param data_table A data frame of data table of which uniqueness should
#' #'  be tested.
#' #' @param index_vars Vector of strings, which specify the columns of
#' #'  data_table according to which uniqueness should be tested
#' #'  (e.g. country and year).
#' #' @return TRUE if data_table is unique, FALSE and a warning if it is not.
#' test_uniqueness <- function(data_table, index_vars, print_pos=TRUE){
#'   data_table <- data.table::as.data.table(data_table)
#'   if (nrow(data_table)!=data.table::uniqueN(data_table, by = index_vars)){
#'     warning(paste0("Rows in the data.table: ", nrow(data_table),
#'                    ", rows in the unique data.table:",
#'                    data.table::uniqueN(data_table, by = index_vars)))
#'     return(FALSE)
#'   } else {
#'     if (print_pos){
#'       print(paste0("No duplicates in ", as.list(sys.call()[[2]])))
#'     }
#'     return(TRUE)
#'   }
#' }

#' Unfactor a factor
#'
#' Transforms a factor into an integer
#'
#' Transforms a factor into an integer by first transforming it into character
#'
#' @param x An input that is potentially a factor
#' @return x as an integer
unfactor <- function(x){
  y <- as.integer(as.character(x))
  return(y)
}

#' Get countries
#'
#' Returns a pre-specified country list
#'
#' @param countries_to_get A code for a pre-specified country list
#' @return A vector with country codes
get_countries <- function(countries_to_get){

  pre_spec_country_lists <- list(
    "EU" = c("AUT", "BEL", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA",
             "GBR", "GRC", "HUN", "IRL", "ITA", "LUX", "LVA", "NLD", "POL",
             "PRT", "SVK", "SVN", "SWE")
  )

  if (length(countries_to_get)==1 &
      countries_to_get %in% names(pre_spec_country_lists)
      ){
    return(pre_spec_country_lists[[countries_to_get]])
  } else{
    warning(
      paste0("No pre-spec country list found. Use country codes provided: ",
             countries_to_get)
      )
    return(countries_to_get)
  }
}
if (!exists("download_data")){
  download_data <- FALSE
}
# print(download_data)
# # download_data <- T
#
# countries_considered <- countrycode::countrycode(strsplit(
#   "LU, SE, FI, DK, FR, NL, BE, SI, DE, AT, LV, EE, SK, CZ, PL, HU, GB, IE, PT, GR, ES, IT",
#   ", ")[[1]], "iso2c", "iso3c")
#
# first_year <- 1962
# last_year <- 2018
# skip_data <- c()
#
# # Eurostat data on government bond yields======================================
# eurostat_file_name <- "data-raw/eurostat_bond_data.csv"
#
# if (download_data | !file.exists((paste0(eurostat_file_name, ".gz")))){
#   if (!download_data){
#     warning("File for Eurostat data does not exist. Download from www...")
#   }
#   eurostat_id <- "irt_lt_mcby_a"
#   eurostat_bond_data_raw <- eurostat::get_eurostat(
#     eurostat_id,
#     time_format ="num",
#     filters = list(
#       geo=countrycode::countrycode(countries_considered, "iso3c", "eurostat"),
#       time=first_year:last_year)
#   )
#   eurostat_bond_data_raw <- data.table::as.data.table(eurostat_bond_data_raw)
#   eurostat_bond_data_raw <- eurostat_bond_data_raw[, .(
#     iso3c=countrycode::countrycode(geo, "eurostat", "iso3c"),
#     year=unfactor(time),
#     bond_yield=values)]
#
#   test_uniqueness(eurostat_bond_data_raw, c("iso3c", "year"))
#
#   data.table::fwrite(eurostat_bond_data_raw, eurostat_file_name)
#   R.utils::gzip(paste0(eurostat_file_name),
#                 destname=paste0(eurostat_file_name, ".gz"),
#                 overwrite = TRUE, remove = TRUE)
# } else {
#   eurostat_bond_data_raw <- data.table::fread(paste0(eurostat_file_name, ".gz"))
# }

# OECD data====================================================================
#
# oecd_debt_file_name <- "data-raw/oecd_debt_data.csv"
# oecd_debt_vars <- c("DBTS1GDP", "DBTS11GDP", "DBTS12GDP",
#                     "DBTS13GDP", "DBTS14_S15GDI")
#
# if (download_data | !file.exists((paste0(oecd_debt_file_name, ".gz")))){
#   if (!download_data){
#     warning("File for OECD debt data does not exist. Download from www...")
#   }
#   filter_list <- list(countries_considered,
#                       oecd_debt_vars
#   )
#
#   oecd_debt_data_raw <- OECD::get_dataset(dataset = "FIN_IND_FBS",
#                                           start_time = first_year,
#                                           end_time = last_year,
#                                           filter = filter_list)
#   oecd_debt_data_raw <- dplyr::select(oecd_debt_data_raw,
#                                       -dplyr::one_of(
#                                         "TIME_FORMAT", "UNIT", "POWERCODE")
#   )
#   test_uniqueness(oecd_debt_data_raw, c("LOCATION", "INDICATOR", "obsTime"))
#   data.table::fwrite(oecd_debt_data_raw, oecd_debt_file_name)
#   R.utils::gzip(paste0(oecd_debt_file_name),
#                 destname=paste0(oecd_debt_file_name, ".gz"),
#                 overwrite = TRUE)
#   oecd_debt_data_raw <- data.table::as.data.table(oecd_debt_data_raw)
# } else {
#   oecd_debt_data_raw <- data.table::fread(paste0(oecd_debt_file_name, ".gz"))
# }
#
# oecd_debt_data <- data.table::dcast(oecd_debt_data_raw,
#                                     LOCATION + obsTime ~ INDICATOR,
#                                     value.var="obsValue")
# old_names <- c("LOCATION", "obsTime", oecd_debt_vars)
# new_names <- c("iso3c", "year",
#                "total_debt_percGDP",
#                "debt_corp_nf_percGDP",
#                "debt_corp_f_percGDP",
#                "debt_gen_gov_percGDP",
#                "debt_hh_npish_percGDI")
# data.table::setnames(oecd_debt_data, old = old_names, new = new_names)
# oecd_debt_data[,
#                (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
#                .SDcols = setdiff(new_names, "iso3c")
#                ]
#
# # OECD: Public debt to GDP-----------------------------------------------------
# oecd_pub_debt_file_name <- "data-raw/oecd_pub_debt_data.csv"
#
# if (download_data | !file.exists(paste0(oecd_pub_debt_file_name, ".gz"))){
#   if (!download_data){
#     warning(
#       warning("File for OECD public debt data does not exist. Download from www...")
#     )
#   }
#   filter_list <- list(
#     countries_considered,
#     "DBTS13GDP"
#   )
#
#   oecd_pub_debt_data_raw <- OECD::get_dataset(dataset = "NAAG",
#                                           start_time = first_year,
#                                           end_time = last_year,
#                                           filter = filter_list)
#   oecd_pub_debt_data_raw <- data.table::as.data.table(oecd_pub_debt_data_raw)
#   oecd_pub_debt_data <- oecd_pub_debt_data_raw[, .(LOCATION, obsTime, obsValue)]
#   test_uniqueness(oecd_pub_debt_data, c("LOCATION", "obsTime"))
#   data.table::fwrite(oecd_pub_debt_data, oecd_pub_debt_file_name)
#   R.utils::gzip(paste0(oecd_pub_debt_file_name),
#                 destname=paste0(oecd_pub_debt_file_name, ".gz"),
#                 overwrite = TRUE)
# } else {
#   oecd_pub_debt_data <- data.table::fread(
#     paste0(oecd_pub_debt_file_name, ".gz")
#     )
# }
#
# old_names <- c("LOCATION", "obsTime", "obsValue")
# new_names <- c("iso3c", "year", "debt_gen_gvt_gross")
# data.table::setnames(oecd_pub_debt_data, old = old_names, new = new_names)
# oecd_pub_debt_data[,
#                    (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
#                    .SDcols = setdiff(new_names, "iso3c")
#                    ]
#
# # OECD finance data-------------------------------------------------------
# oecd_finance_file_name <- "data-raw/oecd_finance_data.csv"
#
# if (download_data | !file.exists(paste0(oecd_finance_file_name, ".gz"))){
#   if (!download_data){
#     warning(
#       "File for OECD finance data does not exist. Download from www..."
#     )
#   }
#   filter_list <- list(
#     "IRLT",
#     countries_considered,
#     "A")
#
#   oecd_finance_data_raw <- OECD::get_dataset(dataset = "MEI_FIN",
#                                              start_time = first_year,
#                                              end_time = last_year,
#                                              filter = filter_list)
#   oecd_finance_data_raw <- data.table::as.data.table(oecd_finance_data_raw)
#   oecd_finance_data <- oecd_finance_data_raw[,
#                                              .(LOCATION, obsTime, obsValue)
#                                              ]
#   test_uniqueness(oecd_finance_data, c("LOCATION", "obsTime"))
#
#   data.table::fwrite(oecd_finance_data,
#                      oecd_finance_file_name)
#   R.utils::gzip(paste0(oecd_finance_file_name),
#                 destname=paste0(oecd_finance_file_name, ".gz"),
#                 overwrite = TRUE)
# } else {
#   oecd_finance_data <- data.table::fread(paste0(oecd_finance_file_name, ".gz"))
# }
#
# old_names <- c("LOCATION", "obsTime", "obsValue")
# new_names <- c("iso3c", "year",
#                "interest_long_term")
# data.table::setnames(oecd_finance_data, old = old_names, new = new_names)
# oecd_finance_data[,
#                   (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
#                   .SDcols = setdiff(new_names, "iso3c")
#                   ]
#
# # OECD: Average wages----------------------------------------------------------
# oecd_wage_data_raw_file <- "data-raw/oecd_wage_data.csv"
#
# if (download_data | !file.exists(paste0(oecd_wage_data_raw_file, ".gz"))){
#   if (!download_data){
#     warning(
#       "File for OECD wage data does not exist. Download from www..."
#     )
#   }
#   filter_list <- list(
#     countries_considered,
#     c("USDPPP")
#   )
#
#   oecd_wage_data_raw <- OECD::get_dataset(dataset = "AV_AN_WAGE",
#                                           start_time = first_year,
#                                           end_time=last_year,
#                                           filter = filter_list)
#
#   oecd_wage_data_raw <- data.table::as.data.table(oecd_wage_data_raw)
#   oecd_wage_data <- oecd_wage_data_raw[, .(COUNTRY, obsTime, obsValue)]
#   test_uniqueness(oecd_wage_data, c("COUNTRY", "obsTime"))
#
#   data.table::fwrite(oecd_wage_data,
#                      oecd_wage_data_raw_file)
#   R.utils::gzip(paste0(oecd_wage_data_raw_file),
#                 destname=paste0(oecd_wage_data_raw_file, ".gz"),
#                 overwrite = TRUE)
# } else {
#   oecd_wage_data <- data.table::fread(paste0(oecd_wage_data_raw_file, ".gz"))
# }
#
# old_names <- c("COUNTRY", "obsTime", "obsValue")
# new_names <- c("iso3c", "year",
#                "average_wages")
# data.table::setnames(oecd_wage_data, old = old_names, new = new_names)
# oecd_wage_data[,
#                   (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
#                   .SDcols = setdiff(new_names, "iso3c")
#                   ]
#
#
# # Merge OECD data--------------------------------------------------------------
# oecd_data <- Reduce(function(...) merge(..., all=TRUE,
#                                            by = c("iso3c", "year")),
#                        list(oecd_finance_data, oecd_debt_data, oecd_wage_data,
#                             oecd_pub_debt_data)
#   )
# stopifnot(test_uniqueness(oecd_data, c("iso3c", "year")))

# World Bank data==============================================================
# print("World Bank data...")
# # TODO Add export_GDP
# # TODO Download all countries and filter those we do not need
# #
# wb_vars <- c(
#   "BX.KLT.DINV.WD.GD.ZS", # Foreign direct investment, net inflows (% of GDP): https://data.worldbank.org/indicator/BX.KLT.DINV.WD.GD.ZS
#   "BM.KLT.DINV.WD.GD.ZS", # Foreign direct investment, net outflows (% of GDP): https://data.worldbank.org/indicator/BM.KLT.DINV.WD.GD.ZS
#   "GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP): https://data.worldbank.org/indicator/GC.TAX.TOTL.GD.ZS
#   "GC.TAX.INTT.RV.ZS", # Taxes on international trade (% of revenue): https://data.worldbank.org/indicator/GC.TAX.INTT.RV.ZS
#   "GC.TAX.YPKG.RV.ZS", # Taxes on income, profits and capital gains (% of revenue): https://data.worldbank.org/indicator/GC.TAX.YPKG.RV.ZS
#   "NE.TRD.GNFS.ZS", # Trade is the sum of exports and imports of goods and services measured as a share of gross domestic product: https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS
#   "NE.EXP.GNFS.ZS", # Exports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.EXP.GNFS.ZS
#   "NE.IMP.GNFS.ZS", # Imports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.IMP.GNFS.ZS
#   "GB.XPD.RSDV.GD.ZS", # Research and development expenditure (% of GDP): https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS
#   "SP.POP.SCIE.RD.P6", # The number of researchers engaged in Research &Development (R&D), expressed as per million: SP.POP.SCIE.RD.P6
#   "VA.EST", # WGI: Voice and Accountability; WGI home: https://info.worldbank.org/governance/wgi/#home API: https://api.worldbank.org/v2/sources/3/indicators
#   "RQ.EST", # WGI: Regulatory Quality
#   "RL.EST", # WGI: Rule of Law
#   "PV.EST", # WGI: Political Stability and Absence of Violence/Terrorism
#   "GE.EST", # WGI: Government Effectiveness
#   "CC.EST", # WGI: Control of Corruption
#   "sl.ind.empl.zs", # Employment in industry (% of total employment): https://data.worldbank.org/indicator/sl.ind.empl.zs
#   "SL.AGR.EMPL.ZS", # Employment in agriculture (% of total employment): https://data.worldbank.org/indicator/SL.AGR.EMPL.ZS
#   "SL.SRV.EMPL.ZS", # Employment in services (% of total employment): https://data.worldbank.org/indicator/SL.SRV.EMPL.ZS
#   "SL.EMP.SELF.ZS", # Self-employed (% of total employment): https://data.worldbank.org/indicator/SL.EMP.SELF.ZS
#   "SL.UEM.NEET.ZS", # Share of youth not in education, employment of training (% of youth population): https://data.worldbank.org/indicator/SL.UEM.NEET.ZS
#   "NV.IND.TOTL.ZS", # Industry (including construction), value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.TOTL.ZS
#   "NV.IND.MANF.ZS", # Manufacturing, value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.MANF.ZS
#   "BN.CAB.XOKA.GD.ZS", # Current account balance (% of GDP): https://data.worldbank.org/indicator/BN.CAB.XOKA.GD.ZS
#   "SP.POP.TOTL", # Population: https://data.worldbank.org/indicator/SP.POP.TOTL
#   "ny.gdp.totl.rt.zs", # Natural resource rents: https://data.worldbank.org/indicator/ny.gdp.totl.rt.zs
#   "NY.GDP.MKTP.KN", # Real GDP (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KN
#   "NY.GDP.MKTP.KD.ZG", # Real GDP growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
#   "NY.GDP.PCAP.KN", # Real GDP per capita (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KN
#   "NY.GDP.PCAP.KD.ZG", # Real GDP per capita growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD.ZG
#   "NY.GDP.MKTP.KD", # Real GDP (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
#   "NY.GDP.PCAP.KD", # Real GDP per capita (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
#   "NY.GDP.MKTP.CN", # Nominal GDP (current LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.CN
#   "NY.GDP.PCAP.CN", # Nominal GDP per capita (current LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.CN
#   "NY.GDP.MKTP.CD", # Nominal GDP (current US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
#   "NY.GDP.PCAP.CD", # Nominal GDP per capita (current US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
#   "NY.GDP.MKTP.PP.KD", # GDP, PPP (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD
#   "NY.GDP.PCAP.PP.KD", # GDP, PPP per capita (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
#   "NY.GDP.MKTP.PP.CD", # GDP, PPP (current int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD
#   "NY.GDP.PCAP.PP.CD", # GDP, PPP per capita (current int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
#   "SL.UEM.TOTL.ZS" # Unemployment rate from World Bank: https://data.worldbank.org/indicator/SL.UEM.TOTL.ZS
# )
#
# wb_var_names <- c(
#   "fdi_net_inflow_GDP", # Foreign direct investment, net inflows (% of GDP): https://data.worldbank.org/indicator/BX.KLT.DINV.WD.GD.ZS
#   "fdi_net_outflow_GDP", # Foreign direct investment, net outflows (% of GDP): https://data.worldbank.org/indicator/BM.KLT.DINV.WD.GD.ZS
#   "tax_rev_total_GDP", # Tax revenue (% of GDP): https://data.worldbank.org/indicator/GC.TAX.TOTL.GD.ZS
#   "tax_rev_trade_TAX", # Taxes on international trade (% of revenue): https://data.worldbank.org/indicator/GC.TAX.INTT.RV.ZS
#   "tax_rev_inc_profits_TAX", # Taxes on income, profits and capital gains (% of revenue): https://data.worldbank.org/indicator/GC.TAX.YPKG.RV.ZS
#   "trade_total_GDP", # Trade is the sum of exports and imports of goods and services measured as a share of gross domestic product: https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS
#   "trade_exp_GDP", # Exports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.EXP.GNFS.ZS
#   "trade_imp_GDP", # Imports of goods and services (% of GDP): https://data.worldbank.org/indicator/NE.IMP.GNFS.ZS
#   "RD_expend_GDP", # Research and development expenditure (% of GDP): https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS
#   "RD_scientists", # The number of researchers engaged in Research &Development (R&D), expressed as per million: SP.POP.SCIE.RD.P6
#   "wgi_accountability", # WGI: Voice and Accountability; WGI home: https://info.worldbank.org/governance/wgi/#home API: https://api.worldbank.org/v2/sources/3/indicators
#   "wgi_regul_quality", # WGI: Regulatory Quality
#   "wgi_rule_of_law", # WGI: Rule of Law
#   "wgi_pol_stability", # WGI: Political Stability and Absence of Violence/Terrorism
#   "wgi_gov_effectvn", # WGI: Government Effectiveness
#   "wgi_control_corrupt", # WGI: Control of Corruption
#   "empl_ind", # Employment in industry (% of total employment): https://data.worldbank.org/indicator/sl.ind.empl.zs
#   "empl_agr", # Employment in agriculture (% of total employment): https://data.worldbank.org/indicator/SL.AGR.EMPL.ZS
#   "empl_serv", # Employment in services (% of total employment): https://data.worldbank.org/indicator/SL.SRV.EMPL.ZS
#   "empl_self", # Self-employed (% of total employment): https://data.worldbank.org/indicator/SL.EMP.SELF.ZS
#   "unemp_youth_neet", # Share of youth not in education, employment of training (% of youth population): https://data.worldbank.org/indicator/SL.UEM.NEET.ZS
#   "VA_industry_gdp", # Industry (including construction), value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.TOTL.ZS
#   "VA_manufct_gdp", # Manufacturing, value added (% of GDP): https://data.worldbank.org/indicator/NV.IND.MANF.ZS
#   "current_account_GDP_WB", # Current account balance (% of GDP): https://data.worldbank.org/indicator/BN.CAB.XOKA.GD.ZS
#   "population", # Population: https://data.worldbank.org/indicator/SP.POP.TOTL
#   "res_rents", # Natural resource rents: https://data.worldbank.org/indicator/ny.gdp.totl.rt.zs
#   "gdp_real_lcu", # Real GDP (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KN
#   "gdp_real_lcu_growth", # Real GDP growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
#   "gdp_real_pc_lcu", # Real GDP per capita (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KN
#   "gdp_real_pc_lcu_growth", # Real GDP per capita growth (constant LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD.ZG
#   "gdp_real_usd", # Real GDP (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
#   "gdp_real_pc_usd", # Real GDP per capita (constant 2010 US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
#   "gdp_nom_lcu", # Nominal GDP (current LCU): https://data.worldbank.org/indicator/NY.GDP.MKTP.CN
#   "gdp_nom_pc_lcu", # Nominal GDP per capita (current LCU): https://data.worldbank.org/indicator/NY.GDP.PCAP.CN
#   "gdp_nom_usd", # Nominal GDP (current US$): https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
#   "gdp_nom_pc_usd", # Nominal GDP per capita (current US$): https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
#   "gdp_real_ppp", # GDP, PPP (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD
#   "gdp_real_pc_ppp", # GDP, PPP per capita (constant 2011 int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
#   "gdp_nom_ppp", # GDP, PPP (current int $): https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD
#   "gdp_nom_pc_ppp", # GDP, PPP per capita (current int $): https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
#   "unemp_rate_wb" # Unemployment rate from World Bank: https://data.worldbank.org/indicator/SL.UEM.TOTL.ZS
# )
#
# wb_vars_2 <- c(
#   "sl.tlf.totl.in", # Total labor force:  https://data.worldbank.org/indicator/SL.TLF.TOTL.IN
#   "FP.CPI.TOTL", # https://data.worldbank.org/indicator/FP.CPI.TOTL
#   "FP.CPI.TOTL.ZG", # Inflation: https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
#   "FR.INR.RINR" # https://data.worldbank.org/indicator/FR.INR.RINR
# )
#
# wb_var_names_2 <- c(
#   "labor_force_total", # Total labor force:  https://data.worldbank.org/indicator/SL.TLF.TOTL.IN
#   "cpi_wb", # https://data.worldbank.org/indicator/FP.CPI.TOTL
#   "cpi_change_wb", # Inflation: https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
#   "interest_real" # https://data.worldbank.org/indicator/FR.INR.RINR
# )
#
# wb_file_name <- "data-raw/wb_data.csv"
# if (download_data | !file.exists(paste0(wb_file_name, ".gz"))){
#   if (!download_data){
#     warning("File for World Bank data does not exist. Download from www...")
#   }
#   wb_raw_data <- data.table::as.data.table(
#     WDI::WDI(country = countrycode::countrycode(countries_considered,
#                                                 "iso3c", "iso2c"),
#              indicator = wb_vars,
#              start = first_year, end = last_year)
#   )
#   wb_raw_data_2 <- data.table::as.data.table(
#     WDI::WDI(country = countries_considered,
#              indicator = wb_vars_2,
#              start = first_year, end = last_year)
#   )
#
#   wb_raw_data <- wb_raw_data[wb_raw_data_2, on=c("iso2c", "country", "year")]
#
#   data.table::fwrite(wb_raw_data, wb_file_name)
#   R.utils::gzip(paste0(wb_file_name),
#                 destname=paste0(wb_file_name, ".gz"),
#                 overwrite = TRUE, remove = TRUE)
# } else {
#   wb_raw_data <- data.table::fread(paste0(wb_file_name, ".gz"))
# }
#
# data.table::setnames(wb_raw_data, old = c(wb_vars, wb_vars_2),
#                      new = c(wb_var_names, wb_var_names_2))
# wb_data <- wb_raw_data[, iso3c:=countrycode::countrycode(iso2c,
#                                                            "iso2c", "iso3c")
#                          ][, c("iso2c", "country"):=NULL]
# stopifnot(test_uniqueness(wb_data, c("year", "iso3c")))
# print("finished.")

# Lane-Milesi-Ferreti data on financial openness===============================
# print("LMF data...")
# warning("LMF data is not updated automatically. Currently using 1970-2011 data (May 2019).")
#
# lmf_file <- "data-raw/lmf.csv.gz"
#
# if (!file.exists(lmf_file)){
#   warning("LMF data not available in data-raw. This data must be downloaded
#           manually from http://www.philiplane.org/EWN.html")
# } else {
#   lmf_cols <- c(
#     "Year", "Country Name",
#     "Portfolio equity assets (stock)",
#     "Portfolio equity liabilities (stock)",
#     "Portfolio debt assets", "Portfolio debt liabilities",
#     "FDI assets (stock)",  "FDI assets (other)",
#     "FDI liabilities (stock)", "FDI liabilities (other)",
#     "Debt assets (stock)", "Debt liabilities (stock)",
#     "NFA", "NFA/GDP"
#   )
#   lmf_new_names <- c(
#     "year", "iso3c",
#     "lmf_port_eq_asst_stock",
#     "lmf_port_eq_liab_stock",
#     "lmf_port_dbt_asst", "lmf_port_dbt_liab",
#     "lmf_fdi_asst_stock", "lmf_fdi_asst_other",
#     "lmf_fdi_liab_stock", "lmf_fdi_liab_other",
#     "lmf_debt_asst_stock", "lmf_debt_liab_stock",
#     "lmf_nfa", "lmf_nfa_gdp"
#   )
#   lmf_raw <- data.table::fread(lmf_file, na.strings = "")
#   lmf_raw <- dplyr::select(lmf_raw, dplyr::one_of(lmf_cols))
#   lmf <- data.table::as.data.table(lmf_raw)
#   data.table::setnames(lmf, old = lmf_cols, new = lmf_new_names)
#   lmf[!iso3c%in%c("Central African Rep.", "Euro Area", "Kosovo"),
#       iso3c:=countrycode::countrycode(iso3c, "country.name", "iso3c")]
#   lmf <- lmf[iso3c %in% countries_considered]
#   lmf[, lmf_nfa_gdp:=gsub(pattern = "%",
#                           replacement = "",
#                           x = lmf_nfa_gdp)]
#   lmf[,
#       (setdiff(lmf_new_names, "iso3c")):= lapply(.SD, as.double),
#       .SDcols = setdiff(lmf_new_names, "iso3c")
#       ]
# }
# stopifnot(test_uniqueness(lmf, c("iso3c", "year")))
# print("finished.")

# Gini data from Solt==========================================================
# print("SWIID data...")
# swiid_link <- "https://dataverse.harvard.edu/api/access/datafile/3376371"
# swiid_file <- "data-raw/swiid8_0_summary.csv"
# swiid_origin_zip_file <- "swiid8_0/swiid8_0_summary.csv"
#
# if (!download_data & file.exists(paste0(swiid_file, ".gz"))){
#   swiid_raw <- data.table::fread(paste0(swiid_file, ".gz"))
# } else {
#   tmp <- tempfile(fileext = ".zip")
#   download.file(swiid_link, tmp,
#                 quiet = FALSE)
#   unzip(tmp, exdir = "data-raw", files = swiid_origin_zip_file)
#   file.rename(paste0("data-raw/", swiid_origin_zip_file), swiid_file)
#   unlink(paste0("data-raw/", strsplit(swiid_origin_zip_file, "/")[[1]][[1]]),
#          recursive = T)
#   swiid_raw <- data.table::fread(swiid_file)
#   swiid_raw <- swiid_raw[, country:=countrycode::countrycode(
#     country, "country.name", "iso3c", warn = FALSE
#   )][!is.na(country), .(iso3c=country, year=as.double(year),
#                         gini_post_tax=gini_disp, gini_pre_tax=gini_mkt)]
#   swiid_raw <- swiid_raw[iso3c%in%countries_considered]
#   swiid_raw <- unique(swiid_raw, by = c("iso3c", "year"))
#   stopifnot(test_uniqueness(swiid_raw, c("iso3c", "year")))
#   data.table::fwrite(swiid_raw, swiid_file)
#   R.utils::gzip(paste0(swiid_file),
#                 destname=paste0(swiid_file, ".gz"),
#                 overwrite = TRUE)
# }
# print("finished.")

# AMECO data===================================================================
# print("AMECO data...")
# ameco_link <- "http://ec.europa.eu/economy_finance/db_indicators/ameco/documents/ameco0.zip"
# ameco_file <- "data-raw/ameco/AMECO1.TXT.gz"
# ameco_file_dir <- "data-raw/ameco"
#
# if (download_data | !file.exists(ameco_file)){
#   tmp <- tempfile(fileext = ".zip")
#   download.file(ameco_link, tmp,
#                 quiet = FALSE)
#   if (file.exists(ameco_file)){
#     unlink(ameco_file_dir, recursive = TRUE)
#   }
#   unzip(tmp, exdir = ameco_file_dir)
#   for (f in list.files(ameco_file_dir)){
#     R.utils::gzip(paste0(ameco_file_dir, "/", f),
#                   destname=paste0(ameco_file_dir, "/", f, ".gz"),
#                   overwrite = TRUE)
#   }
# }
#
# aggregates_2be_eliminated <- c(
#   "European Union", "European Union excluding UK",
#   "European Union (15 countries)", "Euro area",
#   "Euro area (12 countries)", "EU15 (including D_W West-Germany)",
#   "EA12 (including D_W West-Germany)"
# )
#
# # unemployment---------------------------------------------------------------
# # Remark: two observations exist for 1991 for Germany and West Germany;
# # Here the mean is used
# print("...ameco01...")
# ameco01 <- data.table::fread("data-raw/ameco/AMECO1.TXT.gz",
#                              fill = TRUE, header = TRUE,
#                              stringsAsFactors = FALSE)
# ameco01_unemp <- ameco01[
#   TITLE%in%c("Unemployment rate: total :- Member States: definition EUROSTAT")
#   ][
#     !COUNTRY %in% aggregates_2be_eliminated
#   ]
#
# ameco01_unemp_germany <-data.table::copy(ameco01_unemp)
# ameco01_unemp_germany <- ameco01_unemp_germany[COUNTRY %in% c("Germany", "West Germany")]
#
# ameco01_unemp_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
#
# ameco01_unemp_germany <- data.table::melt(ameco01_unemp_germany, id.vars=c("COUNTRY"),
#                                     variable.name="year",
#                                     value.name = "unemp_rate")
# ameco01_unemp_germany[, year:=as.double(as.character(year))]
# ameco01_unemp_germany[COUNTRY=="West Germany" & year>1990, unemp_rate:=NA]
# ameco01_unemp_germany <- ameco01_unemp_germany[, COUNTRY:=countrycode::countrycode(COUNTRY,
#                                                                        "country.name", "iso3c"
# )]
# ameco01_unemp_germany[, unemp_rate:=mean(unemp_rate, na.rm = T), year]
# ameco01_unemp_germany <- unique(ameco01_unemp_germany)
#
# ameco01_unemp <- ameco01_unemp[!COUNTRY %in% c("Germany", "West Germany")]
# ameco01_unemp[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco01_unemp <- ameco01_unemp[, COUNTRY2:=countrycode::countrycode(COUNTRY,
#                                                         "country.name", "iso3c"
# )
# ][!is.na(COUNTRY)][!is.na(COUNTRY2)][, COUNTRY:=NULL]
# data.table::setnames(ameco01_unemp, old = "COUNTRY2", new = "COUNTRY")
#
# ameco01_unemp <- data.table::melt(ameco01_unemp, id.vars=c("COUNTRY"),
#                             variable.name="year",
#                             value.name = "unemp_rate")
#
# ameco01_unemp <- rbind(ameco01_unemp, ameco01_unemp_germany)
# ameco01_unemp[, unemp_rate:=as.double(as.character(unemp_rate))]
# ameco01_unemp[, year:=unfactor(year)]
# ameco01_unemp <- ameco01_unemp[COUNTRY%in%countries_considered]
#
# # Population-------------------------------------------------------------------
# ameco01_pop <- ameco01[
#   TITLE%in%c("Total population (National accounts)")
#   ][
#     !COUNTRY %in% aggregates_2be_eliminated
#     ]
#
# ameco01_pop_germany <-data.table::copy(ameco01_pop)
# ameco01_pop_germany <- ameco01_pop_germany[COUNTRY %in% c("Germany", "West Germany")]
#
# ameco01_pop_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
#
# ameco01_pop_germany <- data.table::melt(ameco01_pop_germany, id.vars=c("COUNTRY"),
#                                           variable.name="year",
#                                           value.name = "population_ameco")
# ameco01_pop_germany[, year:=as.double(as.character(year))]
# ameco01_pop_germany[COUNTRY=="West Germany" & year>1990, population_ameco:=NA]
# ameco01_pop_germany <- ameco01_pop_germany[, COUNTRY:=countrycode::countrycode(COUNTRY,
#                                                                                    "country.name", "iso3c"
# )]
# ameco01_pop_germany[, population_ameco:=mean(population_ameco, na.rm = T), year]
# ameco01_pop_germany <- unique(ameco01_pop_germany)
#
# ameco01_pop <- ameco01_pop[!COUNTRY %in% c("Germany", "West Germany")]
# ameco01_pop[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco01_pop <- ameco01_pop[, COUNTRY2:=countrycode::countrycode(COUNTRY,
#                                                                     "country.name", "iso3c"
# )
# ][!is.na(COUNTRY)][!is.na(COUNTRY2)][, COUNTRY:=NULL]
# data.table::setnames(ameco01_pop, old = "COUNTRY2", new = "COUNTRY")
#
# ameco01_pop <- data.table::melt(ameco01_pop, id.vars=c("COUNTRY"),
#                                   variable.name="year",
#                                   value.name = "population_ameco")
#
# ameco01_pop <- rbind(ameco01_pop, ameco01_pop_germany)
# ameco01_pop[, population_ameco:=as.double(as.character(population_ameco))]
# ameco01_pop[, year:=unfactor(year)]
# ameco01_pop <- ameco01_pop[COUNTRY%in%countries_considered]
#
# # Harmonised consumer price index (All-items) (2015 = 100)---------------------
# print("...ameco02...")
# ameco02 <- data.table::fread("data-raw/ameco/AMECO2.TXT.gz",
#                              fill = TRUE, header = TRUE)
# ameco02 <- ameco02[
#   TITLE=="Harmonised consumer price index (All-items)"][
#     !COUNTRY %in% aggregates_2be_eliminated
#   ]
# ameco02 <- ameco02[, COUNTRY:=countrycode::countrycode(COUNTRY,
#                                                        "country.name", "iso3c"
# )
# ][!is.na(COUNTRY)]
# ameco02[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
#
# ameco02 <- data.table::melt(ameco02, id.vars=c("COUNTRY"),
#                             variable.name="year",
#                             value.name = "cpi_harm")
# ameco02 <- ameco02[, year:=unfactor(year)]
# ameco02 <- ameco02[COUNTRY%in%countries_considered]
#
# # Capital formation----------------------------------------------------------
# # TODO Wir hatten: Real gross fixed capital formation / real net capital stock; aber welche Werte sind das?
# # capital_formation_real # 3
# # capital_stock_real # 3
# # Remark: after 1990 the values for the united Germany are used
# print("...ameco03...")
# ameco03 <- data.table::fread("data-raw/ameco/AMECO3.TXT.gz",
#                              fill = TRUE, header = TRUE)
# ameco03 <- ameco03[
#   TITLE%in%c("Gross fixed capital formation at current prices: total economy")
#   ][
#     !COUNTRY %in% aggregates_2be_eliminated
#   ]
# ameco03 <- ameco03[UNIT=="Mrd ECU/EUR"]
# ameco03_germany <- ameco03[COUNTRY %in% c("Germany", "West Germany")]
# ameco03_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco03_germany <- ameco03_germany[, COUNTRY:=countrycode::countrycode(COUNTRY,
#                                                                        "country.name", "iso3c"
# )
# ][!is.na(COUNTRY)]
#
# ameco03_germany <- data.table::melt(ameco03_germany, id.vars=c("COUNTRY"),
#                                     variable.name="year",
#                                     value.name = "cap_form")
# ameco03_germany[, cap_form:=mean(cap_form, na.rm=T), year]
#
# ameco03_germany <- unique(ameco03_germany)
#
# ameco03 <- ameco03[!COUNTRY %in% c("Germany", "West Germany")]
# ameco03[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco03 <- ameco03[, COUNTRY:=countrycode::countrycode(COUNTRY,
#                                                        "country.name", "iso3c"
# )
# ][!is.na(COUNTRY)]
# ameco03 <- data.table::melt(ameco03, id.vars=c("COUNTRY"),
#                             variable.name="year",
#                             value.name = "cap_form")
# ameco03 <- rbind(ameco03, ameco03_germany)
# ameco03 <- ameco03[, year:=unfactor(year)]
# ameco03 <- ameco03[COUNTRY%in%countries_considered]
#
# # GDP growth-------------------------------------------------------------------
# # TODO: Einheiten noch fixen, aber vielleicht besser von Weltbank wg coverage
# print("..ameco06..")
# ameco06 <- data.table::fread("data-raw/ameco/AMECO6.TXT.gz",
#                              fill = TRUE, header = TRUE)
# gdp_vars <- c(
#   "Gross domestic product at current prices",
#   "Gross domestic product at 2010 reference levels",
#   "Gross domestic product at current prices per head of population",
#   "Gross domestic product at 2010 reference levels per head of population"
# )
# ameco06_GDP <- ameco06[
#   TITLE%in%gdp_vars
#   ][
#     !COUNTRY %in% aggregates_2be_eliminated
#     ]
#
# # Ameco 7--------------------------------------------------------------------
# print("...ameco07...")
# ameco07 <- data.table::fread("data-raw/ameco/AMECO7.TXT.gz",
#                              fill = TRUE, header = TRUE)
#
# # Wage share-----------------------------------------------------------------
# print("...wage share...")
# ameco07_wage_share <- ameco07[
#   TITLE=="Adjusted wage share: total economy: as percentage of GDP at current prices (Compensation per employee as percentage of GDP at market prices per person employed.)"
#   ][
#     !COUNTRY %in% aggregates_2be_eliminated
#     ]
#
# ameco07_wage_share_germany <-data.table::copy(ameco07_wage_share)
# ameco07_wage_share_germany <- ameco07_wage_share_germany[
#   COUNTRY %in% c("Germany", "West Germany")]
#
# ameco07_wage_share_germany[, c("CODE", "SUB-CHAPTER", "TITLE",
#                                "UNIT",  "V67"):=NULL]
#
# ameco07_wage_share_germany <- data.table::melt(
#   ameco07_wage_share_germany, id.vars=c("COUNTRY"),
#   variable.name="year",
#   value.name = "wage_share")
#
# ameco07_wage_share_germany[, year:=as.double(as.character(year))]
# ameco07_wage_share_germany[COUNTRY=="West Germany" & year>1990, wage_share:=NA]
# ameco07_wage_share_germany <- ameco07_wage_share_germany[, COUNTRY:=countrycode::countrycode(COUNTRY,
#                                                                                              "country.name", "iso3c"
# )]
# ameco07_wage_share_germany[, wage_share:=mean(wage_share, na.rm = T), year]
# ameco07_wage_share_germany <- unique(ameco07_wage_share_germany)
#
# ameco07_wage_share[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco07_wage_share <- ameco07_wage_share[, COUNTRY2:=countrycode::countrycode(COUNTRY,
#                                                                               "country.name", "iso3c"
# )
# ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
# data.table::setnames(ameco07_wage_share, old = "COUNTRY2", new = "COUNTRY")
#
# ameco07_wage_share <- data.table::melt(ameco07_wage_share,
#                                        id.vars=c("COUNTRY"),
#                                        variable.name="year",
#                                        value.name = "wage_share")
#
# ameco07_wage_share <- rbind(ameco07_wage_share, ameco07_wage_share_germany)
# ameco07_wage_share[, wage_share:=as.double(wage_share)]
# ameco07_wage_share[, year:=unfactor(year)]
# ameco07_wage_share <- ameco07_wage_share[COUNTRY%in%countries_considered]
#
# # RULC-----------------------------------------------------------------------
# print("...RULC...")
# ameco07_rulc <- ameco07[
#   TITLE=="Real unit labour costs: total economy (Ratio of compensation per employee to nominal GDP per person employed.)"
#   ][
#     !COUNTRY%in%aggregates_2be_eliminated
#     ][
#       !COUNTRY %in% c(
#         'EU15 (including DEL "linked" Germany)',
#         'EA12 (including DEL "linked" Germany)',
#         'EU15 (including DEL "linked" Germany)',
#         'EA12 (including DEL "linked" Germany)'
#       )
#     ]
#
# # for: ameco07_rulc
# ameco07_rulc[ , COUNTRY:=countrycode::countrycode(
#   COUNTRY, "country.name", "iso3c")]
# ameco07_rulc[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco07_rulc <- data.table::melt(ameco07_rulc,
#                                  id.vars=c("COUNTRY"),
#                                  variable.name="year",
#                                  value.name = "rulc")
# ameco07_rulc <- ameco07_rulc[COUNTRY%in%countries_considered]
# ameco07_rulc[, year:=unfactor(year)]
#
# # NULC-----------------------------------------------------------------------
# print("...NULC...")
# ameco07_nulc <- ameco07[
#   TITLE=="Nominal unit labour costs: total economy (Ratio of compensation per employee to real GDP per person employed.)"
#   ][
#     !COUNTRY%in%aggregates_2be_eliminated
#     ][
#       !COUNTRY %in% c(
#         'EU15 (including DEL "linked" Germany)',
#         'EA12 (including DEL "linked" Germany)',
#         'EU15 (including DEL "linked" Germany)',
#         'EA12 (including DEL "linked" Germany)'
#       )
#       ]
# ameco07_nulc[ , COUNTRY:=countrycode::countrycode(
#   COUNTRY, "country.name", "iso3c")]
# ameco07_nulc[, c("CODE", "SUB-CHAPTER", "TITLE", "V67"):=NULL]
# ameco07_nulc <- data.table::melt(ameco07_nulc,
#                                  id.vars=c("COUNTRY", "UNIT"),
#                                  variable.name="year",
#                                  value.name = "nulc")
# ameco07_nulc <- data.table::dcast(ameco07_nulc, COUNTRY+year~UNIT, value.var="nulc")
# data.table::setnames(ameco07_nulc,
#                      old = c("COUNTRY", "year", "(EUR: 2010 = 100)",
#                              "(National currency: 2010 = 100)"),
#                      new = c("COUNTRY", "year", "nulc_eur", "nulc_lcu")
#                      )
# ameco07_nulc <- ameco07_nulc[COUNTRY%in%countries_considered]
# ameco07_nulc[, year:=unfactor(year)]
#
# # Current account--------------------------------------------------------------
# print("...Current Account...")
# ameco10 <- data.table::fread("data-raw/ameco/AMECO10.TXT.gz",
#                              fill = TRUE, header = TRUE)
# ameco10 <- ameco10[
#   TITLE=="Balance on current transactions with the rest of the world (National accounts)" &
#     UNIT=="(Percentage of gross domestic product at current prices)"
#   ][
#     !COUNTRY%in%aggregates_2be_eliminated
#     ]
#
# ameco10_germany <-data.table::copy(ameco10)
# ameco10_germany <- ameco10_germany[
#   COUNTRY %in% c("Germany", "West Germany")]
#
# ameco10_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
#
# ameco10_germany <- data.table::melt(
#   ameco10_germany, id.vars=c("COUNTRY"),
#   variable.name="year",
#   value.name = "current_account_GDP_ameco")
#
# ameco10_germany[, year:=as.double(as.character(year))]
# ameco10_germany[COUNTRY=="West Germany" & year>1990, current_account_GDP_ameco:=NA]
# ameco10_germany <- ameco10_germany[, COUNTRY:=countrycode::countrycode(
#   COUNTRY, "country.name", "iso3c"
#   )]
# ameco10_germany[, current_account_GDP_ameco:=mean(current_account_GDP_ameco, na.rm = T), year]
# ameco10_germany <- unique(ameco10_germany)
#
# ameco10[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco10 <- ameco10[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
# )
# ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
# data.table::setnames(ameco10, old = "COUNTRY2", new = "COUNTRY")
#
# ameco10 <- data.table::melt(ameco10,
#                                        id.vars=c("COUNTRY"),
#                                        variable.name="year",
#                                        value.name = "current_account_GDP_ameco")
#
# ameco10 <- rbind(ameco10, ameco10_germany)
# ameco10[, current_account_GDP_ameco:=as.double(current_account_GDP_ameco)]
# ameco10 <- ameco10[COUNTRY%in%countries_considered]
# ameco10[, year:=unfactor(year)]
#
# # Sectoral balances from AMECO-------------------------------------------------
# # Foreign sector:
# ameco10_sect_balances <- data.table::fread("data-raw/ameco/AMECO10.TXT.gz",
#                                            fill = TRUE, header = TRUE)
#
# ameco10_sect_balances <- ameco10_sect_balances[
#   TITLE=="Balance on current transactions with the rest of the world (National accounts)" &
#     UNIT=="(Percentage of gross domestic product at current prices)"
#   ][
#     !COUNTRY%in%aggregates_2be_eliminated
#     ]
#
# ameco10_sect_balances_germany <-data.table::copy(ameco10_sect_balances)
# ameco10_sect_balances_germany <- ameco10_sect_balances_germany[
#   COUNTRY %in% c("Germany", "West Germany")]
#
# ameco10_sect_balances_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
#
# ameco10_sect_balances_germany <- data.table::melt(
#   ameco10_sect_balances_germany, id.vars=c("COUNTRY"),
#   variable.name="year",
#   value.name = "sect_balance_foreign")
#
# ameco10_sect_balances_germany[, year:=as.double(as.character(year))]
# ameco10_sect_balances_germany[COUNTRY=="West Germany" & year>1990, sect_balance_foreign:=NA]
# ameco10_sect_balances_germany <- ameco10_sect_balances_germany[, COUNTRY:=countrycode::countrycode(
#   COUNTRY, "country.name", "iso3c"
# )]
# ameco10_sect_balances_germany[, sect_balance_foreign:=mean(sect_balance_foreign, na.rm = T), year]
# ameco10_sect_balances_germany <- unique(ameco10_sect_balances_germany)
#
# ameco10_sect_balances[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco10_sect_balances <- ameco10_sect_balances[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
# )
# ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
# data.table::setnames(ameco10_sect_balances, old = "COUNTRY2", new = "COUNTRY")
#
# ameco10_sect_balances <- data.table::melt(ameco10_sect_balances,
#                             id.vars=c("COUNTRY"),
#                             variable.name="year",
#                             value.name = "sect_balance_foreign")
#
# ameco10_sect_balances <- rbind(ameco10_sect_balances, ameco10_sect_balances_germany)
# ameco10_sect_balances[, sect_balance_foreign:=as.double(sect_balance_foreign)]
# ameco10_sect_balances[, sect_balance_foreign:=sect_balance_foreign*(-1)]
# ameco10_sect_balances[, year:=as.integer(as.character(year))]
# ameco10_sect_balances <- ameco10_sect_balances[year<=last_year & year>=first_year]
#
# # Government sector:
# ameco16_sect_balances <- data.table::fread("data-raw/ameco/AMECO16.TXT.gz",
#                                            fill = TRUE, header = TRUE)
#
# ameco16_sect_balances <- ameco16_sect_balances[
#   TITLE=="Net lending (+) or net borrowing (-): general government :- ESA 2010" &
#     UNIT=="(Percentage of GDP at current prices (excessive deficit procedure))"
#   ][
#     !COUNTRY%in%aggregates_2be_eliminated
#     ]
#
# ameco16_sect_balances_germany <-data.table::copy(ameco16_sect_balances)
# ameco16_sect_balances_germany <- ameco16_sect_balances_germany[
#   COUNTRY %in% c("Germany", "West Germany")]
#
# ameco16_sect_balances_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
#
# ameco16_sect_balances_germany <- data.table::melt(
#   ameco16_sect_balances_germany, id.vars=c("COUNTRY"),
#   variable.name="year",
#   value.name = "sect_balance_gvnt")
#
# ameco16_sect_balances_germany[, year:=as.double(as.character(year))]
# ameco16_sect_balances_germany[COUNTRY=="West Germany" & year>1990, sect_balance_gvnt:=NA]
# ameco16_sect_balances_germany <- ameco16_sect_balances_germany[, COUNTRY:=countrycode::countrycode(
#   COUNTRY, "country.name", "iso3c"
# )]
# ameco16_sect_balances_germany[, sect_balance_gvnt:=mean(sect_balance_gvnt, na.rm = T), year]
# ameco16_sect_balances_germany <- unique(ameco16_sect_balances_germany)
#
# ameco16_sect_balances[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco16_sect_balances <- ameco16_sect_balances[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
# )
# ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
# data.table::setnames(ameco16_sect_balances, old = "COUNTRY2", new = "COUNTRY")
#
# ameco16_sect_balances <- data.table::melt(ameco16_sect_balances,
#                                           id.vars=c("COUNTRY"),
#                                           variable.name="year",
#                                           value.name = "sect_balance_gvnt")
#
# ameco16_sect_balances <- rbind(ameco16_sect_balances, ameco16_sect_balances_germany)
# ameco16_sect_balances[, sect_balance_gvnt:=as.double(sect_balance_gvnt)]
# ameco16_sect_balances[, year:=as.double(as.character(year))]
# ameco16_sect_balances <- ameco16_sect_balances[year<=last_year & year>=first_year]
#
# # Private sector:
# # Step 1: get balance for corporations
# ameco14_sect_balances <- data.table::fread("data-raw/ameco/AMECO14.TXT.gz",
#                                            fill = TRUE, header = TRUE)
#
# ameco14_sect_balances <- ameco14_sect_balances[
#   TITLE=="Net lending (+) or net borrowing (-): corporations" & UNIT=="Mrd ECU/EUR"
#   ][
#     !COUNTRY%in%aggregates_2be_eliminated
#     ]
#
# ameco14_sect_balances_germany <-data.table::copy(ameco14_sect_balances)
# ameco14_sect_balances_germany <- ameco14_sect_balances_germany[
#   COUNTRY %in% c("Germany", "West Germany")]
#
# ameco14_sect_balances_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
#
# ameco14_sect_balances_germany <- data.table::melt(
#   ameco14_sect_balances_germany, id.vars=c("COUNTRY"),
#   variable.name="year",
#   value.name = "sect_balance_corp_abs")
#
# ameco14_sect_balances_germany[, year:=as.double(as.character(year))]
# ameco14_sect_balances_germany[COUNTRY=="West Germany" & year>1990, sect_balance_corp_abs:=NA]
# ameco14_sect_balances_germany <- ameco14_sect_balances_germany[, COUNTRY:=countrycode::countrycode(
#   COUNTRY, "country.name", "iso3c"
# )]
# ameco14_sect_balances_germany[, sect_balance_corp_abs:=mean(sect_balance_corp_abs, na.rm = T), year]
# ameco14_sect_balances_germany <- unique(ameco14_sect_balances_germany)
#
# ameco14_sect_balances[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco14_sect_balances <- ameco14_sect_balances[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
# )
# ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
# data.table::setnames(ameco14_sect_balances, old = "COUNTRY2", new = "COUNTRY")
#
# ameco14_sect_balances <- data.table::melt(ameco14_sect_balances,
#                                           id.vars=c("COUNTRY"),
#                                           variable.name="year",
#                                           value.name = "sect_balance_corp_abs")
#
# ameco14_sect_balances <- rbind(ameco14_sect_balances, ameco14_sect_balances_germany)
# ameco14_sect_balances[, sect_balance_corp_abs:=as.double(sect_balance_corp_abs)]
# ameco14_sect_balances[, year:=as.double(as.character(year))]
# ameco14_sect_balances <- ameco14_sect_balances[year<=last_year & year>=first_year]
#
# # Step 2: get balances for households
# ameco15_sect_balances <- data.table::fread("data-raw/ameco/AMECO15.TXT.gz",
#                                            fill = TRUE, header = TRUE)
#
# ameco15_sect_balances <- ameco15_sect_balances[
#   TITLE=="Net lending (+) or net borrowing (-): households and NPISH" & UNIT=="Mrd ECU/EUR"
#   ][
#     !COUNTRY%in%aggregates_2be_eliminated
#     ]
#
# ameco15_sect_balances_germany <-data.table::copy(ameco15_sect_balances)
# ameco15_sect_balances_germany <- ameco15_sect_balances_germany[
#   COUNTRY %in% c("Germany", "West Germany")]
#
# ameco15_sect_balances_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
#
# ameco15_sect_balances_germany <- data.table::melt(
#   ameco15_sect_balances_germany, id.vars=c("COUNTRY"),
#   variable.name="year",
#   value.name = "sect_balance_HH_abs")
#
# ameco15_sect_balances_germany[, year:=as.double(as.character(year))]
# ameco15_sect_balances_germany[COUNTRY=="West Germany" & year>1990, sect_balance_HH_abs:=NA]
# ameco15_sect_balances_germany <- ameco15_sect_balances_germany[, COUNTRY:=countrycode::countrycode(
#   COUNTRY, "country.name", "iso3c"
# )]
# ameco15_sect_balances_germany[, sect_balance_HH_abs:=mean(sect_balance_HH_abs, na.rm = T), year]
# ameco15_sect_balances_germany <- unique(ameco15_sect_balances_germany)
#
# ameco15_sect_balances[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco15_sect_balances <- ameco15_sect_balances[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
# )
# ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
# data.table::setnames(ameco15_sect_balances, old = "COUNTRY2", new = "COUNTRY")
#
# ameco15_sect_balances <- data.table::melt(ameco15_sect_balances,
#                                           id.vars=c("COUNTRY"),
#                                           variable.name="year",
#                                           value.name = "sect_balance_HH_abs")
#
# ameco15_sect_balances <- rbind(ameco15_sect_balances, ameco15_sect_balances_germany)
# ameco15_sect_balances[, sect_balance_HH_abs:=as.double(sect_balance_HH_abs)]
# ameco15_sect_balances[, year:=as.double(as.character(year))]
# ameco15_sect_balances <- ameco15_sect_balances[year<=last_year & year>=first_year]
#
# # Step 3: get GDP at current prices for normalization--------------------------
# ameco06_sect_balances <- data.table::fread("data-raw/ameco/AMECO6.TXT.gz",
#                              fill = TRUE, header = TRUE)
#
# ameco06_sect_balances <- ameco06_sect_balances[
#   TITLE=="Gross domestic product at current prices" & UNIT=="Mrd ECU/EUR"
#   ][
#     !COUNTRY%in%aggregates_2be_eliminated
#     ]
#
#
# ameco06_sect_balances_germany <-data.table::copy(ameco06_sect_balances)
# ameco06_sect_balances_germany <- ameco06_sect_balances_germany[
#   COUNTRY %in% c("Germany", "West Germany")]
#
# ameco06_sect_balances_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
#
# ameco06_sect_balances_germany <- data.table::melt(
#   ameco06_sect_balances_germany, id.vars=c("COUNTRY"),
#   variable.name="year",
#   value.name = "GDP_cp")
#
# ameco06_sect_balances_germany[, year:=as.double(as.character(year))]
# ameco06_sect_balances_germany[COUNTRY=="West Germany" & year>1990, GDP_cp:=NA]
# ameco06_sect_balances_germany <- ameco06_sect_balances_germany[, COUNTRY:=countrycode::countrycode(
#   COUNTRY, "country.name", "iso3c"
# )]
# ameco06_sect_balances_germany[, GDP_cp:=mean(GDP_cp, na.rm = T), year]
# ameco06_sect_balances_germany <- unique(ameco06_sect_balances_germany)
#
# ameco06_sect_balances[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
# ameco06_sect_balances <- ameco06_sect_balances[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
# )
# ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
# data.table::setnames(ameco06_sect_balances, old = "COUNTRY2", new = "COUNTRY")
#
# ameco06_sect_balances <- data.table::melt(ameco06_sect_balances,
#                                           id.vars=c("COUNTRY"),
#                                           variable.name="year",
#                                           value.name = "GDP_cp")
#
# ameco06_sect_balances <- rbind(ameco06_sect_balances, ameco06_sect_balances_germany)
# ameco06_sect_balances[, GDP_cp:=as.double(GDP_cp)]
# ameco06_sect_balances[, year:=as.double(as.character(year))]
# ameco06_sect_balances <- ameco06_sect_balances[year<=last_year & year>=first_year]
#
# # Step 4: divide corporate and household value by GDP
# ameco_sect_balances <- Reduce(function(...) merge(..., all=TRUE,
#                                          by = c("COUNTRY", "year")),
#                      list(ameco16_sect_balances,
#                           ameco10_sect_balances,
#                           ameco14_sect_balances,
#                           ameco15_sect_balances,
#                           ameco06_sect_balances)
#                      )
# ameco_sect_balances <- ameco_sect_balances[, .(COUNTRY, year,
#                                                sect_balance_gvnt,
#                                                sect_balance_foreign,
#                                                sect_balance_corp_abs,
#                                                sect_balance_HH_abs, GDP_cp)]
# ameco_sect_balances[, sect_balance_priv_corp:=(sect_balance_corp_abs/GDP_cp)*100]
# ameco_sect_balances[, sect_balance_priv_HH:=(sect_balance_HH_abs/GDP_cp)*100]
# ameco_sect_balances[, sect_balance_priv:=sect_balance_priv_corp+sect_balance_priv_HH]
# ameco_sect_balances[, balance_test:=sect_balance_priv+sect_balance_gvnt+sect_balance_foreign]
# ameco_sect_balances[, year:=as.integer(year)]
# ameco_sect_balances[, c("sect_balance_corp_abs", "balance_test",
#                         "sect_balance_HH_abs", "GDP_cp"):=NULL]
# ameco_sect_balances <- ameco_sect_balances[COUNTRY%in%countries_considered]
# stopifnot(test_uniqueness(ameco_sect_balances, c("COUNTRY", "year")))
#
#
#
#
# # Merge all AMECO tables-------------------------------------------------------
# print("...merge all AMECO...")
# print("...test for duplicates in individual ameco parts...")
# lapply(list(ameco01_pop, ameco01_unemp, ameco02, ameco03,
#             ameco07_wage_share, ameco07_rulc, ameco07_nulc,
#             ameco10, ameco_sect_balances), test_uniqueness, c("COUNTRY", "year"))
#
# ameco_full <- Reduce(function(...) merge(..., all=TRUE,
#                                          by = c("COUNTRY", "year")),
#                      list(ameco01_pop, ameco01_unemp, ameco02, ameco03,
#                           ameco07_wage_share, ameco07_rulc, ameco07_nulc,
#                           ameco10, ameco_sect_balances)
# )
#
#
# ameco_full <- ameco_full[, .(year=as.double(as.character(year)),
#                              iso3c=COUNTRY,
#                              cap_form,
#                              cpi_harm,
#                              current_account_GDP_ameco,
#                              nulc_eur,
#                              nulc_lcu,
#                              population_ameco,
#                              rulc,
#                              unemp_rate,
#                              wage_share,
#                              sect_balance_gvnt,
#                              sect_balance_foreign,
#                              sect_balance_priv_corp,
#                              sect_balance_priv_HH,
#                              sect_balance_priv)
#                          ]
# print("...test for uniqueness of ameco_full...")
# stopifnot(test_uniqueness(ameco_full, c("iso3c", "year")))
# print("....finished.")

# Chinn-Ito index==============================================================
# print("Chinn-Ito index...")
# chinn_ito_url <- "http://web.pdx.edu/~ito/kaopen_2016.dta"
# chinn_ito_file <- "data-raw/chinn_ito.csv"
#
# if (download_data | !file.exists((paste0(chinn_ito_file, ".gz")))){
#   tmp <- tempfile(fileext = ".dta")
#   download.file(chinn_ito_url, tmp,
#                 quiet = FALSE)
#   chinn_ito_raw <- data.table::as.data.table(haven::read_dta(tmp))
#
#   chinn_ito_raw <- chinn_ito_raw[ccode %in% countries_considered,
#                      .(ccode, year,
#                        kaopen, # Chinn-Ito index
#                        ka_open # Normalized Chinn-Ito index
#                        )
#                      ]
#   data.table::fwrite(chinn_ito_raw, chinn_ito_file)
#   R.utils::gzip(paste0(chinn_ito_file),
#                 destname=paste0(chinn_ito_file, ".gz"),
#                 overwrite = TRUE)
# } else {
#   chinn_ito_raw <- data.table::fread(paste0(chinn_ito_file, ".gz"))
# }
# chinn_ito <- chinn_ito_raw[,
#                            .(
#                              iso3c = ccode,
#                              year = as.double(year),
#                              chinn_ito = as.double(kaopen),
#                              chinn_ito_normed = as.double(ka_open)
#                            )]
#
# print("finished.")

# KOF Globalization index======================================================
# print("KOF Globalization Index...")
# kof_url <- "https://www.ethz.ch/content/dam/ethz/special-interest/dual/kof-dam/documents/Globalization/2018/Data_2018_2.dta"
# kof_file <- "data-raw/kof.csv"
#
# if (download_data | !file.exists((paste0(kof_file, ".gz")))){
#   warning("KOF data is not updated automatically.
#           Currently downloading file Data_2018_2.dta (May 2019).")
#   tmp <- tempfile(fileext = ".dta")
#   download.file(kof_url, tmp,
#                 quiet = FALSE)
#   kof_raw <- data.table::as.data.table(haven::read_dta(tmp))
#   kof_raw <- kof_raw[code %in% countries_considered,
#                      .(code, year,
#                        KOFGI, # KOF Globalisation Index
#                        KOFGIdf, # "KOF Globalisation Index, de facto
#                        KOFGIdj, # KOF Globalisation Index, de jure
#                        KOFEcGI, # KOF Economic Globalisation Index
#                        KOFEcGIdf, # KOF Economic Globalisation Index, de facto
#                        KOFEcGIdj, # KOF Economic Globalisation Index, de jure
#                        KOFTrGI, # KOF Trade Globalisation Index
#                        KOFTrGIdf, # KOF Trade Globalisation Index, de facto
#                        KOFTrGIdj, # KOF Trade Globalisation Index, de jure
#                        KOFFiGI, # KOF Financial Globalisation Index
#                        KOFFiGIdf, # KOF Financial Globalisation Index, de facto
#                        KOFFiGIdj # KOF Financial Globalisation Index, de jure
#                      )]
#   data.table::fwrite(kof_raw, kof_file)
#   R.utils::gzip(paste0(kof_file),
#                 destname=paste0(kof_file, ".gz"),
#                 overwrite = TRUE)
# } else {
#   kof_raw <- data.table::fread(paste0(kof_file, ".gz"))
# }
#
# kof <- kof_raw[, .(
#   iso3c = code,
#   year = as.double(year),
#   kof_G = as.double(KOFGI), # KOF Globalisation Index
#   kof_G_df = as.double(KOFGIdf), # "KOF Globalisation Index, de facto
#   kof_G_dj = as.double(KOFGIdj), # KOF Globalisation Index, de jure
#   kof_EcG = as.double(KOFEcGI), # KOF Economic Globalisation Index
#   kof_EcG_df = as.double(KOFEcGIdf), # KOF Economic Globalisation Index, de facto
#   kof_EcG_dj = as.double(KOFEcGIdj), # KOF Economic Globalisation Index, de jure
#   kof_trade = as.double(KOFTrGI), # KOF Trade Globalisation Index
#   kof_trade_df = as.double(KOFTrGIdf), # KOF Trade Globalisation Index, de facto
#   kof_trade_dj = as.double(KOFTrGIdj), # KOF Trade Globalisation Index, de jure
#   kof_fin = as.double(KOFFiGI), # KOF Financial Globalisation Index
#   kof_fin_df = as.double(KOFFiGIdf), # KOF Financial Globalisation Index, de facto
#   kof_fin_dj = as.double(KOFFiGIdj) # KOF Financial Globalisation Index, de jure
#   )]
# print("finished.")

# Barro Lee education data=====================================================
# print("Barro-Lee educational data...")
# barro_lee_url <- "http://www.barrolee.com/data/BL_v2.2/BL2013_MF1599_v2.2.csv"
# barro_lee_file <- "data-raw/barro_lee.csv"
# if (download_data | !file.exists((paste0(barro_lee_file, ".gz")))){
#   tmp <- tempfile(fileext = ".csv")
#   download.file(barro_lee_url, tmp,
#                 quiet = FALSE)
#   barro_lee_raw <- data.table::fread(tmp)
#   barro_lee_raw <- barro_lee_raw[, .(year, WBcode, lsc, lhc, yr_sch)]
#   data.table::fwrite(barro_lee_raw, barro_lee_file)
#   R.utils::gzip(paste0(barro_lee_file),
#                 destname=paste0(barro_lee_file, ".gz"),
#                 overwrite = TRUE)
# } else {
#   barro_lee_raw <- data.table::fread(paste0(barro_lee_file, ".gz"))
# }
# barro_lee <- barro_lee_raw[!WBcode%in%c("REU", "ROM", "SER"),
#                            .(iso3c=countrycode::countrycode(WBcode,
#                                                             "wb", "iso3c"),
#                                year=as.double(year),
#                                school_agv_yrs=as.double(yr_sch),
#                                school_share_sec=as.double(lsc),
#                                school_share_ter=as.double(lhc))
#                            ][
#                              iso3c %in% countries_considered
#                              ]
# print("finished.")

# Complexity data==============================================================
# print("Complexity data...")
# complexity_harvard_url <- "https://intl-atlas-downloads.s3.amazonaws.com/country_sitcproductsection_year.csv.zip"
# complexity_harv_file_name <- "data-raw/complexity_harv.csv"
# complexity_harv_origin_zip_file <- "country_sitcproductsection_year.csv"
#
# if (!download_data & file.exists(paste0(complexity_harv_file_name, ".gz"))){
#   complexity_harv <- data.table::fread(
#     paste0(complexity_harv_file_name, ".gz")
#     )
# } else {
#   tmp <- tempfile(fileext = ".zip")
#   download.file(complexity_harvard_url,
#                 tmp,
#                 quiet = FALSE)
#   unzip(tmp, exdir = "data-raw", files = complexity_harv_origin_zip_file)
#   file.rename(paste0("data-raw/", complexity_harv_origin_zip_file),
#               complexity_harv_file_name)
#   unlink(paste0("data-raw/", complexity_harv_origin_zip_file),
#          recursive = T)
#   complexity_harv_raw <- data.table::fread(complexity_harv_file_name)
#   complexity_harv <- complexity_harv_raw[
#     !is.na(countrycode::countrycode(location_code, "iso3c", "country.name",
#                                     warn = FALSE)),
#     .(year, hs_eci, hs_coi, sitc_eci, sitc_coi, location_code)]
#   complexity_harv <- unique(complexity_harv, by = c("year", "location_code"))
#   data.table::fwrite(x = complexity_harv, file = complexity_harv_file_name)
#   R.utils::gzip(paste0(complexity_harv_file_name),
#                 destname=paste0(complexity_harv_file_name, ".gz"),
#                 overwrite = TRUE)
# }
#
# complexity_mit_url <- "https://atlas.media.mit.edu/en/rankings/country/eci/?download=true&download_all=true"
# complexity_mit_country_names_url <- "https://atlas.media.mit.edu/static/db/raw/country_names.tsv.bz2"
# complexity_mit_file_name <- "data-raw/complexity_mit.csv"
# complexity_mit_origin_zip_file <- "country_sitcproductsection_year.csv"
#
# if (!download_data & file.exists(paste0(complexity_harv_file_name, ".gz"))){
#   complexity_mit <- data.table::fread(
#     paste0(complexity_mit_file_name, ".gz")
#   )
# } else {
#   tmp <- tempfile(fileext = ".csv")
#   download.file(complexity_mit_url,
#                 tmp,
#                 quiet = FALSE)
#   complexity_mit_raw <- data.table::fread(tmp)
#
#   tmp2 <- tempfile(fileext = ".bz2")
#   download.file(complexity_mit_country_names_url,
#                 tmp2,
#                 quiet = FALSE)
#   complexity_mit_country_names <- data.table::fread(tmp2)
#
#   complexity_mit <- data.table::copy(complexity_mit_raw)
#
#   complexity_mit[, iso3c:=toupper(countrycode::countrycode(
#     Country, "name", "id_3char",
#     custom_dict = as.data.frame(complexity_mit_country_names)))
#     ][, c("Country", "Country ID"):=NULL]
#
#   data.table::fwrite(x = complexity_mit, file = complexity_mit_file_name)
#   R.utils::gzip(paste0(complexity_mit_file_name),
#                 destname=paste0(complexity_mit_file_name, ".gz"),
#                 overwrite = TRUE)
# }
#
# complexity_data <- data.table::as.data.table(
#   dplyr::full_join(
#     complexity_mit[iso3c %in% countries_considered],
#     complexity_harv[location_code %in% countries_considered],
#     by = c("Year"="year", "iso3c" = "location_code")
#   )
# )
# data.table::setnames(complexity_data,
#                      old = c("Year", "ECI", "ECI+", "iso3c", "hs_eci",
#                              "hs_coi", "sitc_eci", "sitc_coi"),
#                      new = c("year", "eci_mit", "eci_mit_plus", "iso3c",
#                              "eci_harv_hs", "eci_harc_coi_hs",
#                              "eci_harv_sitc", "eci_harc_coi_sitc"))
# print("finished.")

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
                     list(wb_data, swiid_raw, ameco_full, oecd_data, lmf,
                          complexity_data, barro_lee, kof, chinn_ito,
                          eurostat_bond_data_raw)
                     )
test_uniqueness(macro_data, c("iso3c", "year"))
save(macro_data, file = "data/macro_data.rdata")
macro_data_csv_name <- "data/macro_data.csv"
data.table::fwrite(macro_data, file = macro_data_csv_name)
R.utils::gzip(paste0(macro_data_csv_name),
              destname=paste0(macro_data_csv_name, ".gz"),
              overwrite = TRUE)
print("finished.")
