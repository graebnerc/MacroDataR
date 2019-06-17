#' Get AMECO data
#'
#' Downloads and assembles data from the EU AMECO database.
#'
#' @param download_data If TRUE, data will be downloaded from the internet
#' @param countries_considered Vector of iso3c codes of the countries for
#'  which data should be assembled.
#' @param first_year First year for which data is to be collected (numeric).
#' @param last_year Last year for which data is to be collected (numeric).
#' @family download_helpers
get_ameco <- function(download_data, countries_considered,
                      first_year, last_year){
  print("AMECO data...")
  ameco_link <- "http://ec.europa.eu/economy_finance/db_indicators/ameco/documents/ameco0.zip"
  ameco_file <- "data-raw/ameco/AMECO1.TXT.gz"
  ameco_file_dir <- "data-raw/ameco"

  if (download_data | !file.exists(ameco_file)){
    tmp <- tempfile(fileext = ".zip")
    download.file(ameco_link, tmp,
                  quiet = FALSE)
    if (file.exists(ameco_file)){
      unlink(ameco_file_dir, recursive = TRUE)
    }
    unzip(tmp, exdir = ameco_file_dir)
    for (f in list.files(ameco_file_dir)){
      R.utils::gzip(paste0(ameco_file_dir, "/", f),
                    destname=paste0(ameco_file_dir, "/", f, ".gz"),
                    overwrite = TRUE)
    }
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
  ameco01 <- data.table::fread("data-raw/ameco/AMECO1.TXT.gz",
                               fill = TRUE, header = TRUE,
                               stringsAsFactors = FALSE)
  ameco01_unemp <- ameco01[
    TITLE%in%c("Unemployment rate: total :- Member States: definition EUROSTAT")
    ][
      !COUNTRY %in% aggregates_2be_eliminated
      ]

  ameco01_unemp_germany <-data.table::copy(ameco01_unemp)
  ameco01_unemp_germany <- ameco01_unemp_germany[COUNTRY %in% c("Germany", "West Germany")]

  ameco01_unemp_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]

  ameco01_unemp_germany <- data.table::melt(ameco01_unemp_germany, id.vars=c("COUNTRY"),
                                            variable.name="year",
                                            value.name = "unemp_rate")
  ameco01_unemp_germany[, year:=as.double(as.character(year))]
  ameco01_unemp_germany[COUNTRY=="West Germany" & year>1990, unemp_rate:=NA]
  ameco01_unemp_germany <- ameco01_unemp_germany[, COUNTRY:=countrycode::countrycode(COUNTRY,
                                                                                     "country.name", "iso3c"
  )]
  ameco01_unemp_germany[, unemp_rate:=mean(unemp_rate, na.rm = T), year]
  ameco01_unemp_germany <- unique(ameco01_unemp_germany)

  ameco01_unemp <- ameco01_unemp[!COUNTRY %in% c("Germany", "West Germany")]
  ameco01_unemp[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
  ameco01_unemp <- ameco01_unemp[, COUNTRY2:=countrycode::countrycode(COUNTRY,
                                                                      "country.name", "iso3c"
  )
  ][!is.na(COUNTRY)][!is.na(COUNTRY2)][, COUNTRY:=NULL]
  data.table::setnames(ameco01_unemp, old = "COUNTRY2", new = "COUNTRY")

  ameco01_unemp <- data.table::melt(ameco01_unemp, id.vars=c("COUNTRY"),
                                    variable.name="year",
                                    value.name = "unemp_rate")

  ameco01_unemp <- rbind(ameco01_unemp, ameco01_unemp_germany)
  ameco01_unemp[, unemp_rate:=as.double(as.character(unemp_rate))]
  ameco01_unemp[, year:=unfactor(year)]
  ameco01_unemp <- ameco01_unemp[COUNTRY%in%countries_considered]

  # Population-------------------------------------------------------------------
  ameco01_pop <- ameco01[
    TITLE%in%c("Total population (National accounts)")
    ][
      !COUNTRY %in% aggregates_2be_eliminated
      ]

  ameco01_pop_germany <-data.table::copy(ameco01_pop)
  ameco01_pop_germany <- ameco01_pop_germany[COUNTRY %in% c("Germany", "West Germany")]

  ameco01_pop_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]

  ameco01_pop_germany <- data.table::melt(ameco01_pop_germany, id.vars=c("COUNTRY"),
                                          variable.name="year",
                                          value.name = "population_ameco")
  ameco01_pop_germany[, year:=as.double(as.character(year))]
  ameco01_pop_germany[COUNTRY=="West Germany" & year>1990, population_ameco:=NA]
  ameco01_pop_germany <- ameco01_pop_germany[, COUNTRY:=countrycode::countrycode(COUNTRY,
                                                                                 "country.name", "iso3c"
  )]
  ameco01_pop_germany[, population_ameco:=mean(population_ameco, na.rm = T), year]
  ameco01_pop_germany <- unique(ameco01_pop_germany)

  ameco01_pop <- ameco01_pop[!COUNTRY %in% c("Germany", "West Germany")]
  ameco01_pop[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
  ameco01_pop <- ameco01_pop[, COUNTRY2:=countrycode::countrycode(COUNTRY,
                                                                  "country.name", "iso3c"
  )
  ][!is.na(COUNTRY)][!is.na(COUNTRY2)][, COUNTRY:=NULL]
  data.table::setnames(ameco01_pop, old = "COUNTRY2", new = "COUNTRY")

  ameco01_pop <- data.table::melt(ameco01_pop, id.vars=c("COUNTRY"),
                                  variable.name="year",
                                  value.name = "population_ameco")

  ameco01_pop <- rbind(ameco01_pop, ameco01_pop_germany)
  ameco01_pop[, population_ameco:=as.double(as.character(population_ameco))]
  ameco01_pop[, year:=unfactor(year)]
  ameco01_pop <- ameco01_pop[COUNTRY%in%countries_considered]

  # Harmonised consumer price index (All-items) (2015 = 100)---------------------
  print("...ameco02...")
  ameco02 <- data.table::fread("data-raw/ameco/AMECO2.TXT.gz",
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
                              value.name = "cpi_harm")
  ameco02 <- ameco02[, year:=unfactor(year)]
  ameco02 <- ameco02[COUNTRY%in%countries_considered]

  # Capital formation----------------------------------------------------------
  # TODO Wir hatten: Real gross fixed capital formation / real net capital stock; aber welche Werte sind das?
  # capital_formation_real # 3
  # capital_stock_real # 3
  # Remark: after 1990 the values for the united Germany are used
  print("...ameco03...")
  ameco03 <- data.table::fread("data-raw/ameco/AMECO3.TXT.gz",
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
  ameco03 <- ameco03[, year:=unfactor(year)]
  ameco03 <- ameco03[COUNTRY%in%countries_considered]

  # GDP growth-------------------------------------------------------------------
  # TODO: Einheiten noch fixen, aber vielleicht besser von Weltbank wg coverage
  print("..ameco06..")
  ameco06 <- data.table::fread("data-raw/ameco/AMECO6.TXT.gz",
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
  ameco07 <- data.table::fread("data-raw/ameco/AMECO7.TXT.gz",
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
  ameco07_wage_share[, year:=unfactor(year)]
  ameco07_wage_share <- ameco07_wage_share[COUNTRY%in%countries_considered]

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
  ameco07_rulc <- ameco07_rulc[COUNTRY%in%countries_considered]
  ameco07_rulc[, year:=unfactor(year)]

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
  ameco07_nulc <- ameco07_nulc[COUNTRY%in%countries_considered]
  ameco07_nulc[, year:=unfactor(year)]

  # Current account--------------------------------------------------------------
  print("...Current Account...")
  ameco10 <- data.table::fread("data-raw/ameco/AMECO10.TXT.gz",
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
  ameco10 <- ameco10[COUNTRY%in%countries_considered]
  ameco10[, year:=unfactor(year)]

  # Sectoral balances from AMECO-------------------------------------------------
  # Foreign sector:
  ameco10_sect_balances <- data.table::fread("data-raw/ameco/AMECO10.TXT.gz",
                                             fill = TRUE, header = TRUE)

  ameco10_sect_balances <- ameco10_sect_balances[
    TITLE=="Balance on current transactions with the rest of the world (National accounts)" &
      UNIT=="(Percentage of gross domestic product at current prices)"
    ][
      !COUNTRY%in%aggregates_2be_eliminated
      ]

  ameco10_sect_balances_germany <-data.table::copy(ameco10_sect_balances)
  ameco10_sect_balances_germany <- ameco10_sect_balances_germany[
    COUNTRY %in% c("Germany", "West Germany")]

  ameco10_sect_balances_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]

  ameco10_sect_balances_germany <- data.table::melt(
    ameco10_sect_balances_germany, id.vars=c("COUNTRY"),
    variable.name="year",
    value.name = "sect_balance_foreign")

  ameco10_sect_balances_germany[, year:=as.double(as.character(year))]
  ameco10_sect_balances_germany[COUNTRY=="West Germany" & year>1990, sect_balance_foreign:=NA]
  ameco10_sect_balances_germany <- ameco10_sect_balances_germany[, COUNTRY:=countrycode::countrycode(
    COUNTRY, "country.name", "iso3c"
  )]
  ameco10_sect_balances_germany[, sect_balance_foreign:=mean(sect_balance_foreign, na.rm = T), year]
  ameco10_sect_balances_germany <- unique(ameco10_sect_balances_germany)

  ameco10_sect_balances[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
  ameco10_sect_balances <- ameco10_sect_balances[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
  )
  ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
  data.table::setnames(ameco10_sect_balances, old = "COUNTRY2", new = "COUNTRY")

  ameco10_sect_balances <- data.table::melt(ameco10_sect_balances,
                                            id.vars=c("COUNTRY"),
                                            variable.name="year",
                                            value.name = "sect_balance_foreign")

  ameco10_sect_balances <- rbind(ameco10_sect_balances, ameco10_sect_balances_germany)
  ameco10_sect_balances[, sect_balance_foreign:=as.double(sect_balance_foreign)]
  ameco10_sect_balances[, sect_balance_foreign:=sect_balance_foreign*(-1)]
  ameco10_sect_balances[, year:=as.integer(as.character(year))]
  ameco10_sect_balances <- ameco10_sect_balances[year<=last_year & year>=first_year]

  # Government sector:
  ameco16_sect_balances <- data.table::fread("data-raw/ameco/AMECO16.TXT.gz",
                                             fill = TRUE, header = TRUE)

  ameco16_sect_balances <- ameco16_sect_balances[
    TITLE=="Net lending (+) or net borrowing (-): general government :- ESA 2010" &
      UNIT=="(Percentage of GDP at current prices (excessive deficit procedure))"
    ][
      !COUNTRY%in%aggregates_2be_eliminated
      ]

  ameco16_sect_balances_germany <-data.table::copy(ameco16_sect_balances)
  ameco16_sect_balances_germany <- ameco16_sect_balances_germany[
    COUNTRY %in% c("Germany", "West Germany")]

  ameco16_sect_balances_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]

  ameco16_sect_balances_germany <- data.table::melt(
    ameco16_sect_balances_germany, id.vars=c("COUNTRY"),
    variable.name="year",
    value.name = "sect_balance_gvnt")

  ameco16_sect_balances_germany[, year:=as.double(as.character(year))]
  ameco16_sect_balances_germany[COUNTRY=="West Germany" & year>1990, sect_balance_gvnt:=NA]
  ameco16_sect_balances_germany <- ameco16_sect_balances_germany[, COUNTRY:=countrycode::countrycode(
    COUNTRY, "country.name", "iso3c"
  )]
  ameco16_sect_balances_germany[, sect_balance_gvnt:=mean(sect_balance_gvnt, na.rm = T), year]
  ameco16_sect_balances_germany <- unique(ameco16_sect_balances_germany)

  ameco16_sect_balances[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
  ameco16_sect_balances <- ameco16_sect_balances[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
  )
  ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
  data.table::setnames(ameco16_sect_balances, old = "COUNTRY2", new = "COUNTRY")

  ameco16_sect_balances <- data.table::melt(ameco16_sect_balances,
                                            id.vars=c("COUNTRY"),
                                            variable.name="year",
                                            value.name = "sect_balance_gvnt")

  ameco16_sect_balances <- rbind(ameco16_sect_balances, ameco16_sect_balances_germany)
  ameco16_sect_balances[, sect_balance_gvnt:=as.double(sect_balance_gvnt)]
  ameco16_sect_balances[, year:=as.double(as.character(year))]
  ameco16_sect_balances <- ameco16_sect_balances[year<=last_year & year>=first_year]

  # Private sector:
  # Step 1: get balance for corporations
  ameco14_sect_balances <- data.table::fread("data-raw/ameco/AMECO14.TXT.gz",
                                             fill = TRUE, header = TRUE)

  ameco14_sect_balances <- ameco14_sect_balances[
    TITLE=="Net lending (+) or net borrowing (-): corporations" & UNIT=="Mrd ECU/EUR"
    ][
      !COUNTRY%in%aggregates_2be_eliminated
      ]

  ameco14_sect_balances_germany <-data.table::copy(ameco14_sect_balances)
  ameco14_sect_balances_germany <- ameco14_sect_balances_germany[
    COUNTRY %in% c("Germany", "West Germany")]

  ameco14_sect_balances_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]

  ameco14_sect_balances_germany <- data.table::melt(
    ameco14_sect_balances_germany, id.vars=c("COUNTRY"),
    variable.name="year",
    value.name = "sect_balance_corp_abs")

  ameco14_sect_balances_germany[, year:=as.double(as.character(year))]
  ameco14_sect_balances_germany[COUNTRY=="West Germany" & year>1990, sect_balance_corp_abs:=NA]
  ameco14_sect_balances_germany <- ameco14_sect_balances_germany[, COUNTRY:=countrycode::countrycode(
    COUNTRY, "country.name", "iso3c"
  )]
  ameco14_sect_balances_germany[, sect_balance_corp_abs:=mean(sect_balance_corp_abs, na.rm = T), year]
  ameco14_sect_balances_germany <- unique(ameco14_sect_balances_germany)

  ameco14_sect_balances[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
  ameco14_sect_balances <- ameco14_sect_balances[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
  )
  ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
  data.table::setnames(ameco14_sect_balances, old = "COUNTRY2", new = "COUNTRY")

  ameco14_sect_balances <- data.table::melt(ameco14_sect_balances,
                                            id.vars=c("COUNTRY"),
                                            variable.name="year",
                                            value.name = "sect_balance_corp_abs")

  ameco14_sect_balances <- rbind(ameco14_sect_balances, ameco14_sect_balances_germany)
  ameco14_sect_balances[, sect_balance_corp_abs:=as.double(sect_balance_corp_abs)]
  ameco14_sect_balances[, year:=as.double(as.character(year))]
  ameco14_sect_balances <- ameco14_sect_balances[year<=last_year & year>=first_year]

  # Step 2: get balances for households
  ameco15_sect_balances <- data.table::fread("data-raw/ameco/AMECO15.TXT.gz",
                                             fill = TRUE, header = TRUE)

  ameco15_sect_balances <- ameco15_sect_balances[
    TITLE=="Net lending (+) or net borrowing (-): households and NPISH" & UNIT=="Mrd ECU/EUR"
    ][
      !COUNTRY%in%aggregates_2be_eliminated
      ]

  ameco15_sect_balances_germany <-data.table::copy(ameco15_sect_balances)
  ameco15_sect_balances_germany <- ameco15_sect_balances_germany[
    COUNTRY %in% c("Germany", "West Germany")]

  ameco15_sect_balances_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]

  ameco15_sect_balances_germany <- data.table::melt(
    ameco15_sect_balances_germany, id.vars=c("COUNTRY"),
    variable.name="year",
    value.name = "sect_balance_HH_abs")

  ameco15_sect_balances_germany[, year:=as.double(as.character(year))]
  ameco15_sect_balances_germany[COUNTRY=="West Germany" & year>1990, sect_balance_HH_abs:=NA]
  ameco15_sect_balances_germany <- ameco15_sect_balances_germany[, COUNTRY:=countrycode::countrycode(
    COUNTRY, "country.name", "iso3c"
  )]
  ameco15_sect_balances_germany[, sect_balance_HH_abs:=mean(sect_balance_HH_abs, na.rm = T), year]
  ameco15_sect_balances_germany <- unique(ameco15_sect_balances_germany)

  ameco15_sect_balances[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
  ameco15_sect_balances <- ameco15_sect_balances[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
  )
  ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
  data.table::setnames(ameco15_sect_balances, old = "COUNTRY2", new = "COUNTRY")

  ameco15_sect_balances <- data.table::melt(ameco15_sect_balances,
                                            id.vars=c("COUNTRY"),
                                            variable.name="year",
                                            value.name = "sect_balance_HH_abs")

  ameco15_sect_balances <- rbind(ameco15_sect_balances, ameco15_sect_balances_germany)
  ameco15_sect_balances[, sect_balance_HH_abs:=as.double(sect_balance_HH_abs)]
  ameco15_sect_balances[, year:=as.double(as.character(year))]
  ameco15_sect_balances <- ameco15_sect_balances[year<=last_year & year>=first_year]

  # Step 3: get GDP at current prices for normalization--------------------------
  ameco06_sect_balances <- data.table::fread("data-raw/ameco/AMECO6.TXT.gz",
                                             fill = TRUE, header = TRUE)

  ameco06_sect_balances <- ameco06_sect_balances[
    TITLE=="Gross domestic product at current prices" & UNIT=="Mrd ECU/EUR"
    ][
      !COUNTRY%in%aggregates_2be_eliminated
      ]


  ameco06_sect_balances_germany <-data.table::copy(ameco06_sect_balances)
  ameco06_sect_balances_germany <- ameco06_sect_balances_germany[
    COUNTRY %in% c("Germany", "West Germany")]

  ameco06_sect_balances_germany[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]

  ameco06_sect_balances_germany <- data.table::melt(
    ameco06_sect_balances_germany, id.vars=c("COUNTRY"),
    variable.name="year",
    value.name = "GDP_cp")

  ameco06_sect_balances_germany[, year:=as.double(as.character(year))]
  ameco06_sect_balances_germany[COUNTRY=="West Germany" & year>1990, GDP_cp:=NA]
  ameco06_sect_balances_germany <- ameco06_sect_balances_germany[, COUNTRY:=countrycode::countrycode(
    COUNTRY, "country.name", "iso3c"
  )]
  ameco06_sect_balances_germany[, GDP_cp:=mean(GDP_cp, na.rm = T), year]
  ameco06_sect_balances_germany <- unique(ameco06_sect_balances_germany)

  ameco06_sect_balances[, c("CODE", "SUB-CHAPTER", "TITLE", "UNIT",  "V67"):=NULL]
  ameco06_sect_balances <- ameco06_sect_balances[, COUNTRY2:=countrycode::countrycode(COUNTRY, "country.name", "iso3c"
  )
  ][!is.na(COUNTRY) & COUNTRY2 != "DEU"][, COUNTRY:=NULL]
  data.table::setnames(ameco06_sect_balances, old = "COUNTRY2", new = "COUNTRY")

  ameco06_sect_balances <- data.table::melt(ameco06_sect_balances,
                                            id.vars=c("COUNTRY"),
                                            variable.name="year",
                                            value.name = "GDP_cp")

  ameco06_sect_balances <- rbind(ameco06_sect_balances, ameco06_sect_balances_germany)
  ameco06_sect_balances[, GDP_cp:=as.double(GDP_cp)]
  ameco06_sect_balances[, year:=as.double(as.character(year))]
  ameco06_sect_balances <- ameco06_sect_balances[year<=last_year & year>=first_year]

  # Step 4: divide corporate and household value by GDP
  ameco_sect_balances <- Reduce(function(...) merge(..., all=TRUE,
                                                    by = c("COUNTRY", "year")),
                                list(ameco16_sect_balances,
                                     ameco10_sect_balances,
                                     ameco14_sect_balances,
                                     ameco15_sect_balances,
                                     ameco06_sect_balances)
  )
  ameco_sect_balances <- ameco_sect_balances[, .(COUNTRY, year,
                                                 sect_balance_gvnt,
                                                 sect_balance_foreign,
                                                 sect_balance_corp_abs,
                                                 sect_balance_HH_abs, GDP_cp)]
  ameco_sect_balances[, sect_balance_priv_corp:=(sect_balance_corp_abs/GDP_cp)*100]
  ameco_sect_balances[, sect_balance_priv_HH:=(sect_balance_HH_abs/GDP_cp)*100]
  ameco_sect_balances[, sect_balance_priv:=sect_balance_priv_corp+sect_balance_priv_HH]
  ameco_sect_balances[, balance_test:=sect_balance_priv+sect_balance_gvnt+sect_balance_foreign]
  ameco_sect_balances[, year:=as.integer(year)]
  ameco_sect_balances[, c("sect_balance_corp_abs", "balance_test",
                          "sect_balance_HH_abs", "GDP_cp"):=NULL]
  ameco_sect_balances <- ameco_sect_balances[COUNTRY%in%countries_considered]
  stopifnot(test_uniqueness(ameco_sect_balances, c("COUNTRY", "year")))




  # Merge all AMECO tables-------------------------------------------------------
  print("...merge all AMECO...")
  print("...test for duplicates in individual ameco parts...")
  lapply(list(ameco01_pop, ameco01_unemp, ameco02, ameco03,
              ameco07_wage_share, ameco07_rulc, ameco07_nulc,
              ameco10, ameco_sect_balances), test_uniqueness, c("COUNTRY", "year"))

  ameco_full <- Reduce(function(...) merge(..., all=TRUE,
                                           by = c("COUNTRY", "year")),
                       list(ameco01_pop, ameco01_unemp, ameco02, ameco03,
                            ameco07_wage_share, ameco07_rulc, ameco07_nulc,
                            ameco10, ameco_sect_balances)
  )

  ameco_full <- ameco_full[, .(year=as.double(as.character(year)),
                               iso3c=COUNTRY,
                               cap_form,
                               cpi_harm,
                               current_account_GDP_ameco,
                               nulc_eur,
                               nulc_lcu,
                               population_ameco,
                               rulc,
                               unemp_rate,
                               wage_share,
                               sect_balance_gvnt,
                               sect_balance_foreign,
                               sect_balance_priv_corp,
                               sect_balance_priv_HH,
                               sect_balance_priv)
                           ]
  print("...test for uniqueness of ameco_full...")
  stopifnot(test_uniqueness(ameco_full, c("iso3c", "year")))
  print("....finished.")
  return(ameco_full)
}

