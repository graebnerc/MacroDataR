get_oecd_data <- function(download_data, countries_considered,
                          first_year, last_year){
  oecd_full_data <- merge_oecd_data(
    list(get_oecd_debt_data(),
         get_oecd_pub_debt_data(),
         get_oecd_finance_data(),
         get_oecd_av_wages()
         )
    )
  return(oecd_full_data)

}
#' Merges OECD data
merge_oecd_data <- function(oecd_data_list){
  oecd_data <- Reduce(function(...) merge(..., all=TRUE,
                                          by = c("iso3c", "year")),
                      oecd_data_list
  )
  stopifnot(test_uniqueness(oecd_data, c("iso3c", "year")))
  return(oecd_data)
}

get_oecd_debt_data <- function(download_data, countries_considered,
                               first_year, last_year){
  oecd_debt_file_name <- "data-raw/oecd_debt_data.csv"
  oecd_debt_vars <- c("DBTS1GDP", "DBTS11GDP", "DBTS12GDP",
                      "DBTS13GDP", "DBTS14_S15GDI")

  if (download_data | !file.exists((paste0(oecd_debt_file_name, ".gz")))){
    if (!download_data){
      warning("File for OECD debt data does not exist. Download from www...")
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
    test_uniqueness(oecd_debt_data_raw, c("LOCATION", "INDICATOR", "obsTime"))
    data.table::fwrite(oecd_debt_data_raw, oecd_debt_file_name)
    R.utils::gzip(paste0(oecd_debt_file_name),
                  destname=paste0(oecd_debt_file_name, ".gz"),
                  overwrite = TRUE)
    oecd_debt_data_raw <- data.table::as.data.table(oecd_debt_data_raw)
  } else {
    oecd_debt_data_raw <- data.table::fread(paste0(oecd_debt_file_name, ".gz"))
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

}

get_oecd_pub_debt_data <- function(download_data, countries_considered,
                                   first_year, last_year){
  oecd_pub_debt_file_name <- "data-raw/oecd_pub_debt_data.csv"

  if (download_data | !file.exists(paste0(oecd_pub_debt_file_name, ".gz"))){
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
    test_uniqueness(oecd_pub_debt_data, c("LOCATION", "obsTime"))
    data.table::fwrite(oecd_pub_debt_data, oecd_pub_debt_file_name)
    R.utils::gzip(paste0(oecd_pub_debt_file_name),
                  destname=paste0(oecd_pub_debt_file_name, ".gz"),
                  overwrite = TRUE)
  } else {
    oecd_pub_debt_data <- data.table::fread(
      paste0(oecd_pub_debt_file_name, ".gz")
    )
  }

  old_names <- c("LOCATION", "obsTime", "obsValue")
  new_names <- c("iso3c", "year", "debt_gen_gvt_gross")
  data.table::setnames(oecd_pub_debt_data, old = old_names, new = new_names)
  oecd_pub_debt_data[,
                     (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
                     .SDcols = setdiff(new_names, "iso3c")
                     ]

}

get_oecd_finance_data <- function(download_data, countries_considered,
                                  first_year, last_year){
  oecd_finance_file_name <- "data-raw/oecd_finance_data.csv"

  if (download_data | !file.exists(paste0(oecd_finance_file_name, ".gz"))){
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
    test_uniqueness(oecd_finance_data, c("LOCATION", "obsTime"))

    data.table::fwrite(oecd_finance_data,
                       oecd_finance_file_name)
    R.utils::gzip(paste0(oecd_finance_file_name),
                  destname=paste0(oecd_finance_file_name, ".gz"),
                  overwrite = TRUE)
  } else {
    oecd_finance_data <- data.table::fread(paste0(oecd_finance_file_name, ".gz"))
  }

  old_names <- c("LOCATION", "obsTime", "obsValue")
  new_names <- c("iso3c", "year",
                 "interest_long_term")
  data.table::setnames(oecd_finance_data, old = old_names, new = new_names)
  oecd_finance_data[,
                    (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
                    .SDcols = setdiff(new_names, "iso3c")
                    ]
}

get_oecd_av_wages <- function(download_data, countries_considered,
                              first_year, last_year){
  oecd_wage_data_raw_file <- "data-raw/oecd_wage_data.csv"

  if (download_data | !file.exists(paste0(oecd_wage_data_raw_file, ".gz"))){
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
    test_uniqueness(oecd_wage_data, c("COUNTRY", "obsTime"))

    data.table::fwrite(oecd_wage_data,
                       oecd_wage_data_raw_file)
    R.utils::gzip(paste0(oecd_wage_data_raw_file),
                  destname=paste0(oecd_wage_data_raw_file, ".gz"),
                  overwrite = TRUE)
  } else {
    oecd_wage_data <- data.table::fread(paste0(oecd_wage_data_raw_file, ".gz"))
  }

  old_names <- c("COUNTRY", "obsTime", "obsValue")
  new_names <- c("iso3c", "year",
                 "average_wages")
  data.table::setnames(oecd_wage_data, old = old_names, new = new_names)
  oecd_wage_data[,
                 (setdiff(new_names, "iso3c")):= lapply(.SD, as.double),
                 .SDcols = setdiff(new_names, "iso3c")
                 ]

}



