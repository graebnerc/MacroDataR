#' Get data from LMF
#'
#' Assembles relevant indicators from LMF database
#'
#' Assembles relevant indicators from the Lane-Milesi-Ferreti database. Because
#'  data is published only in Excel, the dataset as such must be downloaded
#'  manually and placed in the data-raw folder. It then can be processed using
#'  this function. This is done to avoid any functions that rely on JAVA, since
#'  this often becomes problematic in practice.
#' @param download_data If TRUE, data will be downloaded from the internet
#' @param countries_considered Vector of iso3c codes of the countries for
#'  which data should be assembled.
#' @param first_year First year for which data is to be collected (numeric).
#' @param last_year Last year for which data is to be collected (numeric).
#' @family download_helpers
get_lmf <- function(download_data, countries_considered,
                    first_year, last_year){
  print("LMF data...")
  warning("LMF data is not updated automatically. Currently using 1970-2011 data (May 2019).")

  lmf_file <- "data-raw/lmf.csv.gz"

  if (!file.exists(lmf_file)){
    warning("LMF data not available in data-raw. This data must be downloaded
            manually from http://www.philiplane.org/EWN.html")
  } else {
    lmf_cols <- c(
      "Year", "Country Name",
      "Portfolio equity assets (stock)",
      "Portfolio equity liabilities (stock)",
      "Portfolio debt assets", "Portfolio debt liabilities",
      "FDI assets (stock)",  "FDI assets (other)",
      "FDI liabilities (stock)", "FDI liabilities (other)",
      "Debt assets (stock)", "Debt liabilities (stock)",
      "NFA", "NFA/GDP"
    )
    lmf_new_names <- c(
      "year", "iso3c",
      "lmf_port_eq_asst_stock",
      "lmf_port_eq_liab_stock",
      "lmf_port_dbt_asst", "lmf_port_dbt_liab",
      "lmf_fdi_asst_stock", "lmf_fdi_asst_other",
      "lmf_fdi_liab_stock", "lmf_fdi_liab_other",
      "lmf_debt_asst_stock", "lmf_debt_liab_stock",
      "lmf_nfa", "lmf_nfa_gdp"
    )
    lmf_raw <- data.table::fread(lmf_file, na.strings = "")
    lmf_raw <- dplyr::select(lmf_raw, dplyr::one_of(lmf_cols))
    lmf <- data.table::as.data.table(lmf_raw)
    data.table::setnames(lmf, old = lmf_cols, new = lmf_new_names)
    lmf[!iso3c%in%c("Central African Rep.", "Euro Area", "Kosovo"),
        iso3c:=countrycode::countrycode(iso3c, "country.name", "iso3c")]
    lmf <- lmf[iso3c %in% countries_considered]
    lmf[, lmf_nfa_gdp:=gsub(pattern = "%",
                            replacement = "",
                            x = lmf_nfa_gdp)]

    #' Transforms cols with characters into doubles explicitly
    #'
    #' Conducts the following cleaning procedures:
    #'  1. NA values remain NA.
    #'  2. If the string contains the substring 'NA', set to NA
    #'  3. Remove points from strings
    #'  4. Convert to double
    clear_lmf_chars <- Vectorize(
      function(x_old){
        if (is.na(x_old) | (grepl("NA", x_old))){
          x_new <- NA
          return(x_new)
        }
        x_sub <- gsub(pattern = ",",
                      replacement = ".",
                      x = gsub(
                        pattern = "\\.",
                        replacement = "",
                        x = x_old)
                      )
        x_new <- as.double(x_sub)
        return(x_sub)
      },
      SIMPLIFY = T, USE.NAMES = F
    )

    lmf[,
        (setdiff(lmf_new_names, "iso3c")):= lapply(.SD, clear_lmf_chars),
        .SDcols = setdiff(lmf_new_names, "iso3c")
        ]
  }
  stopifnot(test_uniqueness(lmf, c("iso3c", "year")))
  print("finished.")
  return(lmf)
}
