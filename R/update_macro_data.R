#' Updata data
#'
#' \code{update_data} function updates the data.
#'
#'  \code{update_data} function updates the data set. To function properly it
#'  requires access to the internet. The data can also be updated selectively.
#'
#' @param updata_full Should all data be downloaded anew (if true), or should
#'     previously downloaded data be used (if false)?
#' @param vars Specifies the variables to be updated as strings. Default is all.
#' @return Returns the updated data set.
#' TODO: Maybe only information?
#' @export
update_macro_data <- function(update_full=TRUE, vars="all") {
  data_download <- update_full
  print(paste0("Downloading data: ", data_download))
  source("R/macro_data.R")
  print("...complete!")
}
