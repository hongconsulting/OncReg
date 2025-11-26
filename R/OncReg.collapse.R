#' Collapse repeated measurements across IDs
#'
#' For each ID in a data frame, collapses all rows belonging to that ID into a
#' single row. Variables whose names match a specified pattern are summarized
#' according to a chosen `method`. The output data frame contains one row per
#' unique ID and one column per matched variable. Non-matching variables are
#' ignored.
#' @param data A data frame containing repeated measurements.
#' @param ID_varname Character string giving the name of the ID variable. Rows
#' with the same ID are treated as belonging to the same individual.
#' @param pattern String containing a regular expression used to select
#' variables. All variable names matching this pattern (via `grep()`) are
#' collapsed.
#' @param method Character string specifying how multiple values within each ID
#' should be collapsed. Options include:
#' \itemize{
#'   \item `"left"`: Returns the first non-missing value in order of appearance.
#'   \item `"mean"`: Returns the mean of non-missing values.
#'   \item `"max"`: Returns the maximum of non-missing values.
#'   \item `"min"`: Returns the minimum of non-missing values.
#'   \item `"single"` (default): Returns the mean of non-missing values and prints
#'   a message if more than one unique non-missing value is found.
#' }
#' @return A data frame with one row per unique ID and one column for each
#' variable matching `pattern`, summarized according to `method`.
#' @export
OR.collapse <- function(data, ID_varname, pattern, method = "single") {
  IDs  <- unique(data[[ID_varname]])
  varnames <- grep(pattern, names(data), value = TRUE)
  output <- data.frame("ID" = IDs)
  for (i in 1:length(varnames)) {
    varname <- varnames[i]
    output[[varname]] <- NA
    for (j in 1:length(IDs)) {
      ID <- IDs[j]
      ID_mask <- data[[ID_varname]] == ID
      subdata <- data[[varname]][ID_mask]
      subdata_na_omit <- OR.NA.rm(subdata)
      if (method != "single" & length(subdata_na_omit) == 0) {
        message("[OR.collapse] ", varname, " for ID ", ID, " is empty")
        next
      }
      subdata_unique <- unique(subdata_na_omit)
      if (method == "left") {
        output[[varname]][j] <- subdata_na_omit[1]
      } else if (method == "max") {
        output[[varname]][j] <- max(subdata_unique)
      } else if (method == "mean" | method == "single") {
        if (method == "single" & length(subdata_unique) > 1) {
          message("[OR.collapse] ", varname, " for ID ", ID, " has more than 1 unique value\n")
        }
        output[[varname]][j] <- mean(subdata_na_omit)
      } else if (method == "min") {
        output[[varname]][j] <- min(subdata_unique)
      } else {
        stop(paste0("[OR.collapse] method ", method, " not implemented"))
      }
    }
  }
  return(output)
}
