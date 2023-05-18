#' Formats glucose in a consistent manner
#'
#' @param data data frame read by readCGM
#' @param glucCol Character. Name of the column with the glucose information
#'
#' @return Glucose formatted
#' @export
#'
formatGlucose = function(data, glucCol = NULL) {
  if (is.null(glucCol)) {
    glucCol = grep("glucos", colnames(data),
                   ignore.case = TRUE, value = TRUE)
    if (length(glucCol) > 1) {
      glucCol_lengths = rep(NA, length(glucCol))
      for (glucCol_i in 1:length(glucCol)) {
        glucCol_lengths[glucCol_i] = length(table(data[, glucCol[glucCol_i]]))
      }
      glucCol = glucCol[which.max(glucCol_lengths)]
    }

    # check units
    glucose = data[, glucCol]
    if (is.character(glucose)) glucose = gsub(",", ".", glucose, fixed = TRUE)
    glucose = as.numeric(glucose)
    if (grepl("mmol/l", glucCol, ignore.case = TRUE)) glucose = glucose*18.0182
  }
  return(glucose)
}
