#' Formats glucose in a consistent manner
#'
#' @param data data frame read by readCGM
#' @param glucCol Character. Name of the column with the glucose information
#'
#' @return Glucose formatted
#' @export
#' @importFrom data.table nafill
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
    # extract glucose in vector
    glucose = data[, glucCol]
    # check if decimal sep is other than default
    defaultSep = unlist(options("OutDec"))
    fileSep = gsub('[[:digit:]]+', '', glucose[1])
    if (nchar(fileSep) == 1) {
      if (fileSep != defaultSep) {
        glucose = gsub(fileSep, defaultSep, glucose)
      }
    }
    # remove header
    if (is.na(as.numeric(glucose[1])) &
        as.numeric(glucose[6]) == 250 &
        as.numeric(glucose[7]) == 70 &
        is.na(as.numeric(glucose[8]))) {
      glucose = glucose[-c(1:8)]
    }
    # impute missing values if needed
    toImpute = grep("Nivel bajo", glucose)
    glucose[toImpute] = NA
    glucose = as.numeric(glucose)
    glucose = data.table::nafill(glucose, "locf")
    # check units
    if (is.character(glucose)) glucose = gsub(",", ".", glucose, fixed = TRUE)
    glucose = as.numeric(glucose)
    if (grepl("mmol/l", glucCol, ignore.case = TRUE)) glucose = glucose*18.0182
  }
  return(glucose)
}
