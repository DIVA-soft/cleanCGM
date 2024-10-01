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
      glucCol = glucCol[which(glucCol_lengths > 2)]
    }
    # extract glucose in vector
    if (length(glucCol) == 1) {
      glucose = data[, glucCol]
    } else {
      tmp = apply(data[, glucCol], 2, is.na)
      if (any(rowSums(tmp) == 0)) {
        glucCol = glucCol[which.max(glucCol_lengths)]
        glucose = data[, glucCol]
      } else {
        colIndex = apply(tmp, 1, function(x) ifelse(any(x == F),
                                                    which(x == F), 0))
        glucose = rep(NA, nrow(data))
        for (ii in 1:length(glucCol)) {
          glucose[which(colIndex == ii)] = data[which(colIndex == ii), glucCol[ii]]
        }
        # keep only one glucCol for future calculations
        glucCol = glucCol[which.max(glucCol_lengths)]
      }
    }
    # check if decimal sep is other than default
    defaultSep = unlist(options("OutDec"))
    fileSep = gsub('[[:digit:]]+', '', glucose[which(!is.na(glucose))])
    fileSep = unique(fileSep[fileSep != ""])
    if (length(fileSep) > 0) {
      if (nchar(fileSep) > 0) {
        if (fileSep != defaultSep) {
          glucose = gsub(fileSep, defaultSep, glucose)
        }
      }
    }
    # remove header
    if (all(is.na(as.numeric(glucose[c(1, 8)]))) &
        all(!is.na(as.numeric(glucose[c(6, 7)])))) {
      if (as.numeric(glucose[6]) == 250 & as.numeric(glucose[7]) == 70) {
        glucose = glucose[-c(1:8)]
      }
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
