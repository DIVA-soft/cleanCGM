#' Formats timestamp in a consistent manner
#'
#' @param data data frame read by readCGM
#' @param timeCol Character. Name of the column with the timestamp.
#'
#' @return timestamp formatted
#' @export
#'
formatTime = function(data, timeCol = NULL) {
  if (is.null(timeCol)) {
    timeCol = grep("time|tiempo|datae|fecha|hora", colnames(data),
                   ignore.case = TRUE, value = TRUE)
    if (length(timeCol) > 1) {
      timeCol_lengths = rep(NA, length(timeCol))
      for (timeCol_i in 1:length(timeCol)) {
        timeCol_lengths[timeCol_i] = length(table(data[, timeCol[timeCol_i]]))
      }
      timeCol = timeCol[which.max(timeCol_lengths)]
    }
  }
  if (is.numeric(data[, timeCol])) {
    timestamp = as.POSIXct(data[, timeCol]*86400,
                                        origin = "1899-12-30",tz = "GMT")
  } else if (is.character(data[, timeCol])) {
    timestamp = strptime(data[, timeCol],
                                      format = "%d/%m/%Y %H:%M", tz = "GMT")
    if (all(is.na(timestamp))) {
      timestamp = strptime(data[, timeCol],
                           format = "%Y/%m/%d %H:%M", tz = "GMT")
    }
  }
  # check and correct timestamp
  if (any(is.na(timestamp))) {
    timestamp = strptime(data[, timeCol],
                         format = "%Y/%m/%d %H:%M", tz = "GMT")
  }

  if (any(is.na(timestamp))) {
    timestamp = data[, timeCol]
  }

  # round to minutes and convert to character
  timestamp = as.character(round(as.POSIXct(timestamp), "mins"))

  # check missing hour:min
  addHour = which(nchar(timestamp) == 10)
  if (length(addHour) > 0) {
    timestamp[addHour] = paste(timestamp[addHour], "00:00:00")
  }

  return(timestamp)
}
