#' Formats timestamp in a consistent manner
#'
#' @param data data frame read by readCGM
#' @param timeCol Character. Name of the column with the timestamp.
#' @param ts_indices Rows in data containing the actual time series.
#' @param scan_indices Rows in data containing scans information.
#'
#' @return timestamp formatted
#' @export
#' @importFrom lubridate force_tz
#'
formatTime = function(data, timeCol = NULL, ts_indices = NULL, scan_indices = NULL) {
  # initialize out objects
  duplicated_timestamps = no_sequential_timestamps = FALSE
  n_gaps_over_30min = 0
  ts_indices_to_remove = NULL
  if (is.null(timeCol)) {
    # identify time column
    timeCol = grep("time|tiempo|date|fecha|hora|temporal", colnames(data),
                   ignore.case = TRUE, value = TRUE)
    if (length(timeCol) > 1) {
      # keep the one formatted as POSIX
      timeCol_class = rep(NA, length(timeCol))
      for (timeCol_i in 1:length(timeCol)) {
        timeCol_class[timeCol_i] = class(data[, timeCol[timeCol_i]])[1]
      }
      if (any(timeCol_class == "POSIXct")) {
        timeCol = timeCol[which(timeCol_class == "POSIXct")]
      }
    }
    if (length(timeCol) > 1) {
      # keep the one with more distinct values
      timeCol_lengths = rep(NA, length(timeCol))
      for (timeCol_i in 1:length(timeCol)) {
        timeCol_lengths[timeCol_i] = length(table(data[, timeCol[timeCol_i]]))
      }
      timeCol = timeCol[which.max(timeCol_lengths)]
    }
  }
  if (is.numeric(data[, timeCol])) {
    # read as UTC as device does not detect DSTs (UTC does not include DSTs)
    ts_utc = as.POSIXct(data[, timeCol]*86400, origin = "1899-12-30", tz = "UTC")
    # calculate time increment along recordings to derive new timestamp
    time_increment = as.numeric(diff(ts_utc, units = "secs"))
    # new timestamp = t0 (in local timezone) + time increments
    t0 = lubridate::force_tz(ts_utc[1], tzone = Sys.timezone())
    timestamp = c(t0, t0 + cumsum(time_increment))
  } else if (is.character(data[, timeCol])) {
    ts_utc = strptime(data[, timeCol], format = "%d/%m/%Y %H:%M", tz = "UTC")
    if (any(is.na(ts_utc))) {
      ts_utc = strptime(data[, timeCol], format = "%Y/%m/%d %H:%M", tz = "UTC")
    }
    if (any(is.na(ts_utc))) {
      ts_utc = strptime(data[, timeCol], format = "%d-%m-%Y %H:%M", tz = "UTC")
    }
    # calculate time increment along recordings to derive new timestamp
    time_increment = as.numeric(diff(ts_utc, units = "secs"))
    # new timestamp = t0 (in local timezone) + time increments
    t0 = lubridate::force_tz(ts_utc[1], tzone = Sys.timezone())
    timestamp = c(t0, t0 + cumsum(time_increment))
  } else if ("POSIXct" %in% class(data[, timeCol])) {
    timestamp = data[, timeCol]
  }

  # round to minutes
  timestamp = round(as.POSIXct(timestamp), "mins")

  # quality checks (only in time series indices)
  time_increments = as.numeric(diff(timestamp[ts_indices], units = "mins"))
  if (any(time_increments > 30)) n_gaps_over_30min = sum(time_increments > 30)
  if (any(duplicated(timestamp[ts_indices]))) duplicated_timestamps = TRUE
  if (any(time_increments < 0)) no_sequential_timestamps = TRUE

  # return
  return(list(timestamp = timestamp,
              n_gaps_over_30min = n_gaps_over_30min,
              duplicated_timestamps = duplicated_timestamps,
              no_sequential_timestamps = no_sequential_timestamps,
              ts_indices = ts_indices))
}
