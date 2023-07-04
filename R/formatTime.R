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
    timeCol = grep("time|tiempo|date|fecha|hora", colnames(data),
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

  # check if any of the dates is a DST (daylight saving time)
  from = as.Date(substr(timestamp[1], 1, 10))
  to_index = which(!is.na(timestamp))[length(which(!is.na(timestamp)))]
  to = as.Date(substr(timestamp[to_index], 1, 10)) + 1
  dates = seq.Date(from, to, by = "day")
  offset = c()
  for (di in 1:length(dates)) {
    date = dates[di]
    dayStart = paste(date, "00:00:00")
    dayEnd = paste(date + 1, "00:00:00")
    daylength_hours = as.numeric(difftime(dayEnd, dayStart, units = "hours"))
    offset[di] = 24 - daylength_hours
  }
  names(offset) = dates
  if (any(offset != 0)) there_is_DST = TRUE else there_is_DST = FALSE

  # if there is a DST, then revise timestamp
  if (there_is_DST) {
    offset_secs = offset[which(offset != 0)] * 60 * 60
    # Revise timestamps from 2am in dst days...
    from = paste(names(offset[which(offset != 0)]), "01:59:59")
    from = as.numeric(as.POSIXct(from, tz = ""))
    timenum = as.numeric(strptime(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Madrid"))
    dst2revise = which(is.na(timenum) | timenum > from)
    if (length(dst2revise) > 0) {
      # summer or winter DST
      if (offset_secs == 3600) {
        # assume device does not account for dst, then 03 should be 02 and might be duplicates
        timestamp = paste0(timestamp, "+0100")
        timestamp[dst2revise] = gsub("0100", "0200", timestamp[dst2revise])
        timestamp = gsub(" ", "T", timestamp, fixed = TRUE)
        # new timestamp
        plus1h = grep("0200", timestamp)
        for (hi in 0:23) {
          prefix = ifelse(nchar(hi) == 1, "T0", "T")
          pattern = paste0(prefix, hi, ":")
          rephi = hi + 1
          if (rephi > 23) rephi = 0
          prefix = ifelse(nchar(rephi) == 1, "T0", "T")
          replacement = paste0(prefix, rephi, ":")
          exclude_next_loop = grep(pattern, timestamp[plus1h])
          timestamp[plus1h] = gsub(pattern, replacement, timestamp[plus1h])
          plus1h = plus1h[-exclude_next_loop]
        }
      } else if (offset_secs == -3600) {
        # assume device does not account for dst, then 03 should be 02 and might be duplicates
        to = dst2revise[grep(" 03:| 04:| 05:| 06:| 07:| 08:| 09:| 10:", timestamp[dst2revise])[1]]
        if (length(to) == 0) to = length(timestamp)
        timestamp = paste0(timestamp, "+0100")
        timestamp[1:(to - 1)] = gsub("0100", "0200", timestamp[1:(to - 1)])
        timestamp = gsub(" ", "T", timestamp, fixed = TRUE)
        # now substract 1 hour
        minus1h = grep("0100", timestamp)
        for (hi in 0:23) {
          prefix = ifelse(nchar(hi) == 1, "T0", "T")
          pattern = paste0(prefix, hi, ":")
          rephi = hi - 1
          if (rephi < 0) rephi = 23
          prefix = ifelse(nchar(rephi) == 1, "T0", "T")
          replacement = paste0(prefix, rephi, ":")
          exclude_next_loop = grep(pattern, timestamp[minus1h])
          timestamp[minus1h] = gsub(pattern, replacement, timestamp[minus1h])
          minus1h = minus1h[-exclude_next_loop]
        }
      }
    }
  }
  # check is iso8601
  all_iso = c()
  for (ti in 1:length(timestamp)) all_iso[ti] = GGIR::is.ISO8601(timestamp[ti])

  # turn to iso8601 format
  if (all(all_iso) == FALSE) {
    timestamp = GGIR::POSIXtime2iso8601(x = as.POSIXct(timestamp, tz = "Europe/Madrid"),
                                        tz = "Europe/Madrid")
  }

  return(timestamp)
}
