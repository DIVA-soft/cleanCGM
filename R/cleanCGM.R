#' Full Pipeline to Read Data and Extract Variables
#'
#' @param datadir Character. Path to directory containing the CGM files (accepts txt and xlsx formats).
#' @param outputdir Character. Path to directory to save output (time series files).
#' @param timeCol Character. Name of the column with timestamp information (accepts numeric or character).
#' @param glucCol Character. Name of the column with the glucose information.
#' @param typeCol Character. Name of the column with the type of registry information (usually scan or measure).
#' @param verbose Logical. Whether to print progress messages in the console.
#' @param suffix Character. Suffix to be added to the filename of the output file to be saved.
#'
#' @return Does not return any object, it saves time series files in the output folder.
#' @export
#'
cleanCGM = function(datadir = NULL, outputdir = NULL,
                    timeCol = NULL, glucCol = NULL, typeCol = NULL,
                    suffix = NULL,
                    verbose = TRUE) {

  # rm(list = ls())
  # datadir = "D:/EXTREME/CGM_PRE_EXTREME/"
  # outputdir = "D:/EXTREME/output/CGM/1-baseline/"
  # timeCol = NULL
  # glucCol = NULL
  # typeCol = NULL
  # verbose = TRUE

  if (is.null(datadir) | !dir.exists(outputdir)) stop("Please, specify the datadir.")
  if (is.null(outputdir) | !dir.exists(outputdir)) stop("Please, specify the outputdir.")

  # print in console...
  if (verbose) {
    cat('\n')
    cat(paste0(rep('_', options()$width), collapse = ''))
    cat("Processing data...\n")
    cat("ID: ")
  }

  # list of files to be read
  FILES = dir(datadir, full.names = TRUE)

  # extract IDs from filenames
  IDs = data.frame(filename = FILES, id = NA)
  for (i in 1:length(FILES)) {
    id_unlist = unlist(strsplit(basename(FILES[i]), split = "_"))
    id = id_unlist[2]
    if (suppressWarnings(is.na(as.numeric(id))) &
        !grepl("ASP|MG|NF|NVS", id)) id = id_unlist[1]
    IDs[i, 2] = id
  }
  ids = unique(IDs$id)

  # back up of typeCol
  typeCol_bu = typeCol

  # loop through ids
  for (i in 1:length(ids)) {
    id = ids[i]
    if (verbose) cat(id, " ")
    # files to read -----
    files2read = IDs$filename[which(IDs$id == id)]
    # 1 - read CGM file/s
    data = as.data.frame(readCGM(files = files2read))
    if (nrow(data) < 10) {
      cat("skipped because insufficient data collected")
      next
    }

    # 2 - format timestamp
    timestamp = formatTime(data, timeCol = timeCol)

    # 3 - format glucose
    glucose = formatGlucose(data, glucCol = glucCol)
    # adjustment for PREFIT UP data (first rows )
    if (length(glucose) > length(timestamp)) {
      diff = length(glucose) - length(timestamp)
      glucose = glucose[(diff + 1):length(glucose)]
    }

    # 4 - mark scan and time series
    if (is.null(typeCol)) {
      typeCol = grep("type|tipo", colnames(data),
                     ignore.case = TRUE, value = TRUE)
      if (grepl("evento", typeCol)) {
        typeCol = typeCol[-grep("evento", typeCol)]
      }
    }
    # categories
    if (length(typeCol) > 0) {
      ts = which(data[, typeCol] == 0)
      scans = which(data[, typeCol] == 1)

      # reset typecol
      typeCol = typeCol_bu
    } else {
      scans = c()
      ts = 1:length(glucose)
    }

    # 5 - merge data
    SCANS = data.frame(timestamp = timestamp[scans],
                       GLUC = glucose[scans])
    TS = data.frame(timestamp = timestamp[ts],
                    GLUC = glucose[ts])

    # 7 - save timeseries
    scansDir = file.path(outputdir, "scans")
    tsDir = file.path(outputdir, "time series")
    if (!dir.exists(scansDir)) dir.create(scansDir)
    if (!dir.exists(tsDir)) dir.create(tsDir)
    if (!is.null(suffix)) id = paste0(id, suffix)
    save(SCANS, file = file.path(scansDir, paste0(id, ".RData")))
    save(TS, file = file.path(tsDir, paste0(id, ".RData")))
  }
}
