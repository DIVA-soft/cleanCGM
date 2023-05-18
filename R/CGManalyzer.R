#' Full Pipeline to Read Data and Extract Variables
#'
#' @param datadir Character. Path to directory containing the CGM files (accepts txt and xlsx formats).
#' @param outputdir Character. Path to directory to save output (time series files).
#' @param timeCol Character. Name of the column with timestamp information (accepts numeric or character).
#' @param glucCol Character. Name of the column with the glucose information.
#' @param typeCol Character. Name of the column with the type of registry information (usually scan or measure).
#' @param verbose Logical. Whether to print progress messages in the console.
#'
#' @return Does not return any object, it saves time series files in the output folder.
#' @export
#'
CGManalyzer = function(datadir = NULL, outputdir = NULL,
                       timeCol = NULL, glucCol = NULL, typeCol = NULL,
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
  ids = c()
  for (i in 1:length(FILES)) {
    id_unlist = unlist(strsplit(basename(FILES[i]), split = "_"))
    id = id_unlist[2]
    if (suppressWarnings(is.na(as.numeric(id)))) id = id_unlist[1]
    ids[i] = id
  }
  ids = unique(ids)

  # back up of typeCol
  typeCol_bu = typeCol

  # loop through ids
  for (i in 1:length(ids)) {
    id = ids[i]
    # if (id == "282") browser()
    if (verbose) cat(id, " ")

    # files to read -----
    files2read = grep(id, FILES, value = TRUE)
    if (length(files2read) > 2) stop("Revise id = ", id)

    # 1 - read CGM file/s
    data = as.data.frame(readCGM(files = files2read))

    # 2 - format timestamp
    timestamp = formatTime(data, timeCol = timeCol)

    # 3 - format glucose
    glucose = formatGlucose(data, glucCol = glucCol)

    # 4 - mark scan and time series
    if (is.null(typeCol)) {
      typeCol = grep("type|tipo", colnames(data),
                     ignore.case = TRUE, value = TRUE)
    }
    # categories
    ts = which(data[, typeCol] == 0)
    scans = which(data[, typeCol] == 1)

    # reset typecol
    typeCol = typeCol_bu

    # 5 - cutoffs
    glucRanges = cut(glucose, breaks = c(0, 54, 70, 140, 180, 250, Inf), right = F)

    # 6 - merge data
    SCANS = data.frame(timestamp = timestamp[scans],
                       GLUC = glucose[scans],
                       classid = glucRanges[scans])
    TS = data.frame(timestamp = timestamp[ts],
                    GLUC = glucose[ts],
                    classid = glucRanges[ts])

    # 7 - save timeseries
    scansDir = file.path(outputdir, "scans")
    tsDir = file.path(outputdir, "time series")
    if (!dir.exists(scansDir)) dir.create(scansDir)
    if (!dir.exists(tsDir)) dir.create(tsDir)

    save(SCANS, file = file.path(scansDir, paste0(id, ".RData")))
    save(TS, file = file.path(tsDir, paste0(id, ".RData")))
  }
}
