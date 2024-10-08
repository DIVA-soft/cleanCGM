#' Reads file with CGM data either in .txt or .xlsx format
#'
#' @param file Character. Path to file with CGM data (accepts txt and xlsx formats).
#'
#' @return Data frame with CGM data.
#' @export
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom data.table fread
readCGM = function(files) {

  # loop through files to load data -------
  datasets = list()
  length(datasets) = length(files)
  for (i in 1:length(files)) {

    # format of files
    format = unique(tools::file_ext(files))
    if (format == "txt") {
      datasets[[i]] = read.delim(files[i], sep = '\t', header = TRUE)
      NAs = which(is.na(datasets[[i]]$ID))
      if (length(NAs) > 0) datasets[[i]] = datasets[[i]][-NAs,]

      # reformat dataset?
      NAs = which(is.na(datasets[[i]]$Tipo.de.registro) & is.na(datasets[[i]]$Histórico.glucosa..mg.dL.) &
                    is.na(datasets[[i]]$Glucosa.leída..mg.dL.))
      if (length(NAs) > 0) {
        datasets[[i]]$Tipo.de.registro[NAs] = 0
        datasets[[i]]$Histórico.glucosa..mg.dL.[NAs] = datasets[[i]]$Hora[NAs]
        datasets[[i]]$Hora[NAs] = datasets[[i]]$ID[NAs]
        datasets[[i]]$ID[NAs] = 0
        # fix timestamp format in scan rows
        tsRevise = 1:(NAs[1] - 1)
        newTS = strptime(datasets[[i]]$Hora[tsRevise], format = "%Y/%m/%d %H:%M")
        newTS = format(newTS, format = "%d/%m/%Y %H:%M")
        datasets[[i]]$Hora[tsRevise] = newTS
        # remove extra rows if any
        extra_rows = which(datasets[[i]]$Hora == "" & datasets[[i]]$Histórico.glucosa..mg.dL. == "")
        if (length(extra_rows) > 0) datasets[[i]] = datasets[[i]][-extra_rows,]
      }


    } else if (format == "xlsx") {
      datasets[[i]] = openxlsx::read.xlsx(files[i], sheet = 1, startRow = 3)
      # colnames similar to .txt files
      colnames(datasets[[i]]) = gsub(".", " ", colnames(datasets[[i]]), fixed = TRUE)
    } else if (format == "csv") {
      datasets[[i]] = suppressWarnings(data.table::fread(files[i],
                                                         data.table = FALSE,
                                                         verbose = FALSE))
      # colnames similar to .txt files
      colnames(datasets[[i]]) = gsub(".", " ", colnames(datasets[[i]]), fixed = TRUE)
    } else {
      stop(paste0("Format ", format, " not supported at the moment"))
    }
  }
  # indices for rbinding
  inds = 1
  if (length(datasets) > 1) {
    t0 = c()
    for (i in 1:length(datasets)) {
      t0 = c(t0,as.POSIXct(formatTime(head(datasets[[i]], 1))$timestamp))
    }
    inds = order(t0)
    for (i in 1:length(datasets)) {
      datasets[[i]]$file_nr = inds[i]
      datasets[[i]]$filename = basename(files[i])
    }
  } else {
    datasets[[1]]$file_nr = 1
    datasets[[1]]$filename = basename(files[i])
  }
  # rbind datasets
  DAT = do.call("rbind", datasets[inds])

  return(DAT)
}
