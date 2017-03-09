library(RCurl)

curlProgress <- function(down, up, pcur, width) {
  total <- as.numeric(down[1]) # Total size as passed from curlPerform
  cur <- as.numeric(down[2])   # Current size as passed from curlPerform
  x <- cur/total
  px <- round(100 * x)
  if(!is.nan(x) && px!=pcur){
    x <- round(width * x)
    sc <- rev(which(total> c(1024^0, 1024^1, 1024^2, 1024^3)))[1] - 1
    lb <- c('B', 'KB', 'MB', 'GB')[sc+1]
    cat(paste(c(
      "\r  |", rep.int(".", x), rep.int(" ", width - x),
      sprintf("| %g%s of %g%s %3d%%", round(cur/1024^sc, 2), lb, round(total/1024^sc, 2), lb, px)),
      collapse = ""))
    flush.console()
    return(px)
  }
  return(pcur)
}

curlDownload <- function(url, fname, verbose=FALSE) {
  f <- CFILE(fname, mode="wb")
  width <- getOption("width") - 25
  pcur <- 0
  result <- curlPerform(url=url, writedata=f@ref, noprogress=FALSE,
                        progressfunction=function(down,up) pcur<<-curlProgress(down, up, pcur, width),
                        followlocation=TRUE)
  close(f)
  if (verbose) cat('\n   Download ', names(result), ' (Result: ', result, ')\n', sep="")
  invisible(result)
}

curlLoad <- function(url, envir=parent.frame(), verbose=FALSE) {
  tmpFile <- tempfile()
  curlDownload(url, tmpFile)
  result <- load(tmpFile, envir, verbose)
  file.remove(tmpFile)
  invisible(result)
}

getURLHeaders <- function(url) {
  h <- basicHeaderGatherer()
  result <- curlPerform(url=url, customrequest='HEAD',
                        nobody=1L, headerfunction=h$update, followlocation=T)
  h$value()
}

# Dropbox specific
curlCachedLoad <- function(url, envir=parent.frame(), verbose=FALSE, folder="cache") {
  headers <- getURLHeaders(url)
  etag <- headers["etag"]
  filename <- sub("\"", "", sub("filename=\"", "", unlist(strsplit(headers["content-disposition"], "; "))[2]))
  filecache <- file.path(folder, paste0(filename, "-", etag))
  if (!file.exists(filecache)) {
    dir.create(folder, showWarnings=FALSE, recursive=TRUE)
    suppressWarnings(file.remove(list.files(folder, pattern=filename, full.names=TRUE)))
    curlDownload(url, filecache)
  }
  load(filecache, envir, verbose)
}
