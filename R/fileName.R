#' @export
removeFileNameEnding <- function(x) {
  return(gsub("\\.[^.]*$", "", x))
}

#' @export
getFileNameEnding <- function(x) {
  return(str_extract(x, "(?<=\\.)[^.]*$"))
}
