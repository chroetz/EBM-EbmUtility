#' @export
removeFileNameEnding <- function(x) {
  return(gsub("\\.[^.]*$", "", x))
}
