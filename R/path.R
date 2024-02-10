#' @export
makeDirsIfNecessary <- function(path) {
  dirPath <- path |> dirname()
  if (!dir.exists(dirPath)) {
    cat("Create directory", dirPath, "for file", fileName, ".\n")
    dir.create(dirPath, recursive=TRUE)
  }
  return(invisible())
}
