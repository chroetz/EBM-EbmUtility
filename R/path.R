#' @export
makeDirsIfNecessary <- function(path) {
  dirPath <- path |> dirname()
  fileName <- path |> basename()
  if (!dir.exists(dirPath)) {
    cat("Create directory", dirPath, "for file", fileName, ".\n")
    dir.create(dirPath, recursive=TRUE)
  }
  return(file.path(normalizePath(dirPath), fileName))
}
