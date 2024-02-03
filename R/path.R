#' @export
makeDirsIfNecessary <- function(path) {
  dirPath <- path |> dirname()
  if (!dir.exists(dirPath)) {
    dir.create(dirPath, recursive=TRUE)
  }
}
