
#' @export
longestCommonPrefix <- function(x) {
  if (length(x) == 0) return(NA_character_)
  x <- sort(x)
  n <- min(nchar(x))
  charsFirst <- strsplit(first(x), "")[[1]][1:n]
  charsLast <- strsplit(last(x), "")[[1]][1:n]
  fristNonMatch <- which(charsFirst != charsLast)[1]
  if (is.na(fristNonMatch)) {
    return(paste0(charsFirst, collapse=""))
  } else if (fristNonMatch == 1) {
    return("")
  } else {
    return(paste0(charsFirst[1:(fristNonMatch-1)], collapse=""))
  }
}


#' @export
longestCommonSuffix <- function(x) {
  if (length(x) == 0) return(character(0))
  stringi::stri_reverse(x) |> longestCommonPrefix() |> stringi::stri_reverse()
}


#' @export
uniqueMiddle <- function(x) {
  if (length(x) == 0) return(character(0))
  prefix <- longestCommonPrefix(x)
  suffix <- longestCommonSuffix(x)
  return(gsub(paste0("^", prefix, "|", suffix, "$"), "", x))
}

