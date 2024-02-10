#' @export
setupBatches <- function(vec, nBatches) {
  n <- length(vec)
  batchSizeSmall <- floor(n / nBatches)
  batchSizeLarge <- batchSizeSmall + 1
  nBatchesLarge <- n - batchSizeSmall * nBatches
  nBatchesSmall <- nBatches - nBatchesLarge
  batches <- split(
    vec,
    c(rep(seq_len(nBatchesLarge), each=batchSizeLarge),
      rep(nBatchesLarge+seq_len(nBatchesSmall), each=batchSizeSmall)))
  return(batches)
}


#' @export
splitAndGetOneBatch <- function(name, values, nBatches, batchIndex) {
  if (!hasValue(nBatches) || nBatches[[1]] == 1) {
    cat("All", name, "values are one batch of length", length(values), ".\n")
    return(values)
  }
  cat("Split", name, "into", nBatches, "batches.\n")
  batches <- setupBatches(values, nBatches)
  if (batchIndex > length(batches)) {
    cat("Batch", batchIndex, "is out of range. Nothing to do.\n")
    return(invisible())
  }
  batch <- batches[[batchIndex]]
  if (length(batch) == 0) {
    cat("Batch", batchIndex, "is empty. Nothing to do.\n")
    return(invisible())
  }
  cat("Process batch", batchIndex, "with", length(batch), name, "values.\n")
  return(batch)
}
