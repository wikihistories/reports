split_batches <- function(variable, batch_size) {
  batches <- split(variable, ceiling(seq_along(variable)/batch_size)) %>%
    map(\(x) paste0(x, collapse = "|"))
  batches
}
