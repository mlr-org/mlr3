#' @field col_hashes (named `character`)\cr
#' Hash (unique identifier) for all columns except the `primary_key`: A `character` vector, named by the columns that each element refers to.\cr
#' Columns of different [`Task`]s or [`DataBackend`]s that have agreeing `col_hashes` always represent the same data, given that the same `row`s are selected.
#' The reverse is not necessarily true: There can be columns with the same content that have different `col_hashes`.
