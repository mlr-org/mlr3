
#' * `$overwrite()` overwrite the data in the [DataBackend] with data provided as [data.table()].
#'   The row ids must be provided as column in `data` (with column name matching the primary key name of the [DataBackend]).
#' * `$replace_columns()` replaces columns in the [DataBackend] with columns provided in a [data.table()].
#'   The row ids must be provided as column in `data` (with column name matching the primary key name of the [DataBackend]).

    overwrite = function(data) {
      task_overwrite(self, data)
    },

    replace_columns = function(data) {
      task_replace_columns(self, data)
    },

