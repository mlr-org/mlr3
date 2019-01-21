Mlr3Object = R6Class("Mlr3Object",
  public = list(
    initialize = function(id, hash = NA_character_) {
      private$.id = assert_id(id)
      private$.hash = assert_string(hash, na.ok = TRUE)
    }
  ),

  active = list(
    id = function(rhs) {
      if (missing(rhs))
        return(private$.id)
      private$.hash = NA_character_
      private$.id = assert_id(rhs)
    },

    hash = function() {
      if (is.na(private$.hash))
        private$.hash = private$.calculate_hash()
      private$.hash
    }
  ),

  private = list(
    .id = NULL,
    .hash = NULL
  )
)
