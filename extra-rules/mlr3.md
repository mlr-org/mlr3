### Architecture

This package uses R6 classes organized around a dictionary registry pattern.

#### Class hierarchy

- `Learner` > `LearnerClassif` / `LearnerRegr` > concrete (e.g., `LearnerClassifRpart`)
- `Task` > `TaskSupervised` > `TaskClassif` / `TaskRegr`
- `Measure` > `MeasureClassif` / `MeasureRegr` / `MeasureSimilarity`
- `Resampling` > `ResamplingCV`, `ResamplingHoldout`, etc.
- `DataBackend` > `DataBackendDataTable`, `DataBackendCbind`, etc.
- `Prediction` > `PredictionClassif` / `PredictionRegr`

#### File naming

- One R6 class per file, named exactly as the class: `LearnerClassifRpart.R` contains `LearnerClassifRpart`.
- Named dataset tasks use an underscore: `TaskClassif_iris.R`.
- Dictionary files: `mlr_learners.R`, `mlr_tasks.R`, etc.

#### Dictionary system

Objects are registered in dictionaries and accessed via sugar functions:

| Dictionary            | Sugar                | Example                          |
|-----------------------|----------------------|----------------------------------|
| `mlr_learners`        | `lrn()` / `lrns()`   | `lrn("classif.rpart", cp = 0.1)` |
| `mlr_tasks`           | `tsk()` / `tsks()`   | `tsk("iris")`                    |
| `mlr_measures`        | `msr()` / `msrs()`   | `msr("classif.ce")`              |
| `mlr_resamplings`     | `rsmp()` / `rsmps()` | `rsmp("cv", folds = 5)`          |
| `mlr_task_generators` | `tgen()` / `tgens()` | `tgen("friedman1")`              |

Every new object **must** be registered at the bottom of its file:

```r
#' @include mlr_learners.R
mlr_learners$add("classif.rpart", function() LearnerClassifRpart$new())
```

#### Collation order

Derived classes must declare `#' @include ParentClass.R` in their roxygen header. This controls the `Collate:` field in DESCRIPTION so base classes load before derived classes.

#### Hyperparameters (paradox)

Parameters are defined with `paradox::ps()` and must be tagged `"train"` or `"predict"`:

```r
ps = ps(
  cp = p_dbl(0, 1, default = 0.01, tags = "train"),
  keep_model = p_lgl(default = FALSE, tags = "train")
)
```

In `.train()` / `.predict()`, retrieve values with `self$param_set$get_values(tags = "train")`.

There is a distinction between `default` and `init` values:
- `default` describes the behavior when a parameter is not set at all (i.e., the upstream function's default). It is informational only.
- `init` (via `p_xxx(init = ...)`) sets the parameter to a value upon construction. Use this when the mlr3 default should differ from the upstream default.
- A parameter tagged `"required"` causes an error if not set. A required parameter cannot have a `default` (that would be contradictory).
- paradox does type-checking and range-checking automatically; `get_values()` checks that required params are present. Additional feasibility checks are rarely needed.

#### Public fields as active bindings

Public fields on R6 classes are exposed as active bindings backed by a private `.field`.

For mutable fields, the binding returns the private value when called without arguments and validates the new value with an `assert_*()` call when set:

```r
id = function(rhs) {
  if (missing(rhs)) {
    return(private$.id)
  }
  private$.id = assert_id(rhs)
},
```

For read-only fields, call `assert_ro_binding(rhs)` to raise an error on any assignment attempt:

```r
task_type = function(rhs) {
  assert_ro_binding(rhs)
  private$.data$task_type
},
```

The private slot must be declared in the `private` list as `.id = NULL` (or an appropriate default).

#### Core dependencies

`data.table`, `checkmate`, `mlr3misc`, `paradox`, `R6`, and `cli` are imported wholesale. Use their functions directly without `::`. Key mlr3misc utilities: `map()`, `map_chr()`, `invoke()`, `calculate_hash()`, `str_collapse()`, `%nin%`, `%??%`.

#### Error handling

Use structured error/warning functions from mlr3misc: `error_config()`, `error_input()`, `error_learner_train()`, `error_learner_predict()`, `warning_config()`, `warning_input()`. These support `sprintf`-style formatting.

#### Reflections

`mlr_reflections` is an environment that stores allowed types, properties, and roles. Extension packages modify it to register new task types. Check it when adding new properties or feature types.

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test_{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.
- The full test suite takes a long time. Only run tests relevant to your changes with `devtools::test(filter = '^{name}')`.
- New learners must pass `run_autotest()` and `run_paramtest()`.
- Use shared assertion helpers: `expect_learner()`, `expect_task()`, `expect_resampling()`, `expect_measure()`, `expect_prediction()`.
- Shared test infrastructure lives in `inst/testthat/` and is sourced by extension packages too.
- Use `skip_if_not_installed(<package_name>)` to skip tests that require suggested packages.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Wrap roxygen comments at 120 characters.
- Write one sentence per line.
- If a sentence exceeds the limit, break at a comma, "and", "or", "but", or other appropriate point.
- Internal functions should not have roxygen documentation.
- Whenever you add a new (non-internal) documentation topic, also add the topic to `_pkgdown.yml`.
- Always re-document the package after changing a roxygen2 comment.
- Use `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.
- Don’t hand-edit generated artifacts: `man/`, or `NAMESPACE`.
- Roxygen templates live in `man-roxygen/` (e.g., `@template learner`, `@template param_id`). Use `@templateVar` to pass values.
- Bibliographic references go in `R/bibentries.R` and are cited with `` `r format_bib("key")` ``.
- Man page names for dictionary objects follow `mlr_learners_classif.rpart`, `mlr_tasks_iris`, etc.
- When you write examples, make sure they work.
- Wrap examples that use suggested packages in `if (mlr3misc::require_namespaces(<package_name>, quietly = TRUE)) {..}` blocks.

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`. Do not add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describe the change to the end user and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any new lines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function early in the bullet.
- Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.

### GitHub

- If you use `gh` to retrieve information about an issue, always use `--comments` to read all the comments.

### Writing

- Use sentence case for headings.
- Use US English.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing.

Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level heading.

Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.

### References

- [mlr3book](https://mlr3book.mlr-org.com/) — comprehensive guide to the mlr3 ecosystem.
- [mlr3misc](https://github.com/mlr-org/mlr3misc) — helper functions used throughout the codebase.
- [paradox](https://github.com/mlr-org/paradox) — hyperparameter/configuration space definitions.
