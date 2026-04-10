# R package development

## Key commands

    # To run code
    Rscript -e "devtools::load_all(); code"

    # To run all tests
    Rscript -e "devtools::test()"

    # To run all tests for files starting with {name}
    Rscript -e "devtools::test(filter = '^{name}')"

    # To run all tests for R/{name}.R
    Rscript -e "devtools::test_active_file('R/{name}.R')"

    # To run a single test "blah" for R/{name}.R
    Rscript -e "devtools::test_active_file('R/{name}.R', desc = 'blah')"

    # To redocument the package
    Rscript -e "devtools::document()"

    # To check pkgdown documentation
    Rscript -e "pkgdown::check_pkgdown()"

    # To check the package with R CMD check
    Rscript -e "devtools::check()"

## Code Style

- Always use `=` for assignment, never `<-`.
- 2-space indentation, 120-character line limit.
- `snake_case` for functions and variables, `CamelCase` for R6 classes.
- When calling a function from imported package `foo` do not write
  `foo::bar()` but `bar()`
- Double quotes for strings, explicit `TRUE`/`FALSE` (never `T`/`F`),
  explicit `1L` for integers.
- Use implicit return values for functions.
- Prefer `result = if (...) ... else ...` over
  `if (...) { result = ... } else { result = ... }` when the only
  difference between branches is the assigned value.
- User-facing API (exported functions, public R6 methods) must have
  `checkmate` `assert_*()` argument checks. For internal code, match the
  existing level of defensiveness.
- Use these mlr3misc utilities when appropriate: `map()`, `map_chr()`,
  `invoke()`, `calculate_hash()`, `str_collapse()`, `%nin%`, `%??%`.
- Before implementing something, read similar existing files first to
  match the established patterns.
- Always use `#nolint next` to disable linters for the next line instead
  of `# nolint` on the same line.

## File structure and naming

- Name the file as the most important contained function / class
- No whitespaces, no special chart in filenames
- Usually one large function / class, per file, but adding multiple
  smaller helpers is ok

## Collation order

- Derived classes must declare `#' @include ParentClass.R` in their
  roxygen header. This controls the `Collate:` field in DESCRIPTION so
  base classes load before derived classes.

## Core dependencies

- Use `checkmate` for arg-checks
- Use `data.table` for efficient table structures
- For OOP-stype use `R6`
- Use `cli` to format messages, warnings, errors and prints

## Testing

- Tests for `R/{name}.R` go in `tests/testthat/test_{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing
  tests.
- Strive to keep your tests minimal with few comments.
- The full test suite takes a long time. Only run tests relevant to your
  changes with `devtools::test(filter = '^{name}')`.

## Documentation

- Every user-facing function should be exported and have roxygen2
  documentation.
- Wrap roxygen comments at 120 characters.
- Write one sentence per line.
- If a sentence exceeds the limit, break at a comma, “and”, “or”, “but”,
  or other appropriate point.
- Internal functions should not have roxygen documentation.
- Always re-document the package after changing a roxygen2 comment.
- Don’t hand-edit generated artifacts: `man/`, or `NAMESPACE`.
- Never edit `README.md` directly – it is generated from `README.Rmd`.
  Always edit `README.Rmd` and then run `devtools::build_readme()` to
  regenerate `README.md`.
- When adding a new S3 method (such as `print.<ClassName>`), always run
  `devtools::document()` afterwards to re-generate the NAMESPACE.
- Environment variables and options are documented in package-level
  documentation (typically `R/package.R`).
- Roxygen templates live in `man-roxygen/`. Use `@template` to avoid
  duplicating common parameter descriptions. Only create new templates
  for sections that will likely be re-used.
- For functions, always document the return value (section
  `#' @return`).
- Bibliographic references go in `R/bibentries.R` and are cited with
  `` `r format_bib("key")` ``.

## Pkgdown

- When adding a new exported function, ensure it’s in the `_pkgdown.yml`
  file.

## `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`. Do not
  add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describe the change to the end user and
  mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any
  new lines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function
  early in the bullet.
- Order bullets alphabetically by function name. Put all bullets that
  don’t mention function names at the beginning.

## GitHub

- If you use `gh` to retrieve information about an issue, always use
  `--comments` to read all the comments.

## Natural Language

- The following applies to all natural language text, so docs,
  commments, NEWS, etc, but not code
- Use American english
- Use the Oxford comma
- Do not capitalize normal nouns or method names. “Bayesian” is
  capitalized, “random forest” is not.
- Use cspell to check against typos, and add needed words to
  .cspell/project-words.txt if reasonable

## Further agents files

- Read and respect all files in the `extra-rules` folder
