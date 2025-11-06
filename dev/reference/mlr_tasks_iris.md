# Iris Classification Task

A classification task for the popular
[datasets::iris](https://rdrr.io/r/datasets/iris.html) data set.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) inheriting
from
[TaskClassif](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md).

## Source

<https://en.wikipedia.org/wiki/Iris_flower_data_set>

Anderson E (1936). “The Species Problem in Iris.” *Annals of the
Missouri Botanical Garden*, **23**(3), 457.
[doi:10.2307/2394164](https://doi.org/10.2307/2394164) .

## Dictionary

This [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md) or with
the associated sugar function
[`tsk()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_tasks$get("iris")
    tsk("iris")

## Meta Information

- Task type: “classif”

- Dimensions: 150x5

- Properties: “multiclass”

- Has Missings: `FALSE`

- Target: “Species”

- Features: “Petal.Length”, “Petal.Width”, “Sepal.Length”, “Sepal.Width”

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html>

- Package [mlr3data](https://CRAN.R-project.org/package=mlr3data) for
  more toy tasks.

- Package [mlr3oml](https://CRAN.R-project.org/package=mlr3oml) for
  downloading tasks from <https://www.openml.org>.

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of [Tasks](https://mlr3.mlr-org.com/dev/reference/Task.md):
  [mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md)

- `as.data.table(mlr_tasks)` for a table of available
  [Tasks](https://mlr3.mlr-org.com/dev/reference/Task.md) in the running
  session (depending on the loaded packages).

- [mlr3fselect](https://CRAN.R-project.org/package=mlr3fselect) and
  [mlr3filters](https://CRAN.R-project.org/package=mlr3filters) for
  feature selection and feature filtering.

- Extension packages for additional task types:

  - Unsupervised clustering:
    [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster)

  - Probabilistic supervised regression and survival analysis:
    <https://mlr3proba.mlr-org.com/>.

Other Task: [`Task`](https://mlr3.mlr-org.com/dev/reference/Task.md),
[`TaskClassif`](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md),
[`TaskRegr`](https://mlr3.mlr-org.com/dev/reference/TaskRegr.md),
[`TaskSupervised`](https://mlr3.mlr-org.com/dev/reference/TaskSupervised.md),
[`TaskUnsupervised`](https://mlr3.mlr-org.com/dev/reference/TaskUnsupervised.md),
[`california_housing`](https://mlr3.mlr-org.com/dev/reference/california_housing.md),
[`mlr_tasks`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md),
[`mlr_tasks_breast_cancer`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_breast_cancer.md),
[`mlr_tasks_german_credit`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_german_credit.md),
[`mlr_tasks_mtcars`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_mtcars.md),
[`mlr_tasks_penguins`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_penguins.md),
[`mlr_tasks_pima`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_pima.md),
[`mlr_tasks_sonar`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_sonar.md),
[`mlr_tasks_spam`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_spam.md),
[`mlr_tasks_wine`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_wine.md),
[`mlr_tasks_zoo`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_zoo.md)
