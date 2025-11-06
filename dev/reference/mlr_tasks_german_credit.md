# German Credit Classification Task

A classification task for the German credit data set. The aim is to
predict creditworthiness, labeled as "good" and "bad". Positive class is
set to label "good".

See example for the creation of a
[MeasureClassifCosts](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.costs.md)
as described misclassification costs.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) inheriting
from
[TaskClassif](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md).

## Source

Data set originally published on
[UCI](http://archive.ics.uci.edu/dataset/144/statlog+german+credit+data).
This is the preprocessed version taken from package
[rchallenge](https://CRAN.R-project.org/package=rchallenge) with factors
instead of dummy variables, and corrected as proposed by Ulrike
Grömping.

Donor: Professor Dr. Hans Hofmann  
Institut für Statistik und Ökonometrie  
Universität Hamburg  
FB Wirtschaftswissenschaften  
Von-Melle-Park 5  
2000 Hamburg 13

## Dictionary

This [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md) or with
the associated sugar function
[`tsk()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_tasks$get("german_credit")
    tsk("german_credit")

## Meta Information

- Task type: “classif”

- Dimensions: 1000x21

- Properties: “twoclass”

- Has Missings: `FALSE`

- Target: “credit_risk”

- Features: “age”, “amount”, “credit_history”, “duration”,
  “employment_duration”, “foreign_worker”, “housing”,
  “installment_rate”, “job”, “number_credits”, “other_debtors”,
  “other_installment_plans”, “people_liable”, “personal_status_sex”,
  “present_residence”, “property”, “purpose”, “savings”, “status”,
  “telephone”

## References

Grömping U (2019). “South German Credit Data: Correcting a Widely Used
Data Set.” Reports in Mathematics, Physics and Chemistry 4, Department
II, Beuth University of Applied Sciences Berlin.

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
[`mlr_tasks_iris`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_iris.md),
[`mlr_tasks_mtcars`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_mtcars.md),
[`mlr_tasks_penguins`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_penguins.md),
[`mlr_tasks_pima`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_pima.md),
[`mlr_tasks_sonar`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_sonar.md),
[`mlr_tasks_spam`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_spam.md),
[`mlr_tasks_wine`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_wine.md),
[`mlr_tasks_zoo`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_zoo.md)

## Examples

``` r
task = tsk("german_credit")
costs = matrix(c(0, 1, 5, 0), nrow = 2)
dimnames(costs) = list(predicted = task$class_names, truth = task$class_names)
measure = msr("classif.costs", id = "german_credit_costs", costs = costs)
print(measure)
#> 
#> ── <MeasureClassifCosts> (german_credit_costs): Cost-sensitive Classification ──
#> • Packages: mlr3
#> • Range: [0, Inf]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: normalize=TRUE
#> • Properties: weights
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()
```
