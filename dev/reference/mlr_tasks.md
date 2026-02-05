# Dictionary of Tasks

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md). Each task has an
associated help page, see `mlr_tasks_[id]`.

This dictionary can get populated with additional tasks by add-on
packages, e.g. [mlr3data](https://CRAN.R-project.org/package=mlr3data),
[mlr3proba](https://CRAN.R-project.org/package=mlr3proba) or
[mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster).
[mlr3oml](https://CRAN.R-project.org/package=mlr3oml) allows to interact
with [OpenML](https://www.openml.org).

For a more convenient way to retrieve and construct tasks, see
[`tsk()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md)/[`tsks()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## S3 methods

- `as.data.table(dict, ..., objects = FALSE)`  
  [mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  -\>
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Returns a
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with columns "key", "label", "task_type", "nrow", "ncol",
  "properties", and the number of features of type "lgl", "int", "dbl",
  "chr", "fct" and "ord", respectively. If `objects` is set to `TRUE`,
  the constructed objects are returned in the list column named
  `object`.

## See also

Sugar functions:
[`tsk()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md),
[`tsks()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md)

Extension Packages:
[mlr3data](https://CRAN.R-project.org/package=mlr3data)

Other Dictionary:
[`mlr_learners`](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md),
[`mlr_measures`](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md),
[`mlr_resamplings`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md),
[`mlr_task_generators`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md)

Other Task: [`Task`](https://mlr3.mlr-org.com/dev/reference/Task.md),
[`TaskClassif`](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md),
[`TaskRegr`](https://mlr3.mlr-org.com/dev/reference/TaskRegr.md),
[`TaskSupervised`](https://mlr3.mlr-org.com/dev/reference/TaskSupervised.md),
[`TaskUnsupervised`](https://mlr3.mlr-org.com/dev/reference/TaskUnsupervised.md),
[`california_housing`](https://mlr3.mlr-org.com/dev/reference/california_housing.md),
[`mlr_tasks_breast_cancer`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_breast_cancer.md),
[`mlr_tasks_german_credit`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_german_credit.md),
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
as.data.table(mlr_tasks)
#> Key: <key>
#>                    key                   label task_type  nrow  ncol properties
#>                 <char>                  <char>    <char> <int> <int>     <list>
#>  1:      breast_cancer Wisconsin Breast Cancer   classif   683    10   twoclass
#>  2: california_housing  California House Value      regr 20640    10           
#>  3:      german_credit           German Credit   classif  1000    21   twoclass
#>  4:               iris            Iris Flowers   classif   150     5 multiclass
#>  5:             mtcars            Motor Trends      regr    32    11           
#>  6:           penguins         Palmer Penguins   classif   344     8 multiclass
#>  7:               pima    Pima Indian Diabetes   classif   768     9   twoclass
#>  8:              sonar  Sonar: Mines vs. Rocks   classif   208    61   twoclass
#>  9:               spam       HP Spam Detection   classif  4601    58   twoclass
#> 10:               wine            Wine Regions   classif   178    14 multiclass
#> 11:                zoo             Zoo Animals   classif   101    17 multiclass
#>       lgl   int   dbl   chr   fct   ord   pxc   dte
#>     <int> <int> <int> <int> <int> <int> <int> <int>
#>  1:     0     0     0     0     0     9     0     0
#>  2:     0     0     8     0     1     0     0     0
#>  3:     0     3     0     0    14     3     0     0
#>  4:     0     0     4     0     0     0     0     0
#>  5:     0     0    10     0     0     0     0     0
#>  6:     0     3     2     0     2     0     0     0
#>  7:     0     0     8     0     0     0     0     0
#>  8:     0     0    60     0     0     0     0     0
#>  9:     0     0    57     0     0     0     0     0
#> 10:     0     2    11     0     0     0     0     0
#> 11:    15     1     0     0     0     0     0     0
task = mlr_tasks$get("penguins") # same as tsk("penguins")
head(task$data())
#>    species bill_depth bill_length body_mass flipper_length    island    sex
#>     <fctr>      <num>       <num>     <int>          <int>    <fctr> <fctr>
#> 1:  Adelie       18.7        39.1      3750            181 Torgersen   male
#> 2:  Adelie       17.4        39.5      3800            186 Torgersen female
#> 3:  Adelie       18.0        40.3      3250            195 Torgersen female
#> 4:  Adelie         NA          NA        NA             NA Torgersen   <NA>
#> 5:  Adelie       19.3        36.7      3450            193 Torgersen female
#> 6:  Adelie       20.6        39.3      3650            190 Torgersen   male
#>     year
#>    <int>
#> 1:  2007
#> 2:  2007
#> 3:  2007
#> 4:  2007
#> 5:  2007
#> 6:  2007

# Add a new task, based on a subset of penguins:
data = palmerpenguins::penguins
data$species = factor(ifelse(data$species == "Adelie", "1", "0"))
task = TaskClassif$new("penguins.binary", data, target = "species", positive = "1")

# add to dictionary
mlr_tasks$add("penguins.binary", task)

# list available tasks
mlr_tasks$keys()
#>  [1] "breast_cancer"      "california_housing" "german_credit"     
#>  [4] "iris"               "mtcars"             "penguins"          
#>  [7] "penguins.binary"    "pima"               "sonar"             
#> [10] "spam"               "wine"               "zoo"               

# retrieve from dictionary
mlr_tasks$get("penguins.binary")
#> 
#> ── <TaskClassif> (344x8) ───────────────────────────────────────────────────────
#> • Target: species
#> • Target classes: 1 (positive class, 44%), 0 (56%)
#> • Properties: twoclass
#> • Features (7):
#>   • int (3): body_mass_g, flipper_length_mm, year
#>   • dbl (2): bill_depth_mm, bill_length_mm
#>   • fct (2): island, sex

# remove task again
mlr_tasks$remove("penguins.binary")
```
