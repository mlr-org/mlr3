# Manually Partition into Training, Test and Validation Set

Creates a split of the row ids of a
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) into a training
and a test set, and optionally a validation set.

## Usage

``` r
partition(task, ratio = 0.67)

# S3 method for class 'Task'
partition(task, ratio = 0.67)
```

## Arguments

- task:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md))  
  Task to operate on.

- ratio:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Ratio of observations to put into the training set. If a 2 element
  vector is provided, the first element is the ratio for the training
  set, the second element is the ratio for the test set. The validation
  set will contain the remaining observations.

## Examples

``` r
# regression task partitioned into training and test set
task = tsk("california_housing")
split = partition(task, ratio = 0.5)
data = data.frame(
  y = c(task$truth(split$train), task$truth(split$test)),
  split = rep(c("train", "predict"), lengths(split[c("train", "test")]))
)
boxplot(y ~ split, data = data)


# classification task partitioned into training, test and validation set
task = tsk("pima")
split = partition(task, c(0.66, 0.14))
```
