# Check Column Roles

Internal function to check column roles.

## Usage

``` r
task_check_col_roles(task, new_roles, ...)

# S3 method for class 'Task'
task_check_col_roles(task, new_roles, ...)

# S3 method for class 'TaskClassif'
task_check_col_roles(task, new_roles, ...)

# S3 method for class 'TaskRegr'
task_check_col_roles(task, new_roles, ...)

# S3 method for class 'TaskSupervised'
task_check_col_roles(task, new_roles, ...)

# S3 method for class 'TaskUnsupervised'
task_check_col_roles(task, new_roles, ...)
```

## Arguments

- task:

  ([Task](https://mlr3.mlr-org.com/reference/Task.md))  
  Task.

- new_roles:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Column roles.
