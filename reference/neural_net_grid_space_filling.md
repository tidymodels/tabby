# Working with Tuning Paramters that are lists

`neural_net_grid_space_filling()` takes the number of layers in a neural
layer parameter(s), and collapses the grid into list parameters that the
model function needs. `expand_list_parameters()` is a convenience
function that converts the list columns to a wide structure for
printing, visualization, etc.

## Usage

``` r
neural_net_grid_space_filling(
  wflow,
  num_layers = 3,
  size = 10,
  param_info = NULL,
  range = c(2, 50),
  collapse = TRUE
)

expand_list_parameters(x, pattern = "*")
```

## Arguments

- wflow:

  A `workflows::workflow()` object.

- num_layers:

  A single integer for the number of layers containing hidden units.

- size:

  The *minimum* grid size. It may be adjusted upwards to find a feasable
  design.

- param_info:

  A
  [`dials::parameters()`](https://dials.tidymodels.org/reference/parameters.html)
  object or `NULL`. If none is given, a parameters set is derived from
  other arguments. Passing this argument can be useful when parameter
  ranges need to be customized.

- range:

  A range for the number of hidden units in each layer.

- collapse:

  A single logical for whether to collapses the parameters into list
  columns.

- x:

  A data frame of grid points, some of which as list columns.

- pattern:

  A regular expression pattern to select which list columns should be
  expanded to a wide format.

## Value

A tibble with grid points, some of which are list-columns containing
integer vectors.

## Examples

``` r
rn_spec <-
  tabular_resnet(hidden_units = tune(),
             bottleneck_units = tune(),
             penalty = tune())

rn_grid <- neural_net_grid_space_filling(rn_spec)
rn_grid
#> # A tibble: 10 × 3
#>    hidden_units bottleneck_units       penalty
#>    <list>       <list>                   <dbl>
#>  1 <int [3]>    <int [3]>        0.00599      
#>  2 <int [3]>    <int [3]>        0.0000359    
#>  3 <int [3]>    <int [3]>        0.00000278   
#>  4 <int [3]>    <int [3]>        0.0000000001 
#>  5 <int [3]>    <int [3]>        0.00000000129
#>  6 <int [3]>    <int [3]>        0.0774       
#>  7 <int [3]>    <int [3]>        0.000000215  
#>  8 <int [3]>    <int [3]>        0.000464     
#>  9 <int [3]>    <int [3]>        1            
#> 10 <int [3]>    <int [3]>        0.0000000167 

rn_grid |> expand_list_parameters()
#> # A tibble: 10 × 7
#>          penalty hidden_units_1 hidden_units_2 hidden_units_3
#>            <dbl>          <int>          <int>          <int>
#>  1 0.00599                    2             50             28
#>  2 0.0000359                  7             18             34
#>  3 0.00000278                12              2             44
#>  4 0.0000000001              18             28             18
#>  5 0.00000000129             23             34              2
#>  6 0.0774                    28              7             12
#>  7 0.000000215               34             39             39
#>  8 0.000464                  39             12              7
#>  9 1                         44             44             23
#> 10 0.0000000167              50             23             50
#> # ℹ 3 more variables: bottleneck_units_1 <int>,
#> #   bottleneck_units_2 <int>, bottleneck_units_3 <int>
```
