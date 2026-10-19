# Model predictions across many sub-models

Model predictions across many sub-models

## Usage

``` r
# S3 method for class '`_brulee_auto_int`'
multi_predict(object, new_data, type = NULL, epochs = NULL, ...)

# S3 method for class '`_brulee_resnet`'
multi_predict(object, new_data, type = NULL, epochs = NULL, ...)

# S3 method for class '`_brulee_rln`'
multi_predict(object, new_data, type = NULL, epochs = NULL, ...)
```

## Arguments

- object:

  A [model
  fit](https://parsnip.tidymodels.org/reference/model_fit.html).

- new_data:

  A rectangular data object, such as a data frame.

- type:

  A single character value or `NULL`. Possible values are:

  - regression: "`numeric`"

  - classification: "`class`", "`prob`"

  - censored regression: "`survival`", "`time`", "`hazard`",
    "`linear_pred`"

  - quantile regression: "`quantile`"

  - interval estimates: "`conf_int`", "`pred_int`"

  - other: "`raw`"

  When `NULL`, [`predict()`](https://rdrr.io/r/stats/predict.html) will
  choose an appropriate value based on the model's mode.

- epochs:

  An integer vector for the number of training epochs.

- ...:

  Optional arguments to pass to `predict.model_fit(type = "raw")` such
  as `type`.

## Value

A tibble with the same number of rows as `new_data`. Its `.pred` column
is a list of tibbles, each containing the predictions for the
corresponding row of `new_data` across the requested values of `epochs`.
