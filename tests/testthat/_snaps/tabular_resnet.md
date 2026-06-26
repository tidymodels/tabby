# check_args.tabular_resnet() validates penalty

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

# check_args.tabular_resnet() validates dropout range

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `dropout` must be a number between 0 and 1 or `NULL`, not the number 1.5.

# check_args.tabular_resnet() rejects both penalty and dropout

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! Both weight decay and dropout should not be specified.

# tabular_resnet() does not support multi_predict()

    Code
      parsnip::multi_predict(fit, mtcars[1:3, ], epochs = c(2L, 4L))
    Condition
      Error in `parsnip::multi_predict()`:
      ! No `multi_predict()` method exists for objects with classes <_brulee_resnet/model_fit>.

