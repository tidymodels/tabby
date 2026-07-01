# check_args.tabular_saint() validates dropout_attn range

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `dropout_attn` must be a number between 0 and 1 or `NULL`, not the number 1.5.

# check_args.tabular_saint() validates dropout_hidden range

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `dropout_hidden` must be a number between 0 and 1 or `NULL`, not the number 2.

# check_args.tabular_saint() validates dropout_last range

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `dropout_last` must be a number between 0 and 1 or `NULL`, not the number -0.1.

# check_args.tabular_saint() validates penalty

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

# check_args.tabular_saint() validates mixture range

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number 2.

# check_args.tabular_saint() validates integer params

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `epochs` must be a whole number larger than or equal to 1 or `NULL`, not the number -1.

---

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `num_attn_heads` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

# check_args.tabular_saint() validates attention_type

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `attention_type` must be one of "column", "row", or "both".

# check_args.tabular_saint() rejects both penalty and dropout_attn

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! Both weight decay and dropout should not be specified.

# tabular_saint() does not support multi_predict()

    Code
      parsnip::multi_predict(fit, mtcars[1:3, ], epochs = c(3L, 7L))
    Condition
      Error in `parsnip::multi_predict()`:
      ! No `multi_predict()` method exists for objects with classes <_brulee_saint/model_fit>.

