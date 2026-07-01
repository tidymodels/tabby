# check_args.tabular_auto_int() validates dropout range

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `dropout` must be a number between 0 and 1 or `NULL`, not the number 1.5.

# check_args.tabular_auto_int() validates dropout_attn range

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `dropout_attn` must be a number between 0 and 1 or `NULL`, not the number 2.

# check_args.tabular_auto_int() validates dropout_embedding range

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `dropout_embedding` must be a number between 0 and 1 or `NULL`, not the number -0.1.

# check_args.tabular_auto_int() validates penalty

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

# check_args.tabular_auto_int() validates mixture range

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number 2.

# check_args.tabular_auto_int() validates integer params

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

# check_args.tabular_auto_int() rejects both penalty and dropout

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! Both weight decay and dropout should not be specified.

