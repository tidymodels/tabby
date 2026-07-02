# check_args.tabular_icl() validates softmax_temperature

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `softmax_temperature` must be a number larger than or equal to 0 or `NULL`, not the number -1.

# check_args.tabular_icl() validates num_estimators

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `num_estimators` must be a whole number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `num_estimators` must be a whole number or `NULL`, not the number 1.5.

