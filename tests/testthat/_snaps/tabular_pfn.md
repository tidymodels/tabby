# check_args.tabular_pfn() validates softmax_temperature

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `softmax_temperature` must be a number larger than or equal to 0 or `NULL`, not the number -1.

# check_args.tabular_pfn() validates num_estimators

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `num_estimators` must be a whole number larger than or equal to 0 or `NULL`, not the number -1.

# check_args.tabular_pfn() validates balance_probabilities

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `balance_probabilities` must be a logical vector or `NULL`, not the string "yes".

# check_args.tabular_pfn() validates average_before_softmax

    Code
      parsnip::check_args(spec)
    Condition
      Error:
      ! `average_before_softmax` must be a logical vector or `NULL`, not the number 1.

