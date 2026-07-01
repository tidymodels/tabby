# tabular_rln() only supports regression mode

    Code
      parsnip::set_engine(tabular_rln(mode = "classification"), "brulee")
    Condition
      Error in `tabular_rln()`:
      ! "classification" is not a known mode for model `tabular_rln()`.

# check_args.tabular_rln() rejects invalid penalty_type

    Code
      parsnip::check_args(tabular_rln(penalty_type = "L3"))
    Condition
      Error:
      ! `penalty_type` must be "L1" or "L2", not "L3".

# check_args.tabular_rln() rejects negative penalty_average

    Code
      parsnip::check_args(tabular_rln(penalty_average = -1))
    Condition
      Error:
      ! `penalty_average` must be a number larger than or equal to 0 or `NULL`, not the number -1.

# check_args.tabular_rln() rejects negative step_rate

    Code
      parsnip::check_args(tabular_rln(step_rate = -1))
    Condition
      Error:
      ! `step_rate` must be a number larger than or equal to 0 or `NULL`, not the number -1.

