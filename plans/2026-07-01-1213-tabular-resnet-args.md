# Add main arguments to `tabular_resnet()`

## Overview

Add/expose the following as _main_ arguments in `tabular_resnet()` (and
`update.tabular_resnet()`): `residual_at`, `mixture`, `rate_schedule`,
`momentum`, `batch_size`, `class_weights`, and `stop_iter`.

Decisions:
- The existing `resid_at` main argument is renamed to `residual_at` (matches
  the `brulee::brulee_resnet()` argument name). The dials parameter function
  stays `resid_at`, i.e. `func = list(pkg = "dials", fun = "resid_at")`.
- `rate_schedule`, `momentum`, `batch_size` are already registered as model
  args in `make_tabular_resnet()` but were not exposed as function arguments.
- `mixture`, `class_weights`, `stop_iter` need to be added as both function
  arguments and model-arg registrations. Use `tabular_saint()` as the template.

## Work Items

- [x] Rename `resid_at` -> `residual_at` in `tabular_resnet()` signature + args
- [x] Add `mixture`, `rate_schedule`, `momentum`, `batch_size`,
      `class_weights`, `stop_iter` to `tabular_resnet()` signature + args
- [x] Mirror the same changes in `update.tabular_resnet()`
- [x] Update `check_args.tabular_resnet()` (mixture, stop_iter checks)
- [x] Update the model-arg registrations in `make_tabular_resnet()`
      (rename resid_at parsnip name -> residual_at; add mixture,
      class_weights, stop_iter)
- [x] Update roxygen docs (@param entries) and regenerate man pages
- [x] Add/expand tests in `tests/testthat/test-tabular_resnet.R`
- [x] Run `air format`
- [x] Run R CMD check
