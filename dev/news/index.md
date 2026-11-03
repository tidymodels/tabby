# Changelog

## tabby (development version)

- Engine arguments for the brulee and tabpfn engines are now registered
  as tuning parameters: the learning rate schedule arguments (`initial`,
  `largest`, `reduction`, `steps`, `step_size`, `decay`) for
  [`tabular_auto_int()`](https://parsnip.tidymodels.org/reference/tabular_auto_int.html),
  [`tabular_resnet()`](https://parsnip.tidymodels.org/reference/tabular_resnet.html),
  [`tabular_rln()`](https://parsnip.tidymodels.org/reference/tabular_rln.html),
  and
  [`tabular_saint()`](https://parsnip.tidymodels.org/reference/tabular_saint.html);
  `normalization` and `training_set_limit` for
  [`tabular_icl()`](https://parsnip.tidymodels.org/reference/tabular_icl.html);
  and `training_set_limit` for
  [`tabular_pfn()`](https://parsnip.tidymodels.org/reference/tabular_pfn.html).

## tabby 0.0.1

CRAN release: 2026-10-15

- Initial CRAN submission.
