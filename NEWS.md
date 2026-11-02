# tabby (development version)

* Engine arguments for the brulee and tabpfn engines are now registered as tuning parameters: `validation` and the learning rate schedule arguments (`initial`, `largest`, `reduction`, `steps`, `step_size`, `decay`) for `tabular_auto_int()`, `tabular_resnet()`, `tabular_rln()`, and `tabular_saint()`; `normalization` and `training_set_limit` for `tabular_icl()`; and `training_set_limit` for `tabular_pfn()`.

# tabby 0.0.1

* Initial CRAN submission.
