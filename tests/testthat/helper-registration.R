# Reset the engine-level registrations that tabby's `make_*()` functions add to
# parsnip's model environment so that `make_*()` can be called again. The base
# model (its `models` list entry and its modes) is registered by parsnip itself,
# so it is left intact; only the engine, argument, fit, encoding, prediction,
# and dependency tables that tabby populates are cleared. tabby runs `make_*()`
# in `.onLoad()`, which covr does not attribute to any test; clearing and
# re-running registration lets coverage reflect the registration code honestly.
reregister_model <- function(model) {
  e <- parsnip::get_model_env()
  tables <- c(
    model,
    paste0(model, c("_args", "_fit", "_encoding", "_predict", "_pkgs"))
  )
  for (nm in intersect(tables, rlang::env_names(e))) {
    tbl <- rlang::env_get(e, nm)
    rlang::env_poke(e, nm, tbl[0, , drop = FALSE])
  }
  invisible(NULL)
}

expect_engine_args <- function(spec, args) {
  res <- generics::tunable(spec)
  res <- res[res$component_id == "engine", ]
  expect_setequal(res$name, names(args))
  funs <- purrr::map_chr(res$call_info, "fun")
  expect_equal(funs, unname(args[res$name]))
  pkgs <- purrr::map_chr(res$call_info, "pkg")
  expect_all_equal(pkgs, "dials")
}

brulee_rate_args <- c(
  validation = "validation_set_prop",
  initial = "rate_initial",
  largest = "rate_largest",
  reduction = "rate_reduction",
  steps = "rate_steps",
  step_size = "rate_step_size",
  decay = "rate_decay"
)
