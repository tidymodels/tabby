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
