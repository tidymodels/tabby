# Remove a model's registration from parsnip's model environment so that the
# corresponding `make_*()` function can be called again. parsnip registers
# these models in `.onLoad()`, which covr does not attribute to any test;
# clearing and re-running registration lets coverage reflect the registration
# code honestly.
reregister_model <- function(model) {
  e <- parsnip::get_model_env()
  nms <- grep(paste0("^", model, "($|_)"), rlang::env_names(e), value = TRUE)
  rlang::env_poke(e, "models", setdiff(rlang::env_get(e, "models"), model))
  rlang::env_unbind(e, nms)
  invisible(NULL)
}
