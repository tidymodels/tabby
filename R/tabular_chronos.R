#' @method required_pkgs tabular_chronos
#' @export
required_pkgs.tabular_chronos <- function(x, infra = TRUE, ...) {
  c("brulee", "tabby")
}

# ------------------------------------------------------------------------------
# Prediction post-processors: brulee_chronos returns a tibble with `.pred`
# (median) and `.pred_quantile` (a hardhat::quantile_pred). For multiple series
# it also prepends an id column; the parsnip interface only forecasts a single
# series (one horizon-length output cannot label which series each row belongs
# to), so `chronos_single_series()` errors rather than silently dropping the id.

chronos_single_series <- function(x, call = rlang::caller_env()) {
  extra <- setdiff(names(x), c(".pred", ".pred_quantile"))
  if (length(extra) > 0L) {
    cli::cli_abort(
      c(
        "The {.pkg parsnip} interface to {.fn brulee::brulee_chronos} forecasts a single series.",
        "i" = "Multiple series were detected (id column {.val {extra}}).",
        "i" = "For multi-series forecasting, use {.fn brulee::brulee_chronos} directly."
      ),
      call = call
    )
  }
  invisible(x)
}

chronos_quantile <- function(x, object) {
  chronos_single_series(x)
  tibble::tibble(.pred_quantile = x$.pred_quantile)
}

chronos_numeric <- function(x, object) {
  chronos_single_series(x)
  tibble::tibble(.pred = x$.pred)
}

# ------------------------------------------------------------------------------

make_tabular_chronos <- function() {
  # Quantile regression

  parsnip::set_model_engine(
    "tabular_chronos",
    mode = "quantile regression",
    eng = "brulee"
  )
  parsnip::set_dependency(
    "tabular_chronos",
    eng = "brulee",
    pkg = "brulee",
    mode = "quantile regression"
  )
  parsnip::set_dependency(
    "tabular_chronos",
    eng = "brulee",
    pkg = "tabby",
    mode = "quantile regression"
  )

  parsnip::set_fit(
    model = "tabular_chronos",
    eng = "brulee",
    mode = "quantile regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "brulee", fun = "brulee_chronos"),
      # `quantile_levels` is bound into the fit eval_env from the spec's mode;
      # forward it to brulee (mirrors linear_reg() + quantreg's `tau`).
      defaults = list(quantile_levels = rlang::expr(quantile_levels))
    )
  )

  parsnip::set_encoding(
    model = "tabular_chronos",
    eng = "brulee",
    mode = "quantile regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "tabular_chronos",
    eng = "brulee",
    mode = "quantile regression",
    type = "quantile",
    value = list(
      pre = NULL,
      post = chronos_quantile,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data)
      )
    )
  )

  # ----------------------------------------------------------------------------
  # Regression (median point forecast)

  parsnip::set_model_engine(
    "tabular_chronos",
    mode = "regression",
    eng = "brulee"
  )
  parsnip::set_dependency(
    "tabular_chronos",
    eng = "brulee",
    pkg = "brulee",
    mode = "regression"
  )
  parsnip::set_dependency(
    "tabular_chronos",
    eng = "brulee",
    pkg = "tabby",
    mode = "regression"
  )

  parsnip::set_fit(
    model = "tabular_chronos",
    eng = "brulee",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "brulee", fun = "brulee_chronos"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_chronos",
    eng = "brulee",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "tabular_chronos",
    eng = "brulee",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = chronos_numeric,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data)
      )
    )
  )
}
