#' Chronos-2 pretrained forecasting model
#'
#' @description
#' `tabular_chronos()` defines a pretrained time-series forecasting model that
#' produces quantile (distributional) forecasts. The network has fixed
#' pretrained weights, so no training is performed; the historical ("context")
#' data is ingested at fit time and the model forecasts a fixed horizon. This
#' function can fit quantile regression and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_chronos")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the type of model. The possible
#'   values for this model are `"quantile regression"` (the natural mode, which
#'   returns a `hardhat::quantile_pred()`) and `"regression"` (which returns the
#'   median point forecast). The mode must be set before fitting; for
#'   `"quantile regression"` it is set with
#'   `set_mode("quantile regression", quantile_levels = ...)`.
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. The only valid value is `"brulee"`.
#'
#' @details
#' Unlike the other models in this package, Chronos-2 is pretrained and has no
#' tuning parameters. Forecast configuration is supplied through the engine with
#' [parsnip::set_engine()], e.g. `set_engine("brulee", prediction_length = 14)`.
#' The available engine arguments mirror [brulee::brulee_chronos()]:
#' `prediction_length`, `id_column`, `timestamp_column`, `model_id`, `revision`,
#' `device`, and `cache_dir`. The `quantile_levels` are taken from the mode (via
#' [parsnip::set_mode()]) and forwarded to the fit automatically.
#'
#' On first use the engine downloads the pretrained weights (about 500MB) and
#' caches them locally.
#'
#' @templateVar modeltype tabular_chronos
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_chronos")} [brulee::brulee_chronos()]
#'
#' @references
#' Ansari, A. F., Shchur, O., Küken, J., Auer, A., Han, B., Mercado, P., et al.
#' (2025). "Chronos-2: From univariate to universal forecasting."
#' _arXiv preprint_ arXiv:2510.15821.
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_chronos")
#'
#' # Quantile (distributional) forecast
#' tabular_chronos() |>
#'   set_engine("brulee", prediction_length = 14) |>
#'   set_mode("quantile regression", quantile_levels = (1:9) / 10)
#'
#' # Median point forecast
#' tabular_chronos() |>
#'   set_engine("brulee", prediction_length = 14) |>
#'   set_mode("regression")
#' @export
tabular_chronos <-
  function(mode = "unknown", engine = "brulee") {
    args <- list()

    parsnip::new_model_spec(
      "tabular_chronos",
      args = args,
      eng_args = NULL,
      mode = mode,
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
  }

# ------------------------------------------------------------------------------

#' @method update tabular_chronos
#' @rdname tabular_update
#' @inheritParams tabular_chronos
#' @inheritParams update.tab_resnet
#' @export
update.tabular_chronos <-
  function(object, parameters = NULL, fresh = FALSE, ...) {
    parsnip::update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = list(),
      fresh = fresh,
      cls = "tabular_chronos",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @method check_args tabular_chronos
#' @export
check_args.tabular_chronos <- function(object, call = rlang::caller_env()) {
  # No main (tunable) arguments to validate; forecast configuration is supplied
  # as engine arguments and checked by brulee::brulee_chronos().
  invisible(object)
}

#' @method required_pkgs tabular_chronos
#' @export
required_pkgs.tabular_chronos <- function(x, infra = TRUE, ...) {
  c("brulee", "tabular")
}

# ------------------------------------------------------------------------------
# Prediction post-processors: brulee_chronos returns a tibble with `.pred`
# (median) and `.pred_quantile` (a hardhat::quantile_pred), plus an id column
# for multi-series output. Select the column parsnip expects for each type.

chronos_quantile <- function(x, object) {
  tibble::tibble(.pred_quantile = x$.pred_quantile)
}

chronos_numeric <- function(x, object) {
  tibble::tibble(.pred = x$.pred)
}

# ------------------------------------------------------------------------------

make_tabular_chronos <- function() {
  parsnip::set_new_model("tabular_chronos")
  parsnip::set_model_mode("tabular_chronos", mode = "quantile regression")
  parsnip::set_model_mode("tabular_chronos", mode = "regression")

  # ----------------------------------------------------------------------------
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
