#' @method required_pkgs tabular_rln
#' @export
required_pkgs.tabular_rln <- function(x, infra = TRUE, ...) {
  c("brulee", "tabby")
}

# ------------------------------------------------------------------------------

#' Model predictions across many sub-models
#' @importFrom purrr map
#' @importFrom dplyr arrange select
#' @rdname multi_predict
#' @inheritParams parsnip::multi_predict
#' @param epochs An integer vector for the number of training epochs.
#' @export
multi_predict._brulee_rln <-
  function(object, new_data, type = NULL, epochs = NULL, ...) {
    load_libs(object, quiet = TRUE, attach = TRUE)

    if (is.null(epochs)) {
      epochs <- length(object$fit$estimates) - 1L
    }

    epochs <- sort(epochs)

    if (is.null(type)) {
      type <- "numeric"
    }

    res <-
      purrr::map(
        epochs,
        ~ predict(object$fit, new_data, type = type, epoch = .x) |>
          dplyr::mutate(epochs = .x)
      ) |>
      purrr::map(\(x) x |> dplyr::mutate(.row = seq_len(nrow(new_data)))) |>
      purrr::list_rbind() |>
      dplyr::arrange(.row, epochs)
    res <- split(dplyr::select(res, -.row), res$.row)
    names(res) <- NULL
    tibble::tibble(.pred = res)
  }

# ------------------------------------------------------------------------------

make_tabular_rln <- function() {
  parsnip::set_model_engine("tabular_rln", mode = "regression", eng = "brulee")
  parsnip::set_dependency(
    "tabular_rln",
    eng = "brulee",
    pkg = "brulee",
    mode = "regression"
  )
  parsnip::set_dependency(
    "tabular_rln",
    eng = "brulee",
    pkg = "tabby",
    mode = "regression"
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "hidden_units",
    original = "hidden_units",
    func = list(pkg = "dials", fun = "hidden_units"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "penalty_type",
    original = "penalty_type",
    func = list(pkg = "dials", fun = "penalty_type"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "penalty_average",
    original = "penalty_average",
    func = list(pkg = "dials", fun = "penalty_average"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "step_rate",
    original = "step_rate",
    func = list(pkg = "dials", fun = "step_rate"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "activation",
    original = "activation",
    func = list(
      pkg = "dials",
      fun = "activation",
      values = c("relu", "elu", "tanh")
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "epochs",
    original = "epochs",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "learn_rate",
    original = "learn_rate",
    func = list(pkg = "dials", fun = "learn_rate", range = c(-2.5, -0.5)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "rate_schedule",
    original = "rate_schedule",
    func = list(pkg = "dials", fun = "rate_schedule"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "momentum",
    original = "momentum",
    func = list(pkg = "dials", fun = "momentum", range = c(0.50, 0.99)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "batch_size",
    original = "batch_size",
    func = list(pkg = "dials", fun = "batch_size", range = c(4, 7)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_rln",
    eng = "brulee",
    parsnip = "stop_iter",
    original = "stop_iter",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "tabular_rln",
    eng = "brulee",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "brulee", fun = "brulee_rln"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_rln",
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
    model = "tabular_rln",
    eng = "brulee",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = reformat_torch_num,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "numeric"
      )
    )
  )
}
