#' @export
required_pkgs.tabular_auto_int <- function(x, infra = TRUE, ...) {
  c("brulee", "tabby")
}

## -----------------------------------------------------------------------------

#' @importFrom purrr map
#' @importFrom dplyr arrange select
#' @rdname multi_predict
#' @param epochs An integer vector for the number of training epochs.
#' @export
multi_predict._brulee_auto_int <-
  function(object, new_data, type = NULL, epochs = NULL, ...) {
    load_libs(object, quiet = TRUE, attach = TRUE)

    if (is.null(epochs)) {
      epochs <- length(object$fit$estimates) - 1L
    }

    epochs <- sort(epochs)

    if (is.null(type)) {
      if (object$spec$mode == "classification") {
        type <- "class"
      } else {
        type <- "numeric"
      }
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

make_tabular_auto_int <- function() {
  parsnip::set_model_engine(
    "tabular_auto_int",
    mode = "classification",
    eng = "brulee"
  )

  parsnip::set_model_engine(
    "tabular_auto_int",
    mode = "regression",
    eng = "brulee"
  )

  parsnip::set_dependency(
    "tabular_auto_int",
    eng = "brulee",
    pkg = "brulee",
    mode = "classification"
  )
  parsnip::set_dependency(
    "tabular_auto_int",
    eng = "brulee",
    pkg = "brulee",
    mode = "regression"
  )

  # ---------------------------------------------------------------------------
  # Model arguments

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "epochs",
    original = "epochs",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = TRUE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "num_embedding",
    original = "num_embedding",
    func = list(pkg = "dials", fun = "num_embedding", range = c(2L, 25L)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "hidden_units",
    original = "hidden_units",
    func = list(pkg = "dials", fun = "hidden_units", range = c(2L, 25L)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "hidden_activations",
    original = "hidden_activations",
    func = list(
      pkg = "dials",
      fun = "activation",
      values = c("relu", "elu", "tanh")
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "num_attn_feat",
    original = "num_attn_feat",
    func = list(pkg = "dials", fun = "num_attn_feat", range = c(0L, 25L)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "num_attn_heads",
    original = "num_attn_heads",
    func = list(pkg = "dials", fun = "num_attn_heads"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "num_attn_blocks",
    original = "num_attn_blocks",
    func = list(pkg = "dials", fun = "num_attn_blocks"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
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
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "dropout",
    original = "dropout",
    func = list(pkg = "dials", fun = "dropout", range = c(0.05, 0.50)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "dropout_attn",
    original = "dropout_attn",
    func = list(pkg = "dials", fun = "dropout_attn", range = c(0.05, 0.50)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "dropout_embedding",
    original = "dropout_embedding",
    func = list(
      pkg = "dials",
      fun = "dropout_embedding",
      range = c(0.05, 0.50)
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "penalty",
    original = "penalty",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "mixture",
    original = "mixture",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "learn_rate",
    original = "learn_rate",
    func = list(pkg = "dials", fun = "learn_rate", range = c(-2.0, -0.1)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "rate_schedule",
    original = "rate_schedule",
    func = list(pkg = "dials", fun = "rate_schedule"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "momentum",
    original = "momentum",
    func = list(pkg = "dials", fun = "momentum", range = c(0.50, 0.99)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "batch_size",
    original = "batch_size",
    func = list(pkg = "dials", fun = "batch_size", range = c(4, 10)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "class_weights",
    original = "class_weights",
    func = list(pkg = "dials", fun = "class_weights"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_auto_int",
    eng = "brulee",
    parsnip = "stop_iter",
    original = "stop_iter",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )

  # ---------------------------------------------------------------------------
  # Fit

  parsnip::set_fit(
    model = "tabular_auto_int",
    eng = "brulee",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "brulee", fun = "brulee_auto_int"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_auto_int",
    eng = "brulee",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "tabular_auto_int",
    eng = "brulee",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "brulee", fun = "brulee_auto_int"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_auto_int",
    eng = "brulee",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  # ---------------------------------------------------------------------------
  # Predictions

  parsnip::set_pred(
    model = "tabular_auto_int",
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

  parsnip::set_pred(
    model = "tabular_auto_int",
    eng = "brulee",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "class"
      )
    )
  )

  parsnip::set_pred(
    model = "tabular_auto_int",
    eng = "brulee",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "prob"
      )
    )
  )
}
