#' @method required_pkgs tabular_saint
#' @export
required_pkgs.tabular_saint <- function(x, infra = TRUE, ...) {
  c("brulee", "tabby")
}

# ------------------------------------------------------------------------------

make_tabular_saint <- function() {
  parsnip::set_model_engine(
    "tabular_saint",
    mode = "classification",
    eng = "brulee"
  )

  parsnip::set_model_engine(
    "tabular_saint",
    mode = "regression",
    eng = "brulee"
  )

  parsnip::set_dependency(
    "tabular_saint",
    eng = "brulee",
    pkg = "brulee",
    mode = "classification"
  )
  parsnip::set_dependency(
    "tabular_saint",
    eng = "brulee",
    pkg = "brulee",
    mode = "regression"
  )
  parsnip::set_dependency(
    "tabular_saint",
    eng = "brulee",
    pkg = "tabby",
    mode = "classification"
  )
  parsnip::set_dependency(
    "tabular_saint",
    eng = "brulee",
    pkg = "tabby",
    mode = "regression"
  )

  # ---------------------------------------------------------------------------
  # Model arguments

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "epochs",
    original = "epochs",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = TRUE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "num_embedding",
    original = "num_embedding",
    func = list(pkg = "dials", fun = "num_embedding", range = c(2L, 25L)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "attention_type",
    original = "attention_type",
    func = list(
      pkg = "dials",
      fun = "attention_type",
      values = c("column", "row", "both")
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "num_attn_heads",
    original = "num_attn_heads",
    func = list(pkg = "dials", fun = "num_attn_heads"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "num_attn_blocks",
    original = "num_attn_blocks",
    func = list(pkg = "dials", fun = "num_attn_blocks"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "dropout_attn",
    original = "dropout_attn",
    func = list(pkg = "dials", fun = "dropout_attn", range = c(0.05, 0.50)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "dropout_hidden",
    original = "dropout_hidden",
    func = list(pkg = "dials", fun = "dropout_hidden", range = c(0.05, 0.50)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "dropout_last",
    original = "dropout_last",
    func = list(pkg = "dials", fun = "dropout_last", range = c(0.05, 0.50)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "hidden_units",
    original = "hidden_units",
    func = list(pkg = "dials", fun = "hidden_units", range = c(2L, 25L)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
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
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "target_token",
    original = "target_token",
    func = list(pkg = "dials", fun = "target_token"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "penalty",
    original = "penalty",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "mixture",
    original = "mixture",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "learn_rate",
    original = "learn_rate",
    func = list(pkg = "dials", fun = "learn_rate", range = c(-2.5, -0.5)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "rate_schedule",
    original = "rate_schedule",
    func = list(pkg = "dials", fun = "rate_schedule"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "momentum",
    original = "momentum",
    func = list(pkg = "dials", fun = "momentum", range = c(0.50, 0.99)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "batch_size",
    original = "batch_size",
    func = list(pkg = "dials", fun = "batch_size", range = c(4, 10)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "class_weights",
    original = "class_weights",
    func = list(pkg = "dials", fun = "class_weights"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_saint",
    eng = "brulee",
    parsnip = "stop_iter",
    original = "stop_iter",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )

  # ---------------------------------------------------------------------------
  # Fit

  parsnip::set_fit(
    model = "tabular_saint",
    eng = "brulee",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "brulee", fun = "brulee_saint"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_saint",
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
    model = "tabular_saint",
    eng = "brulee",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "brulee", fun = "brulee_saint"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_saint",
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
    model = "tabular_saint",
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
    model = "tabular_saint",
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
    model = "tabular_saint",
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
