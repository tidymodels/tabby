#' @export
required_pkgs.tabular_icl <- function(x, infra = TRUE, ...) {
  c("brulee", "tabby")
}

# ------------------------------------------------------------------------------

make_tabular_icl <- function() {
  parsnip::set_model_engine(
    "tabular_icl",
    mode = "classification",
    eng = "brulee"
  )
  parsnip::set_dependency("tabular_icl", eng = "brulee", pkg = "brulee")
  parsnip::set_dependency("tabular_icl", eng = "brulee", pkg = "tabby")

  parsnip::set_fit(
    model = "tabular_icl",
    eng = "brulee",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "brulee", fun = "brulee_tab_icl"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_icl",
    eng = "brulee",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "tabular_icl",
    eng = "brulee",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        type = "class",
        object = quote(object$fit),
        new_data = quote(new_data)
      )
    )
  )

  parsnip::set_pred(
    model = "tabular_icl",
    eng = "brulee",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        type = "prob",
        object = quote(object$fit),
        new_data = quote(new_data)
      )
    )
  )

  parsnip::set_model_arg(
    model = "tabular_icl",
    eng = "brulee",
    parsnip = "num_estimators",
    original = "num_estimators",
    func = list(pkg = "dials", fun = "num_estimators"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_icl",
    eng = "brulee",
    parsnip = "softmax_temperature",
    original = "softmax_temperature",
    func = list(pkg = "dials", fun = "softmax_temperature"),
    has_submodel = FALSE
  )

  # ----------------------------------------------------------------------------

  parsnip::set_model_engine("tabular_icl", mode = "regression", eng = "brulee")
  parsnip::set_dependency("tabular_icl", eng = "brulee", pkg = "brulee")
  parsnip::set_dependency("tabular_icl", eng = "brulee", pkg = "tabby")

  parsnip::set_fit(
    model = "tabular_icl",
    eng = "brulee",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "brulee", fun = "brulee_tab_icl"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_icl",
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
    model = "tabular_icl",
    eng = "brulee",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data)
      )
    )
  )
}
