#' @export
required_pkgs.tabular_pfn <- function(x, infra = TRUE, ...) {
  c("tabpfn", "tabby")
}

# ------------------------------------------------------------------------------

make_tabular_pfn <- function() {
  parsnip::set_model_engine(
    "tabular_pfn",
    mode = "classification",
    eng = "tabpfn"
  )
  parsnip::set_dependency("tabular_pfn", eng = "tabpfn", pkg = "tabpfn")
  parsnip::set_dependency("tabular_pfn", eng = "tabpfn", pkg = "tabby")

  parsnip::set_fit(
    model = "tabular_pfn",
    eng = "tabpfn",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "tabpfn", fun = "tab_pfn"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_pfn",
    eng = "tabpfn",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "tabular_pfn",
    eng = "tabpfn",
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
    model = "tabular_pfn",
    eng = "tabpfn",
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
    model = "tabular_pfn",
    eng = "tabpfn",
    parsnip = "num_estimators",
    original = "num_estimators",
    func = list(pkg = "dials", fun = "num_estimators"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_pfn",
    eng = "tabpfn",
    parsnip = "softmax_temperature",
    original = "softmax_temperature",
    func = list(pkg = "dials", fun = "softmax_temperature"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_pfn",
    eng = "tabpfn",
    parsnip = "balance_probabilities",
    original = "balance_probabilities",
    func = list(pkg = "dials", fun = "balance_probabilities"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_pfn",
    eng = "tabpfn",
    parsnip = "average_before_softmax",
    original = "average_before_softmax",
    func = list(pkg = "dials", fun = "average_before_softmax"),
    has_submodel = FALSE
  )

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("tabular_pfn", mode = "regression", eng = "tabpfn")
  parsnip::set_dependency("tabular_pfn", eng = "tabpfn", pkg = "tabpfn")
  parsnip::set_dependency("tabular_pfn", eng = "tabpfn", pkg = "tabby")

  parsnip::set_fit(
    model = "tabular_pfn",
    eng = "tabpfn",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "tabpfn", fun = "tab_pfn"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_pfn",
    eng = "tabpfn",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "tabular_pfn",
    eng = "tabpfn",
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
