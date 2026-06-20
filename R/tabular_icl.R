#' TabICL: prior data fitted networks
#'
#' @description
#' This function uses a pre-trained deep learning network that emulates Bayesian
#' inference. The model was trained on a large number of simulated data sets
#' and an attention mechanism is use to make relevant predictions for specific
#' (i.e., real) data sets.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_icl")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param num_estimators An integer for the ensemble size. Default is `8L`.
#'
#' @param softmax_temperature An adjustment factor that is a divisor in the
#' exponents of the softmax function (see [brulee::brulee_tab_icl()]). Defaults
#' to 0.9.
#'
#' @param mode A single character value for the type of model.
#'  The possible values for this model are "classification" and "regression".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"brulee"`.
#'
#' @templateVar modeltype tabular_icl
# @template spec-details
#'
#' @details This function fits classification and regression models.
#'
# @template spec-references
#'
#' @references
#'
#' [https://github.com/soda-inria/tabicl](https://github.com/soda-inria/tabicl)
#'
#' Qu, J., HolzmÃžller, D., Varoquaux, G., & Morvan, M. L. (2025). Tabicl: A
#' tabular foundation model for in-context learning on large data. arXiv
#' preprint arXiv:2502.05564.
#'
#' Qu, J., Holzmüller, D., Varoquaux, G., & Morvan, M. L. (2026). TabICLv2: A
#' better, faster, scalable, and open tabular foundation model. arXiv preprint
#' arXiv:2602.11139.
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_icl")} [brulee::brulee_tab_icl()]
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_icl")
#'
#' tabular_icl()
#' @export
tabular_icl <-
  function(
    mode = "unknown",
    engine = "brulee",
    num_estimators = NULL,
    softmax_temperature = NULL
  ) {
    args <- list(
      num_estimators = enquo(num_estimators),
      softmax_temperature = enquo(softmax_temperature)
    )

    parsnip::new_model_spec(
      "tabular_icl",
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

#' @method update tabular_icl
#' @rdname tabular_update
#' @inheritParams tabular_icl
#' @export
update.tabular_icl <-
  function(
    object,
    parameters = NULL,
    num_estimators = NULL,
    softmax_temperature = NULL,
    fresh = FALSE,
    ...
  ) {
    args <- list(
      num_estimators = enquo(num_estimators),
      softmax_temperature = enquo(softmax_temperature)
    )

    parsnip::update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "tabular_icl",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.tabular_icl <- function(object, call = rlang::caller_env()) {
  args <- lapply(object$args, rlang::eval_tidy)

  check_number_decimal(
    args$softmax_temperature,
    min = 0,
    allow_null = TRUE,
    call = call,
    arg = "softmax_temperature"
  )
  check_number_whole(
    args$num_estimators,
    min = 0,
    allow_null = TRUE,
    call = call,
    arg = "num_estimators"
  )
  invisible(object)
}


#' @export
required_pkgs.tabular_icl <- function(x, infra = TRUE, ...) {
  c("brulee", "tabular")
}

# ------------------------------------------------------------------------------

make_tabular_icl <- function() {
  parsnip::set_new_model("tabular_icl")
  parsnip::set_model_mode("tabular_icl", mode = "classification")
  parsnip::set_model_mode("tabular_icl", mode = "regression")

  parsnip::set_model_engine(
    "tabular_icl",
    mode = "classification",
    eng = "brulee"
  )
  parsnip::set_dependency("tabular_icl", eng = "brulee", pkg = "brulee")

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

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("tabular_icl", mode = "regression", eng = "brulee")
  parsnip::set_dependency("tabular_icl", eng = "brulee", pkg = "brulee")

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
