#' Residual Neural Network for Tabular Data
#'
#' @description
#' `tabular_resnet()` ... This function can fit classification and
#' regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_resnet")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams parsnip::mlp
#' @inheritParams parsnip::linear_reg
#' @inheritParams parsnip::boost_tree
#' @param hidden_units An integer vector for the number of units in the hidden
#' model.
#' @param bottleneck_units The number of embeddings that are produced by batch
#' normalization.
#' @param residual_at An integer vector with the layer number should use a
#' residual connection (i.e., skip layer).
#' @param penalty A non-negative numeric value for the amount of weight
#'  decay.
#' @param mixture A number between zero and one (inclusive) giving the
#'  proportion of L1 regularization (i.e. lasso) in the model. `mixture = 1`
#'  is a pure lasso model while `mixture = 0` indicates ridge regression
#'  (a.k.a weight decay).
#' @param dropout A number between 0 (inclusive) and 1 denoting the proportion
#'  of model parameters randomly set to zero during model training.
#' @param epochs An integer for the number of training iterations.
#' @param activation A vector character strings denoting the type of relationship
#'  between the layers. The activation
#'  function between the hidden and output layers is automatically set to either
#'  "linear" or "softmax" depending on the type of outcome. Possible values
#'  depend on the engine being used.
#' @param rate_schedule A character string for the learning rate schedule.
#' @param momentum A number for the momentum parameter in optimizers that use it.
#' @param batch_size An integer for the number of training instances in each
#'  batch.
#' @param class_weights Numeric class weights for imbalanced data
#'  (classification only).
#'
#' @templateVar modeltype tabular_resnet
# @template spec-details
#'
# @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_resnet")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_resnet")
#'
#' tabular_resnet(mode = "classification", penalty = 0.01)
#' @export

tabular_resnet <-
  function(
    mode = "unknown",
    engine = "brulee",
    hidden_units = NULL,
    bottleneck_units = NULL,
    residual_at = NULL,
    penalty = NULL,
    mixture = NULL,
    dropout = NULL,
    epochs = NULL,
    activation = NULL,
    learn_rate = NULL,
    rate_schedule = NULL,
    momentum = NULL,
    batch_size = NULL,
    class_weights = NULL,
    stop_iter = NULL
  ) {
    args <- list(
      hidden_units = enquo(hidden_units),
      bottleneck_units = enquo(bottleneck_units),
      residual_at = enquo(residual_at),
      penalty = enquo(penalty),
      mixture = enquo(mixture),
      dropout = enquo(dropout),
      epochs = enquo(epochs),
      activation = enquo(activation),
      learn_rate = enquo(learn_rate),
      rate_schedule = enquo(rate_schedule),
      momentum = enquo(momentum),
      batch_size = enquo(batch_size),
      class_weights = enquo(class_weights),
      stop_iter = enquo(stop_iter)
    )

    parsnip::new_model_spec(
      "tabular_resnet",
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

#' Updating a model specification
#' @method update tabular_resnet
#' @rdname tabular_update
#' @inheritParams tabular_resnet
#' @param object A [model specification][model_spec].
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. Use **either** `parameters` **or** the main arguments
#'  directly when updating. If the main arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place or replaced wholesale.
#' @export
update.tabular_resnet <-
  function(
    object,
    parameters = NULL,
    hidden_units = NULL,
    bottleneck_units = NULL,
    residual_at = NULL,
    penalty = NULL,
    mixture = NULL,
    dropout = NULL,
    epochs = NULL,
    activation = NULL,
    learn_rate = NULL,
    rate_schedule = NULL,
    momentum = NULL,
    batch_size = NULL,
    class_weights = NULL,
    stop_iter = NULL,
    fresh = FALSE,
    ...
  ) {
    args <- list(
      hidden_units = enquo(hidden_units),
      bottleneck_units = enquo(bottleneck_units),
      residual_at = enquo(residual_at),
      penalty = enquo(penalty),
      mixture = enquo(mixture),
      dropout = enquo(dropout),
      epochs = enquo(epochs),
      activation = enquo(activation),
      learn_rate = enquo(learn_rate),
      rate_schedule = enquo(rate_schedule),
      momentum = enquo(momentum),
      batch_size = enquo(batch_size),
      class_weights = enquo(class_weights),
      stop_iter = enquo(stop_iter)
    )

    parsnip::update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "tabular_resnet",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.tabular_resnet <- function(object, call = rlang::caller_env()) {
  args <- lapply(object$args, rlang::eval_tidy)

  check_number_decimal(
    args$penalty,
    min = 0,
    allow_null = TRUE,
    call = call,
    arg = "penalty"
  )
  check_number_decimal(
    args$mixture,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "mixture"
  )
  check_number_decimal(
    args$dropout,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout"
  )
  check_number_whole(
    args$stop_iter,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "stop_iter"
  )

  if (
    is.numeric(args$penalty) &&
      is.numeric(args$dropout) &&
      args$dropout > 0 &&
      args$penalty > 0
  ) {
    cli::cli_abort(
      "Both weight decay and dropout should not be specified.",
      call = call
    )
  }

  invisible(object)
}

#' @export
required_pkgs.tabular_resnet <- function(x, infra = TRUE, ...) {
  c("brulee", "tabular")
}

## -----------------------------------------------------------------------------

#' Model predictions across many sub-models
#' @importFrom purrr map
#' @importFrom dplyr arrange select
#' @rdname multi_predict
#' @inheritParams parsnip::multi_predict
#' @param epochs An integer vector for the number of training epochs.
#' @export
multi_predict._torch_resnet <-
  function(object, new_data, type = NULL, epochs = NULL, ...) {
    load_libs(object, quiet = TRUE, attach = TRUE)

    if (is.null(epochs)) {
      epochs <- length(object$fit$models)
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
        ~ predict(object, new_data, type, epochs = .x) |>
          dplyr::mutate(epochs = .x)
      ) |>
      purrr::map(\(x) x |> dplyr::mutate(.row = seq_len(nrow(new_data)))) |>
      purrr::list_rbind() |>
      dplyr::arrange(.row, epochs)
    res <- split(dplyr::select(res, -.row), res$.row)
    names(res) <- NULL
    tibble::tibble(.pred = res)
  }


reformat_torch_num <- function(results, object) {
  if (isTRUE(ncol(results) > 1)) {
    nms <- colnames(results)
    results <- tibble::as_tibble(results, .name_repair = "minimal")
    if (length(nms) == 0 && length(object$preproc$y_var) == ncol(results)) {
      names(results) <- object$preproc$y_var
    }
  } else {
    results <- unname(results[[1]])
  }
  results
}

# ------------------------------------------------------------------------------

make_tabular_resnet <- function() {
  parsnip::set_new_model("tabular_resnet")
  parsnip::set_model_mode("tabular_resnet", mode = "classification")
  parsnip::set_model_mode("tabular_resnet", mode = "regression")

  parsnip::set_model_engine(
    "tabular_resnet",
    mode = "classification",
    eng = "brulee"
  )
  parsnip::set_model_engine(
    "tabular_resnet",
    mode = "regression",
    eng = "brulee"
  )
  parsnip::set_dependency(
    "tabular_resnet",
    eng = "brulee",
    pkg = "brulee",
    mode = "classification"
  )
  parsnip::set_dependency(
    "tabular_resnet",
    eng = "brulee",
    pkg = "brulee",
    mode = "regression"
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "hidden_units",
    original = "hidden_units",
    func = list(pkg = "dials", fun = "hidden_units"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "bottleneck_units",
    original = "bottleneck_units",
    func = list(pkg = "dials", fun = "bottleneck_units"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "residual_at",
    original = "residual_at",
    func = list(pkg = "dials", fun = "resid_at"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "penalty",
    original = "penalty",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "mixture",
    original = "mixture",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "epochs",
    original = "epochs",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "dropout",
    original = "dropout",
    func = list(pkg = "dials", fun = "dropout"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "learn_rate",
    original = "learn_rate",
    func = list(pkg = "dials", fun = "learn_rate", range = c(-2.5, -0.5)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "activation",
    original = "activation",
    func = list(
      pkg = "dials",
      fun = "activation",
      values = c('relu', 'elu', 'tanh')
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "rate_schedule",
    original = "rate_schedule",
    func = list(
      pkg = "dials",
      fun = "rate_schedule"
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "momentum",
    original = "momentum",
    func = list(
      pkg = "dials",
      fun = "momentum",
      range = c(0.50, 0.99)
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "batch_size",
    original = "batch_size",
    func = list(
      pkg = "dials",
      fun = "batch_size",
      range = c(4, 7)
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "class_weights",
    original = "class_weights",
    func = list(pkg = "dials", fun = "class_weights"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tabular_resnet",
    eng = "brulee",
    parsnip = "stop_iter",
    original = "stop_iter",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "tabular_resnet",
    eng = "brulee",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "brulee", fun = "brulee_resnet"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_resnet",
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
    model = "tabular_resnet",
    eng = "brulee",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "brulee", fun = "brulee_resnet"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tabular_resnet",
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
    model = "tabular_resnet",
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
    model = "tabular_resnet",
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
    model = "tabular_resnet",
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
