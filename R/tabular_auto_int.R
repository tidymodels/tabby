#' AutoInt: Automatic Feature Interaction Learning
#'
#' @description
#' `tabular_auto_int()` uses an attention mechanism to automatically learn
#' feature interactions for tabular data. This function can fit classification
#' and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_auto_int")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams parsnip::mlp
#' @inheritParams parsnip::linear_reg
#' @inheritParams parsnip::boost_tree
#' @param hidden_units An integer vector for the number of units in the hidden
#'  layers after the attention mechanism.
#' @param hidden_activations A character vector denoting the activation functions
#'  for the hidden layers.
#' @param num_embedding An integer for the dimensionality of the embedding space
#'  for features.
#' @param num_attn_feat An integer for the number of attention features.
#' @param num_attn_heads An integer for the number of attention heads in the
#'  multi-head attention mechanism.
#' @param num_attn_blocks An integer for the number of sequential attention
#'  blocks.
#' @param dropout_attn A number between 0 (inclusive) and 1 denoting the
#'  proportion of attention weights set to zero during model training.
#' @param dropout_embedding A number between 0 (inclusive) and 1 denoting the
#'  proportion of embedding values set to zero during model training.
#' @param rate_schedule A character string for the learning rate schedule.
#' @param momentum A number for the momentum parameter in optimizers that use it.
#' @param batch_size An integer for the number of training instances in each
#'  batch.
#' @param class_weights Numeric class weights for imbalanced data
#'  (classification only).
#'
#' @templateVar modeltype tabular_auto_int
# @template spec-details
#'
# @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_auto_int")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_auto_int")
#'
#' tabular_auto_int(mode = "classification", num_attn_blocks = 4)
#' @export

tabular_auto_int <-
  function(
    mode = "unknown",
    engine = "brulee",
    epochs = NULL,
    num_embedding = NULL,
    hidden_units = NULL,
    hidden_activations = NULL,
    num_attn_feat = NULL,
    num_attn_heads = NULL,
    num_attn_blocks = NULL,
    activation = NULL,
    dropout = NULL,
    dropout_attn = NULL,
    dropout_embedding = NULL,
    penalty = NULL,
    mixture = NULL,
    learn_rate = NULL,
    rate_schedule = NULL,
    momentum = NULL,
    batch_size = NULL,
    class_weights = NULL,
    stop_iter = NULL
  ) {
    args <- list(
      epochs = enquo(epochs),
      num_embedding = enquo(num_embedding),
      hidden_units = enquo(hidden_units),
      hidden_activations = enquo(hidden_activations),
      num_attn_feat = enquo(num_attn_feat),
      num_attn_heads = enquo(num_attn_heads),
      num_attn_blocks = enquo(num_attn_blocks),
      activation = enquo(activation),
      dropout = enquo(dropout),
      dropout_attn = enquo(dropout_attn),
      dropout_embedding = enquo(dropout_embedding),
      penalty = enquo(penalty),
      mixture = enquo(mixture),
      learn_rate = enquo(learn_rate),
      rate_schedule = enquo(rate_schedule),
      momentum = enquo(momentum),
      batch_size = enquo(batch_size),
      class_weights = enquo(class_weights),
      stop_iter = enquo(stop_iter)
    )

    parsnip::new_model_spec(
      "tabular_auto_int",
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

#' @method update tabular_auto_int
#' @rdname tabular_update
#' @inheritParams tabular_auto_int
#' @export
update.tabular_auto_int <-
  function(
    object,
    parameters = NULL,
    epochs = NULL,
    num_embedding = NULL,
    hidden_units = NULL,
    hidden_activations = NULL,
    num_attn_feat = NULL,
    num_attn_heads = NULL,
    num_attn_blocks = NULL,
    activation = NULL,
    dropout = NULL,
    dropout_attn = NULL,
    dropout_embedding = NULL,
    penalty = NULL,
    mixture = NULL,
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
      epochs = enquo(epochs),
      num_embedding = enquo(num_embedding),
      hidden_units = enquo(hidden_units),
      hidden_activations = enquo(hidden_activations),
      num_attn_feat = enquo(num_attn_feat),
      num_attn_heads = enquo(num_attn_heads),
      num_attn_blocks = enquo(num_attn_blocks),
      activation = enquo(activation),
      dropout = enquo(dropout),
      dropout_attn = enquo(dropout_attn),
      dropout_embedding = enquo(dropout_embedding),
      penalty = enquo(penalty),
      mixture = enquo(mixture),
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
      cls = "tabular_auto_int",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.tabular_auto_int <- function(object, call = rlang::caller_env()) {
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
  check_number_decimal(
    args$dropout_attn,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout_attn"
  )
  check_number_decimal(
    args$dropout_embedding,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout_embedding"
  )
  check_number_whole(
    args$epochs,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "epochs"
  )
  check_number_whole(
    args$num_attn_feat,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "num_attn_feat"
  )
  check_number_whole(
    args$num_attn_heads,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "num_attn_heads"
  )
  check_number_whole(
    args$num_attn_blocks,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "num_attn_blocks"
  )
  check_number_whole(
    args$num_embedding,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "num_embedding"
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
  parsnip::set_new_model("tabular_auto_int")
  parsnip::set_model_mode("tabular_auto_int", mode = "classification")
  parsnip::set_model_mode("tabular_auto_int", mode = "regression")

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
