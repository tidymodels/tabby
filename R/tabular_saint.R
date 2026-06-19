#' SAINT: Self-Attention and Inter-sample Attention Transformer
#'
#' @description
#' `tabular_saint()` uses self-attention and inter-sample attention mechanisms
#' to learn feature interactions for tabular data. This function can fit
#' classification and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_saint")}
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
#' @param attention_type A character string for the type of attention to use.
#'  Options are `"column"` (SAINT-s), `"row"` (SAINT-i), or `"both"` (full
#'  SAINT).
#' @param num_attn_heads An integer for the number of attention heads in the
#'  multi-head attention mechanism.
#' @param num_attn_blocks An integer for the number of sequential attention
#'  blocks.
#' @param dropout_attn A number between 0 (inclusive) and 1 denoting the
#'  proportion of attention weights set to zero during model training.
#' @param dropout_hidden A number between 0 (inclusive) and 1 denoting the
#'  proportion of values in the feed-forward layers set to zero during training.
#' @param dropout_last A number between 0 (inclusive) and 1 denoting the
#'  proportion of values set to zero between the last hidden layer and the
#'  output head.
#' @param rate_schedule A character string for the learning rate schedule.
#' @param momentum A number for the momentum parameter in optimizers that use it.
#' @param batch_size An integer for the number of training instances in each
#'  batch.
#' @param class_weights Numeric class weights for imbalanced data
#'  (classification only).
#'
#' @templateVar modeltype tabular_saint
# @template spec-details
#'
# @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_saint")}
#'
#' @references
#' Somepalli, G., Goldblum, M., Schwarzschild, A., Bruss, C. B., & Goldstein,
#' T. (2021). SAINT: Improved Neural Networks for Tabular Data via Row
#' Attention and Contrastive Pre-Training. arXiv:2106.01342.
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_saint")
#'
#' tabular_saint(mode = "classification", num_attn_blocks = 4)
#' @export

tabular_saint <-
  function(
    mode = "unknown",
    engine = "brulee",
    epochs = NULL,
    num_embedding = NULL,
    attention_type = NULL,
    num_attn_heads = NULL,
    num_attn_blocks = NULL,
    dropout_attn = NULL,
    dropout_hidden = NULL,
    dropout_last = NULL,
    hidden_units = NULL,
    hidden_activations = NULL,
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
      attention_type = enquo(attention_type),
      num_attn_heads = enquo(num_attn_heads),
      num_attn_blocks = enquo(num_attn_blocks),
      dropout_attn = enquo(dropout_attn),
      dropout_hidden = enquo(dropout_hidden),
      dropout_last = enquo(dropout_last),
      hidden_units = enquo(hidden_units),
      hidden_activations = enquo(hidden_activations),
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
      "tabular_saint",
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

#' @method update tabular_saint
#' @rdname tabular_update
#' @inheritParams tabular_saint
#' @export
update.tabular_saint <-
  function(
    object,
    parameters = NULL,
    epochs = NULL,
    num_embedding = NULL,
    attention_type = NULL,
    num_attn_heads = NULL,
    num_attn_blocks = NULL,
    dropout_attn = NULL,
    dropout_hidden = NULL,
    dropout_last = NULL,
    hidden_units = NULL,
    hidden_activations = NULL,
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
      attention_type = enquo(attention_type),
      num_attn_heads = enquo(num_attn_heads),
      num_attn_blocks = enquo(num_attn_blocks),
      dropout_attn = enquo(dropout_attn),
      dropout_hidden = enquo(dropout_hidden),
      dropout_last = enquo(dropout_last),
      hidden_units = enquo(hidden_units),
      hidden_activations = enquo(hidden_activations),
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
      cls = "tabular_saint",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @method check_args tabular_saint
#' @export
check_args.tabular_saint <- function(object, call = rlang::caller_env()) {
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
    args$dropout_attn,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout_attn"
  )
  check_number_decimal(
    args$dropout_hidden,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout_hidden"
  )
  check_number_decimal(
    args$dropout_last,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout_last"
  )
  check_number_whole(
    args$epochs,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "epochs"
  )
  check_number_whole(
    args$num_embedding,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "num_embedding"
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
    args$stop_iter,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "stop_iter"
  )

  if (
    !is.null(args$attention_type) &&
      !args$attention_type %in% c("column", "row", "both")
  ) {
    cli::cli_abort(
      "{.arg attention_type} must be one of {.val column}, {.val row}, or {.val both}.",
      call = call
    )
  }

  if (
    is.numeric(args$penalty) &&
      is.numeric(args$dropout_attn) &&
      args$dropout_attn > 0 &&
      args$penalty > 0
  ) {
    cli::cli_abort(
      "Both weight decay and dropout should not be specified.",
      call = call
    )
  }

  invisible(object)
}

#' @method required_pkgs tabular_saint
#' @export
required_pkgs.tabular_saint <- function(x, infra = TRUE, ...) {
  c("brulee", "tabular")
}

## -----------------------------------------------------------------------------

#' @importFrom purrr map
#' @importFrom dplyr arrange select
#' @rdname multi_predict
#' @param epochs An integer vector for the number of training epochs.
#' @export
multi_predict._brulee_saint <-
  function(object, new_data, type = NULL, epochs = NULL, ...) {
    load_libs(object, quiet = TRUE, attach = TRUE)

    if (is.null(epochs)) {
      epochs <- length(object$fit$estimates)
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
        ~ predict(object, new_data, type, epoch = .x) |>
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

make_tabular_saint <- function() {
  parsnip::set_new_model("tabular_saint")
  parsnip::set_model_mode("tabular_saint", mode = "classification")
  parsnip::set_model_mode("tabular_saint", mode = "regression")

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
