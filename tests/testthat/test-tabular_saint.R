test_that("tabular_saint() creates a model spec", {
  spec <- tabular_saint()

  expect_s3_class(spec, "tabular_saint")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_saint() accepts mode and engine", {
  spec <- tabular_saint(mode = "classification")
  expect_equal(spec$mode, "classification")

  spec <- tabular_saint(mode = "regression")
  expect_equal(spec$mode, "regression")
})

test_that("tabular_saint() captures arguments", {
  spec <- tabular_saint(
    epochs = 50,
    num_embedding = 32,
    num_attn_heads = 4,
    dropout_attn = 0.1
  )

  expect_equal(rlang::eval_tidy(spec$args$epochs), 50)
  expect_equal(rlang::eval_tidy(spec$args$num_embedding), 32)
  expect_equal(rlang::eval_tidy(spec$args$num_attn_heads), 4)
  expect_equal(rlang::eval_tidy(spec$args$dropout_attn), 0.1)
})

test_that("tabular_saint() engine is registered", {
  engines <- show_engines("tabular_saint")

  expect_true("brulee" %in% engines$engine)
  expect_true("classification" %in% engines$mode)
  expect_true("regression" %in% engines$mode)
})

test_that("update.tabular_saint() works", {
  spec <- tabular_saint(epochs = 50, dropout_attn = 0.1)
  updated <- update(spec, epochs = 100)

  expect_equal(rlang::eval_tidy(updated$args$epochs), 100)
  expect_equal(rlang::eval_tidy(updated$args$dropout_attn), 0.1)
})

test_that("update.tabular_saint() with fresh = TRUE replaces all args", {
  spec <- tabular_saint(epochs = 50, dropout_attn = 0.1)
  updated <- update(spec, epochs = 100, fresh = TRUE)

  expect_equal(rlang::eval_tidy(updated$args$epochs), 100)
  expect_null(rlang::eval_tidy(updated$args$dropout_attn))
})

test_that("check_args.tabular_saint() validates dropout_attn range", {
  spec <- tabular_saint(mode = "regression", dropout_attn = 1.5)
  expect_error(
    parsnip::check_args(spec),
    "dropout_attn"
  )
})

test_that("check_args.tabular_saint() validates dropout_hidden range", {
  spec <- tabular_saint(mode = "regression", dropout_hidden = 2)
  expect_error(
    parsnip::check_args(spec),
    "dropout_hidden"
  )
})

test_that("check_args.tabular_saint() validates dropout_last range", {
  spec <- tabular_saint(mode = "regression", dropout_last = -0.1)
  expect_error(
    parsnip::check_args(spec),
    "dropout_last"
  )
})

test_that("check_args.tabular_saint() validates penalty", {
  spec <- tabular_saint(mode = "regression", penalty = -1)
  expect_error(
    parsnip::check_args(spec),
    "penalty"
  )
})

test_that("check_args.tabular_saint() validates mixture range", {
  spec <- tabular_saint(mode = "regression", mixture = 2)
  expect_error(
    parsnip::check_args(spec),
    "mixture"
  )
})

test_that("check_args.tabular_saint() validates integer params", {
  spec <- tabular_saint(mode = "regression", epochs = -1)
  expect_error(
    parsnip::check_args(spec),
    "epochs"
  )

  spec <- tabular_saint(mode = "regression", num_attn_heads = 0)
  expect_error(
    parsnip::check_args(spec),
    "num_attn_heads"
  )
})

test_that("check_args.tabular_saint() validates attention_type", {
  spec <- tabular_saint(mode = "regression", attention_type = "bad_value")
  expect_error(
    parsnip::check_args(spec),
    "attention_type"
  )
})

test_that("check_args.tabular_saint() rejects both penalty and dropout_attn", {
  spec <- tabular_saint(mode = "regression", penalty = 0.1, dropout_attn = 0.2)
  expect_error(
    parsnip::check_args(spec),
    "Both weight decay and dropout"
  )
})

test_that("check_args.tabular_saint() allows NULL args", {
  spec <- tabular_saint(mode = "regression")
  expect_no_error(parsnip::check_args(spec))
})

test_that("required_pkgs.tabular_saint() returns expected packages", {
  spec <- tabular_saint()
  pkgs <- required_pkgs(spec)

  expect_true("brulee" %in% pkgs)
  expect_true("tabular" %in% pkgs)
})
