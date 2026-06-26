test_that("tabular_auto_int() creates a model spec", {
  spec <- tabular_auto_int()

  expect_s3_class(spec, "tabular_auto_int")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_auto_int() accepts mode and engine", {
  spec <- tabular_auto_int(mode = "classification")
  expect_equal(spec$mode, "classification")

  spec <- tabular_auto_int(mode = "regression")
  expect_equal(spec$mode, "regression")
})

test_that("tabular_auto_int() captures arguments", {
  spec <- tabular_auto_int(
    epochs = 50,
    num_embedding = 32,
    num_attn_heads = 4,
    dropout = 0.1
  )

  expect_equal(rlang::eval_tidy(spec$args$epochs), 50)
  expect_equal(rlang::eval_tidy(spec$args$num_embedding), 32)
  expect_equal(rlang::eval_tidy(spec$args$num_attn_heads), 4)
  expect_equal(rlang::eval_tidy(spec$args$dropout), 0.1)
})

test_that("tabular_auto_int() engine is registered", {
  reregister_model("tabular_auto_int")
  expect_no_error(make_tabular_auto_int())

  engines <- show_engines("tabular_auto_int")

  expect_true("brulee" %in% engines$engine)
  expect_true("classification" %in% engines$mode)
  expect_true("regression" %in% engines$mode)
})

test_that("update.tabular_auto_int() works", {
  spec <- tabular_auto_int(epochs = 50, dropout = 0.1)
  updated <- update(spec, epochs = 100)

  expect_equal(rlang::eval_tidy(updated$args$epochs), 100)
  expect_equal(rlang::eval_tidy(updated$args$dropout), 0.1)
})

test_that("update.tabular_auto_int() with fresh = TRUE replaces all args", {
  spec <- tabular_auto_int(epochs = 50, dropout = 0.1)
  updated <- update(spec, epochs = 100, fresh = TRUE)

  expect_equal(rlang::eval_tidy(updated$args$epochs), 100)
  expect_null(rlang::eval_tidy(updated$args$dropout))
})

test_that("check_args.tabular_auto_int() validates dropout range", {
  spec <- tabular_auto_int(mode = "regression", dropout = 1.5)
  expect_error(
    parsnip::check_args(spec),
    "dropout"
  )
})

test_that("check_args.tabular_auto_int() validates dropout_attn range", {
  spec <- tabular_auto_int(mode = "regression", dropout_attn = 2)
  expect_error(
    parsnip::check_args(spec),
    "dropout_attn"
  )
})

test_that("check_args.tabular_auto_int() validates dropout_embedding range", {
  spec <- tabular_auto_int(mode = "regression", dropout_embedding = -0.1)
  expect_error(
    parsnip::check_args(spec),
    "dropout_embedding"
  )
})

test_that("check_args.tabular_auto_int() validates penalty", {
  spec <- tabular_auto_int(mode = "regression", penalty = -1)
  expect_error(
    parsnip::check_args(spec),
    "penalty"
  )
})

test_that("check_args.tabular_auto_int() validates mixture range", {
  spec <- tabular_auto_int(mode = "regression", mixture = 2)
  expect_error(
    parsnip::check_args(spec),
    "mixture"
  )
})

test_that("check_args.tabular_auto_int() validates integer params", {
  spec <- tabular_auto_int(mode = "regression", epochs = -1)
  expect_error(
    parsnip::check_args(spec),
    "epochs"
  )

  spec <- tabular_auto_int(mode = "regression", num_attn_heads = 0)
  expect_error(
    parsnip::check_args(spec),
    "num_attn_heads"
  )
})

test_that("check_args.tabular_auto_int() rejects both penalty and dropout", {
  spec <- tabular_auto_int(mode = "regression", penalty = 0.1, dropout = 0.2)
  expect_error(
    parsnip::check_args(spec),
    "Both weight decay and dropout"
  )
})

test_that("check_args.tabular_auto_int() allows NULL args", {
  spec <- tabular_auto_int(mode = "regression")
  expect_no_error(parsnip::check_args(spec))
})

test_that("required_pkgs.tabular_auto_int() returns expected packages", {
  spec <- tabular_auto_int()
  pkgs <- required_pkgs(spec)

  expect_true("brulee" %in% pkgs)
  expect_true("tabular" %in% pkgs)
})
