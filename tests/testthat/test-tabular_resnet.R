test_that("tabular_resnet() creates a model spec", {
  spec <- tabular_resnet()

  expect_s3_class(spec, "tabular_resnet")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_resnet() accepts mode and engine", {
  spec <- tabular_resnet(mode = "classification")
  expect_equal(spec$mode, "classification")

  spec <- tabular_resnet(mode = "regression")
  expect_equal(spec$mode, "regression")
})

test_that("tabular_resnet() captures arguments", {
  spec <- tabular_resnet(
    hidden_units = 10L,
    residual_at = 2L,
    penalty = 0.01,
    mixture = 0.5,
    rate_schedule = "cyclic",
    momentum = 0.9,
    batch_size = 32L,
    class_weights = 2,
    stop_iter = 5L
  )

  expect_equal(rlang::eval_tidy(spec$args$hidden_units), 10L)
  expect_equal(rlang::eval_tidy(spec$args$residual_at), 2L)
  expect_equal(rlang::eval_tidy(spec$args$penalty), 0.01)
  expect_equal(rlang::eval_tidy(spec$args$mixture), 0.5)
  expect_equal(rlang::eval_tidy(spec$args$rate_schedule), "cyclic")
  expect_equal(rlang::eval_tidy(spec$args$momentum), 0.9)
  expect_equal(rlang::eval_tidy(spec$args$batch_size), 32L)
  expect_equal(rlang::eval_tidy(spec$args$class_weights), 2)
  expect_equal(rlang::eval_tidy(spec$args$stop_iter), 5L)
})

test_that("tabular_resnet() engine is registered", {
  engines <- show_engines("tabular_resnet")

  expect_true("brulee" %in% engines$engine)
  expect_true("classification" %in% engines$mode)
  expect_true("regression" %in% engines$mode)
})

test_that("update.tabular_resnet() works", {
  spec <- tabular_resnet(hidden_units = 10L, mixture = 0.2)
  updated <- update(spec, hidden_units = 20L)

  expect_equal(rlang::eval_tidy(updated$args$hidden_units), 20L)
  expect_equal(rlang::eval_tidy(updated$args$mixture), 0.2)
})

test_that("update.tabular_resnet() with fresh = TRUE replaces all args", {
  spec <- tabular_resnet(hidden_units = 10L, mixture = 0.2)
  updated <- update(spec, hidden_units = 20L, fresh = TRUE)

  expect_equal(rlang::eval_tidy(updated$args$hidden_units), 20L)
  expect_null(rlang::eval_tidy(updated$args$mixture))
})

test_that("check_args.tabular_resnet() validates penalty", {
  spec <- tabular_resnet(mode = "regression", penalty = -1)
  expect_error(
    parsnip::check_args(spec),
    "penalty"
  )
})

test_that("check_args.tabular_resnet() validates mixture range", {
  spec <- tabular_resnet(mode = "regression", mixture = 2)
  expect_error(
    parsnip::check_args(spec),
    "mixture"
  )
})

test_that("check_args.tabular_resnet() validates dropout range", {
  spec <- tabular_resnet(mode = "regression", dropout = 1.5)
  expect_error(
    parsnip::check_args(spec),
    "dropout"
  )
})

test_that("check_args.tabular_resnet() validates stop_iter", {
  spec <- tabular_resnet(mode = "regression", stop_iter = 0)
  expect_error(
    parsnip::check_args(spec),
    "stop_iter"
  )
})

test_that("check_args.tabular_resnet() rejects both penalty and dropout", {
  spec <- tabular_resnet(mode = "regression", penalty = 0.1, dropout = 0.2)
  expect_error(
    parsnip::check_args(spec),
    "Both weight decay and dropout"
  )
})

test_that("check_args.tabular_resnet() allows NULL args", {
  spec <- tabular_resnet(mode = "regression")
  expect_no_error(parsnip::check_args(spec))
})

test_that("required_pkgs.tabular_resnet() returns expected packages", {
  spec <- tabular_resnet()
  pkgs <- required_pkgs(spec)

  expect_true("brulee" %in% pkgs)
  expect_true("tabular" %in% pkgs)
})

# ------------------------------------------------------------------------------
# Integration tests (require the brulee engine + torch)

test_that("tabular_resnet() fits and predicts (regression)", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(1)
  spec <- tabular_resnet(hidden_units = 3L, epochs = 5L) |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("regression")

  fit <- parsnip::fit(spec, mpg ~ ., data = mtcars)

  expect_s3_class(fit, "model_fit")

  preds <- predict(fit, mtcars[1:5, ])
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_equal(nrow(preds), 5)
  expect_true(is.numeric(preds$.pred))
})

test_that("tabular_resnet() fits and predicts (classification)", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(1)
  spec <- tabular_resnet(hidden_units = 3L, epochs = 5L) |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("classification")

  fit <- parsnip::fit(spec, Species ~ ., data = iris)

  expect_s3_class(fit, "model_fit")

  cls <- predict(fit, iris[1:5, ])
  expect_named(cls, ".pred_class")
  expect_s3_class(cls$.pred_class, "factor")
  expect_equal(nrow(cls), 5)

  prob <- predict(fit, iris[1:5, ], type = "prob")
  expect_named(
    prob,
    c(".pred_setosa", ".pred_versicolor", ".pred_virginica")
  )
  expect_equal(nrow(prob), 5)
})
