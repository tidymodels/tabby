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

  engines <- parsnip::show_engines("tabular_auto_int")
  expect_true("brulee" %in% engines$engine)
  expect_setequal(engines$mode, c("classification", "regression"))
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
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("check_args.tabular_auto_int() validates dropout_attn range", {
  spec <- tabular_auto_int(mode = "regression", dropout_attn = 2)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("check_args.tabular_auto_int() validates dropout_embedding range", {
  spec <- tabular_auto_int(mode = "regression", dropout_embedding = -0.1)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("check_args.tabular_auto_int() validates penalty", {
  spec <- tabular_auto_int(mode = "regression", penalty = -1)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("check_args.tabular_auto_int() validates mixture range", {
  spec <- tabular_auto_int(mode = "regression", mixture = 2)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("check_args.tabular_auto_int() validates integer params", {
  spec <- tabular_auto_int(mode = "regression", epochs = -1)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))

  spec <- tabular_auto_int(mode = "regression", num_attn_heads = 0)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("check_args.tabular_auto_int() rejects both penalty and dropout", {
  spec <- tabular_auto_int(mode = "regression", penalty = 0.1, dropout = 0.2)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
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

# ------------------------------------------------------------------------------
# Integration tests (require the brulee engine + torch)

test_that("multi_predict._brulee_auto_int() returns predictions at multiple epochs", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(1)
  spec <- tabular_auto_int(
    epochs = 10L,
    num_embedding = 4L,
    num_attn_heads = 2L
  ) |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("regression")
  fit <- parsnip::fit(spec, mpg ~ ., data = mtcars)

  mp <- parsnip::multi_predict(fit, mtcars[1:3, ], epochs = c(3L, 7L))
  expect_s3_class(mp, "tbl_df")
  expect_equal(nrow(mp), 3)
  expect_named(mp, ".pred")

  inner <- mp$.pred[[1]]
  expect_true(all(c("epochs", ".pred") %in% names(inner)))
  expect_equal(inner$epochs, c(3L, 7L))
  expect_true(is.numeric(inner$.pred))

  # epochs = NULL defaults to the final fitted epoch
  mp_default <- parsnip::multi_predict(fit, mtcars[1:2, ])
  expect_equal(nrow(mp_default), 2)
  expect_equal(nrow(mp_default$.pred[[1]]), 1)
})

test_that("multi_predict._brulee_auto_int() works for classification", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(1)
  spec <- tabular_auto_int(
    epochs = 10L,
    num_embedding = 4L,
    num_attn_heads = 2L
  ) |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("classification")
  fit <- parsnip::fit(spec, Species ~ ., data = iris)

  mp <- parsnip::multi_predict(fit, iris[1:3, ], epochs = c(3L, 7L))
  expect_equal(nrow(mp), 3)

  inner <- mp$.pred[[1]]
  expect_true(".pred_class" %in% names(inner))
  expect_equal(inner$epochs, c(3L, 7L))
})
