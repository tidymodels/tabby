test_that("tabular_rln() creates a model spec with correct defaults", {
  spec <- tabular_rln()

  expect_s3_class(spec, "tabular_rln")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_rln() stores args as quosures", {
  spec <- tabular_rln(hidden_units = 10L, penalty_average = 1e-8)

  expect_equal(rlang::quo_get_expr(spec$args$hidden_units), 10L)
  expect_equal(rlang::quo_get_expr(spec$args$penalty_average), 1e-8)
})

test_that("tabular_rln() only supports regression mode", {
  spec <- tabular_rln(mode = "regression")
  expect_equal(spec$mode, "regression")

  expect_error(
    tabular_rln(mode = "classification") |> parsnip::set_engine("brulee"),
    regexp = "classification"
  )
})

test_that("update.tabular_rln() updates args correctly", {
  spec <- tabular_rln(hidden_units = 5L, epochs = 50L)
  updated <- update(spec, hidden_units = 20L)

  expect_equal(rlang::quo_get_expr(updated$args$hidden_units), 20L)
  expect_equal(rlang::quo_get_expr(updated$args$epochs), 50L)
})

test_that("update.tabular_rln() with fresh = TRUE replaces all args", {
  spec <- tabular_rln(hidden_units = 5L, epochs = 50L)
  updated <- update(spec, hidden_units = 20L, fresh = TRUE)

  expect_equal(rlang::quo_get_expr(updated$args$hidden_units), 20L)
  expect_null(rlang::quo_get_expr(updated$args$epochs))
})

test_that("check_args.tabular_rln() passes valid arguments", {
  expect_no_error(tabular_rln(penalty_type = "L1") |> parsnip::check_args())
  expect_no_error(tabular_rln(penalty_type = "L2") |> parsnip::check_args())
  expect_no_error(tabular_rln(penalty_average = 1e-10) |> parsnip::check_args())
  expect_no_error(tabular_rln(step_rate = 1e6) |> parsnip::check_args())
})

test_that("check_args.tabular_rln() rejects invalid penalty_type", {
  expect_error(
    tabular_rln(penalty_type = "L3") |> parsnip::check_args(),
    regexp = "penalty_type"
  )
})

test_that("check_args.tabular_rln() rejects negative penalty_average", {
  expect_error(
    tabular_rln(penalty_average = -1) |> parsnip::check_args(),
    regexp = "penalty_average"
  )
})

test_that("check_args.tabular_rln() rejects negative step_rate", {
  expect_error(
    tabular_rln(step_rate = -1) |> parsnip::check_args(),
    regexp = "step_rate"
  )
})

test_that("required_pkgs.tabular_rln() returns expected packages", {
  spec <- tabular_rln()
  expect_equal(required_pkgs(spec), c("brulee", "tabular"))
})

test_that("tabular_rln() is registered with parsnip", {
  reregister_model("tabular_rln")
  expect_no_error(make_tabular_rln())

  engines <- parsnip::show_engines("tabular_rln")
  expect_true("brulee" %in% engines$engine)
  expect_true(all(engines$mode == "regression"))
})

test_that("tabular_rln() fits and predicts with brulee engine", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(1)
  spec <- tabular_rln(hidden_units = 3L, epochs = 5L) |>
    parsnip::set_engine("brulee")

  fit <- parsnip::fit(spec, mpg ~ ., data = mtcars)

  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "brulee_rln")

  preds <- predict(fit, mtcars[1:5, ])
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_equal(nrow(preds), 5)
  expect_true(is.numeric(preds$.pred))
})

test_that("multi_predict._brulee_rln() returns predictions at multiple epochs", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(1)
  spec <- tabular_rln(hidden_units = 3L, epochs = 10L) |>
    parsnip::set_engine("brulee")
  fit <- parsnip::fit(spec, mpg ~ ., data = mtcars)

  mp <- parsnip::multi_predict(fit, mtcars[1:3, ], epochs = c(3L, 7L))
  expect_s3_class(mp, "tbl_df")
  expect_equal(nrow(mp), 3)
  expect_named(mp, ".pred")

  inner <- mp$.pred[[1]]
  expect_true(all(c("epochs", ".pred") %in% names(inner)))
  expect_equal(nrow(inner), 2)
  expect_equal(inner$epochs, c(3L, 7L))
})
