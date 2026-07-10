test_that("tabular_icl() is registered with parsnip", {
  reregister_model("tabular_icl")
  expect_no_error(make_tabular_icl())

  engines <- parsnip::show_engines("tabular_icl")
  expect_true("brulee" %in% engines$engine)
  expect_setequal(engines$mode, c("classification", "regression"))
})

test_that("required_pkgs.tabular_icl() returns expected packages", {
  spec <- tabular_icl()
  expect_equal(required_pkgs(spec), c("brulee", "tabby"))
})

test_that("tabular_icl() fits and predicts in classification mode", {
  skip_on_cran()
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  stub_tabicl_loaders()

  spec <- tabular_icl() |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("classification")

  fit <- parsnip::fit(spec, Species ~ ., data = iris)
  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "brulee_tab_icl")

  cls <- predict(fit, iris)
  expect_s3_class(cls, "tbl_df")
  expect_named(cls, ".pred_class")
  expect_s3_class(cls$.pred_class, "factor")
  expect_equal(nrow(cls), nrow(iris))
  # The deterministic stub rotates the arg-max across rows, so predictions must
  # not collapse to a single class.
  expect_gt(length(unique(cls$.pred_class)), 1L)

  prob <- predict(fit, iris, type = "prob")
  expect_s3_class(prob, "tbl_df")
  expect_named(
    prob,
    paste0(".pred_", levels(iris$Species))
  )
  expect_equal(nrow(prob), nrow(iris))
})

test_that("tabular_icl() fits and predicts in regression mode", {
  skip_on_cran()
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  stub_tabicl_loaders()

  spec <- tabular_icl() |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("regression")

  fit <- parsnip::fit(spec, mpg ~ ., data = mtcars)
  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "brulee_tab_icl")

  preds <- predict(fit, mtcars)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_true(is.numeric(preds$.pred))
  expect_equal(nrow(preds), nrow(mtcars))
  # The deterministic stub varies the prediction by row, so .pred must be
  # non-constant.
  expect_gt(length(unique(preds$.pred)), 1L)
})

