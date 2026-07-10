test_that("required_pkgs.tabular_rln() returns expected packages", {
  spec <- tabular_rln()
  expect_equal(required_pkgs(spec), c("brulee", "tabby"))
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

  set.seed(193044)
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

  set.seed(403479)
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

  # epochs = NULL defaults to the final fitted epoch
  mp_default <- parsnip::multi_predict(fit, mtcars[1:2, ])
  expect_equal(nrow(mp_default), 2)
  expect_equal(nrow(mp_default$.pred[[1]]), 1)
})

