test_that("tabular_resnet() is registered with parsnip", {
  reregister_model("tabular_resnet")
  expect_no_error(make_tabular_resnet())

  engines <- parsnip::show_engines("tabular_resnet")
  expect_true("brulee" %in% engines$engine)
  expect_setequal(engines$mode, c("classification", "regression"))
})

test_that("required_pkgs.tabular_resnet() returns expected packages", {
  spec <- tabular_resnet()
  expect_equal(required_pkgs(spec), c("brulee", "tabby"))
})

test_that("tabular_resnet() fits and predicts (regression)", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(93413)
  # sized so the trailing batch isn't 1 row (brulee#122 NaN-poisons batch-norm)
  big <- mtcars[rep(seq_len(32), length.out = 200), ]
  spec <- tabular_resnet(hidden_units = 4L, epochs = 5L) |>
    parsnip::set_engine("brulee", batch_size = 40L) |>
    parsnip::set_mode("regression")

  fit <- parsnip::fit(spec, mpg ~ ., data = big)

  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "brulee_resnet")

  preds <- predict(fit, big[1:5, ])
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_equal(nrow(preds), 5)
  expect_true(all(is.finite(preds$.pred)))
})

test_that("tabular_resnet() fits and predicts (classification)", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(735998)
  spec <- tabular_resnet(hidden_units = 4L, epochs = 5L) |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("classification")

  fit <- parsnip::fit(spec, Species ~ ., data = iris)

  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "brulee_resnet")

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

test_that("multi_predict._brulee_resnet() returns predictions at multiple epochs", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(819425)
  # sized so the trailing batch isn't 1 row (brulee#122 NaN-poisons batch-norm)
  big <- mtcars[rep(seq_len(32), length.out = 200), ]
  spec <- tabular_resnet(hidden_units = 4L, epochs = 10L) |>
    parsnip::set_engine("brulee", batch_size = 40L) |>
    parsnip::set_mode("regression")
  fit <- parsnip::fit(spec, mpg ~ ., data = big)

  mp <- parsnip::multi_predict(fit, big[1:3, ], epochs = c(3L, 7L))
  expect_s3_class(mp, "tbl_df")
  expect_equal(nrow(mp), 3)
  expect_named(mp, ".pred")

  inner <- mp$.pred[[1]]
  expect_true(all(c("epochs", ".pred") %in% names(inner)))
  expect_equal(nrow(inner), 2)
  expect_equal(inner$epochs, c(3L, 7L))
  expect_true(all(is.finite(inner$.pred)))

  # epochs = NULL defaults to the final fitted epoch
  mp_default <- parsnip::multi_predict(fit, big[1:2, ])
  expect_equal(nrow(mp_default), 2)
  expect_equal(nrow(mp_default$.pred[[1]]), 1)
})

test_that("multi_predict._brulee_resnet() supports classification", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(530721)
  spec <- tabular_resnet(hidden_units = 4L, epochs = 10L) |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("classification")
  fit <- parsnip::fit(spec, Species ~ ., data = iris)

  mp <- parsnip::multi_predict(fit, iris[1:3, ], epochs = c(3L, 7L))
  expect_equal(nrow(mp), 3)

  inner <- mp$.pred[[1]]
  expect_true(".pred_class" %in% names(inner))
  expect_equal(inner$epochs, c(3L, 7L))
})

test_that("reformat_torch_num() widens multi-column results", {
  # exercises the multivariate branch not reached via single-outcome predict
  results <- matrix(c(1, 2, 3, 4), ncol = 2)
  object <- list(preproc = list(y_var = c("y1", "y2")))

  out <- reformat_torch_num(results, object)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("y1", "y2"))
  expect_equal(nrow(out), 2)
})

