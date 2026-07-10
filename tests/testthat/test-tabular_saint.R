test_that("tabular_saint() engine is registered", {
  reregister_model("tabular_saint")
  expect_no_error(make_tabular_saint())

  engines <- parsnip::show_engines("tabular_saint")
  expect_true("brulee" %in% engines$engine)
  expect_setequal(engines$mode, c("classification", "regression"))
})

test_that("required_pkgs.tabular_saint() returns expected packages", {
  spec <- tabular_saint()
  pkgs <- required_pkgs(spec)

  expect_true("brulee" %in% pkgs)
  expect_true("tabby" %in% pkgs)
})

test_that("tabular_saint() fits and predicts (regression)", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(425917)
  spec <- tabular_saint(
    epochs = 5L,
    num_embedding = 4L,
    num_attn_heads = 2L,
    num_attn_blocks = 1L
  ) |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("regression")

  fit <- parsnip::fit(spec, mpg ~ ., data = mtcars)

  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "brulee_saint")

  preds <- predict(fit, mtcars[1:5, ])
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_equal(nrow(preds), 5)
  expect_true(is.numeric(preds$.pred))
})

test_that("tabular_saint() fits and predicts (classification)", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(672144)
  spec <- tabular_saint(
    epochs = 5L,
    num_embedding = 4L,
    num_attn_heads = 2L,
    num_attn_blocks = 1L
  ) |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("classification")

  fit <- parsnip::fit(spec, Species ~ ., data = iris)

  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "brulee_saint")

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

test_that("tabular_saint() does not support multi_predict()", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(565509)
  spec <- tabular_saint(
    epochs = 5L,
    num_embedding = 4L,
    num_attn_heads = 2L,
    num_attn_blocks = 1L
  ) |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("regression")
  fit <- parsnip::fit(spec, mpg ~ ., data = mtcars)

  expect_snapshot(
    error = TRUE,
    parsnip::multi_predict(fit, mtcars[1:3, ], epochs = c(3L, 7L))
  )
})

