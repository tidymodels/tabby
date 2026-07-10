test_that("tabular_auto_int() engine is registered", {
  reregister_model("tabular_auto_int")
  expect_no_error(make_tabular_auto_int())

  engines <- parsnip::show_engines("tabular_auto_int")
  expect_true("brulee" %in% engines$engine)
  expect_setequal(engines$mode, c("classification", "regression"))
})

test_that("required_pkgs.tabular_auto_int() returns expected packages", {
  spec <- tabular_auto_int()
  pkgs <- required_pkgs(spec)

  expect_true("brulee" %in% pkgs)
  expect_true("tabby" %in% pkgs)
})

test_that("multi_predict._brulee_auto_int() returns predictions at multiple epochs", {
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_on_cran()

  set.seed(337873)
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

  set.seed(772885)
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
