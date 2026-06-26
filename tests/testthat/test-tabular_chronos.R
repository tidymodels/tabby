test_that("tabular_chronos() creates a model spec with correct defaults", {
  spec <- tabular_chronos()

  expect_s3_class(spec, "tabular_chronos")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "brulee")
})

test_that("set_mode() records quantile_levels and supports regression", {
  qr <- tabular_chronos() |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("quantile regression", quantile_levels = c(.1, .5, .9))
  expect_equal(qr$mode, "quantile regression")
  expect_equal(qr$quantile_levels, c(.1, .5, .9))

  rg <- tabular_chronos() |> parsnip::set_mode("regression")
  expect_equal(rg$mode, "regression")
})

test_that("update.tabular_chronos() returns a spec", {
  spec <- tabular_chronos()
  updated <- update(spec, parameters = NULL)
  expect_s3_class(updated, "tabular_chronos")
})

test_that("required_pkgs.tabular_chronos() returns expected packages", {
  expect_equal(required_pkgs(tabular_chronos()), c("brulee", "tabular"))
})

test_that("tabular_chronos() is registered for both modes", {
  reregister_model("tabular_chronos")
  expect_no_error(make_tabular_chronos())

  engines <- parsnip::show_engines("tabular_chronos")
  expect_true("brulee" %in% engines$engine)
  expect_setequal(engines$mode, c("quantile regression", "regression"))
})

test_that("engine args translate into the fit call", {
  spec <- tabular_chronos() |>
    parsnip::set_engine("brulee", prediction_length = 14L) |>
    parsnip::set_mode("quantile regression", quantile_levels = (1:9) / 10)

  trans <- parsnip::translate(spec)
  fit_args <- trans$method$fit$args

  expect_true("prediction_length" %in% names(fit_args))
  # `quantile_levels` is forwarded from the mode to the fit automatically.
  expect_true("quantile_levels" %in% names(fit_args))
})

test_that("tabular_chronos() fits and forecasts in quantile regression mode", {
  skip_on_cran()
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_if_not_installed("modeldata")

  stub_chronos_loaders(also_mock_predict_core = TRUE)
  Chi <- chicago_subset()

  spec <- tabular_chronos() |>
    parsnip::set_engine(
      "brulee",
      id_column = "series_id",
      timestamp_column = "date",
      prediction_length = 14L
    ) |>
    parsnip::set_mode("quantile regression", quantile_levels = (1:9) / 10)

  fit <- parsnip::fit(spec, ridership ~ Clark_Lake + Austin, data = Chi)
  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "brulee_chronos")

  preds <- predict(fit, Chi)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred_quantile")
  expect_s3_class(preds$.pred_quantile, "quantile_pred")
  expect_equal(nrow(preds), 14L)
})

test_that("tabular_chronos() forecasts the median in regression mode", {
  skip_on_cran()
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_if_not_installed("modeldata")

  stub_chronos_loaders(also_mock_predict_core = TRUE)
  Chi <- chicago_subset()

  spec <- tabular_chronos() |>
    parsnip::set_engine(
      "brulee",
      id_column = "series_id",
      timestamp_column = "date",
      prediction_length = 14L
    ) |>
    parsnip::set_mode("regression")

  fit <- parsnip::fit(spec, ridership ~ Clark_Lake + Austin, data = Chi)
  expect_s3_class(fit$fit, "brulee_chronos")

  preds <- predict(fit, Chi)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_true(is.numeric(preds$.pred))
  expect_equal(nrow(preds), 14L)
  # Guard against the column extraction / median collapsing to a constant: the
  # deterministic stub varies the forecast by horizon step, so .pred must be
  # non-constant.
  expect_gt(length(unique(preds$.pred)), 1L)
})

test_that("tabular_chronos() errors on multiple series via the parsnip interface", {
  skip_on_cran()
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  stub_chronos_loaders(also_mock_predict_core = TRUE)

  set.seed(656377)
  n <- 30L
  multi <- data.frame(
    series_id = rep(c("A", "B"), each = n),
    idx = rep(seq_len(n), times = 2L),
    y = rnorm(2L * n),
    cov1 = rnorm(2L * n)
  )

  base_spec <- tabular_chronos() |>
    parsnip::set_engine(
      "brulee",
      id_column = "series_id",
      timestamp_column = "idx",
      prediction_length = 5L
    )

  # Both prediction types route through chronos_single_series(): regression mode
  # via chronos_numeric, quantile regression mode via chronos_quantile.
  reg_fit <- base_spec |>
    parsnip::set_mode("regression") |>
    parsnip::fit(y ~ cov1, data = multi)
  expect_snapshot(error = TRUE, predict(reg_fit, multi))

  qr_fit <- base_spec |>
    parsnip::set_mode("quantile regression", quantile_levels = (1:9) / 10) |>
    parsnip::fit(y ~ cov1, data = multi)
  expect_snapshot(error = TRUE, predict(qr_fit, multi))
})
