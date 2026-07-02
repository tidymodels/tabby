test_that("tabular_icl() creates a model spec with correct defaults", {
  spec <- tabular_icl()

  expect_s3_class(spec, "tabular_icl")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_icl() accepts mode and engine", {
  spec <- tabular_icl(mode = "classification")
  expect_equal(spec$mode, "classification")

  spec <- tabular_icl(mode = "regression")
  expect_equal(spec$mode, "regression")
})

test_that("tabular_icl() captures arguments as quosures", {
  spec <- tabular_icl(num_estimators = 8L, softmax_temperature = 0.9)

  expect_equal(rlang::quo_get_expr(spec$args$num_estimators), 8L)
  expect_equal(rlang::quo_get_expr(spec$args$softmax_temperature), 0.9)
})

test_that("tabular_icl() is registered with parsnip", {
  reregister_model("tabular_icl")
  expect_no_error(make_tabular_icl())

  engines <- parsnip::show_engines("tabular_icl")
  expect_true("brulee" %in% engines$engine)
  expect_setequal(engines$mode, c("classification", "regression"))
})

test_that("update.tabular_icl() updates args correctly", {
  spec <- tabular_icl(num_estimators = 4L, softmax_temperature = 0.9)
  updated <- update(spec, num_estimators = 16L)

  expect_equal(rlang::quo_get_expr(updated$args$num_estimators), 16L)
  expect_equal(rlang::quo_get_expr(updated$args$softmax_temperature), 0.9)
})

test_that("update.tabular_icl() with fresh = TRUE replaces all args", {
  spec <- tabular_icl(num_estimators = 4L, softmax_temperature = 0.9)
  updated <- update(spec, num_estimators = 16L, fresh = TRUE)

  expect_equal(rlang::quo_get_expr(updated$args$num_estimators), 16L)
  expect_null(rlang::quo_get_expr(updated$args$softmax_temperature))
})

test_that("check_args.tabular_icl() passes valid arguments", {
  expect_no_error(tabular_icl(mode = "classification") |> parsnip::check_args())
  expect_no_error(
    tabular_icl(
      mode = "classification",
      num_estimators = 8L,
      softmax_temperature = 0.9
    ) |>
      parsnip::check_args()
  )
})

test_that("check_args.tabular_icl() validates softmax_temperature", {
  spec <- tabular_icl(mode = "classification", softmax_temperature = -1)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("check_args.tabular_icl() validates num_estimators", {
  spec <- tabular_icl(mode = "classification", num_estimators = -1)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))

  spec <- tabular_icl(mode = "classification", num_estimators = 1.5)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("required_pkgs.tabular_icl() returns expected packages", {
  expect_equal(required_pkgs(tabular_icl()), c("brulee", "tabby"))
})

test_that("engine args translate into the fit call", {
  spec <- tabular_icl(num_estimators = 4L, softmax_temperature = 0.5) |>
    parsnip::set_engine("brulee") |>
    parsnip::set_mode("classification")

  trans <- parsnip::translate(spec)
  fit_args <- trans$method$fit$args

  expect_true("num_estimators" %in% names(fit_args))
  expect_true("softmax_temperature" %in% names(fit_args))
})

test_that("tabular_icl() fits and predicts (classification)", {
  skip_on_cran()
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_if_not_installed("modeldata")

  stub_tabicl_loaders()
  dat <- scat_subset()

  spec <- tabular_icl(mode = "classification")
  fit <- parsnip::fit(spec, Species ~ ., data = dat)

  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "brulee_tab_icl")

  cls <- predict(fit, dat)
  expect_s3_class(cls, "tbl_df")
  expect_named(cls, ".pred_class")
  expect_s3_class(cls$.pred_class, "factor")
  expect_equal(levels(cls$.pred_class), levels(dat$Species))
  expect_equal(nrow(cls), nrow(dat))
  expect_gt(length(unique(cls$.pred_class)), 1L)

  prob <- predict(fit, dat, type = "prob")
  expect_s3_class(prob, "tbl_df")
  expect_named(prob, paste0(".pred_", levels(dat$Species)))
  expect_equal(nrow(prob), nrow(dat))
  expect_equal(rowSums(as.matrix(prob)), rep(1, nrow(dat)))
})

test_that("tabular_icl() fits and predicts (regression)", {
  skip_on_cran()
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_if_not_installed("modeldata")

  stub_tabicl_loaders()
  dat <- deliveries_subset()

  spec <- tabular_icl(mode = "regression")
  fit <- parsnip::fit(spec, time_to_delivery ~ ., data = dat)

  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "brulee_tab_icl")

  preds <- predict(fit, dat)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_true(is.numeric(preds$.pred))
  expect_equal(nrow(preds), nrow(dat))
  expect_gt(length(unique(preds$.pred)), 1L)
})

test_that("main args reach the fitted brulee object", {
  skip_on_cran()
  skip_if_not_installed("brulee")
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_if_not_installed("modeldata")

  stub_tabicl_loaders()
  dat <- scat_subset()

  spec <- tabular_icl(
    mode = "classification",
    num_estimators = 1L,
    softmax_temperature = 0.5
  )
  fit <- parsnip::fit(spec, Species ~ ., data = dat)

  expect_equal(fit$fit$num_estimators, 1L)
  expect_equal(fit$fit$softmax_temperature, 0.5)
})
