test_that("tabular_pfn() creates a model spec with correct defaults", {
  spec <- tabular_pfn()

  expect_s3_class(spec, "tabular_pfn")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "tabpfn")
})

test_that("tabular_pfn() accepts mode and engine", {
  spec <- tabular_pfn(mode = "classification")
  expect_equal(spec$mode, "classification")

  spec <- tabular_pfn(mode = "regression")
  expect_equal(spec$mode, "regression")
})

test_that("tabular_pfn() captures arguments as quosures", {
  spec <- tabular_pfn(
    num_estimators = 8L,
    softmax_temperature = 0.9,
    balance_probabilities = TRUE,
    average_before_softmax = FALSE
  )

  expect_equal(rlang::quo_get_expr(spec$args$num_estimators), 8L)
  expect_equal(rlang::quo_get_expr(spec$args$softmax_temperature), 0.9)
  expect_equal(rlang::quo_get_expr(spec$args$balance_probabilities), TRUE)
  expect_equal(rlang::quo_get_expr(spec$args$average_before_softmax), FALSE)
})

test_that("tabular_pfn() is registered with parsnip", {
  reregister_model("tabular_pfn")
  expect_no_error(make_tabular_pfn())

  engines <- parsnip::show_engines("tabular_pfn")
  expect_true("tabpfn" %in% engines$engine)
  expect_setequal(engines$mode, c("classification", "regression"))
})

test_that("update.tabular_pfn() updates args correctly", {
  spec <- tabular_pfn(num_estimators = 4L, softmax_temperature = 0.9)
  updated <- update(spec, num_estimators = 16L)

  expect_equal(rlang::quo_get_expr(updated$args$num_estimators), 16L)
  expect_equal(rlang::quo_get_expr(updated$args$softmax_temperature), 0.9)
})

test_that("update.tabular_pfn() with fresh = TRUE replaces all args", {
  spec <- tabular_pfn(num_estimators = 4L, softmax_temperature = 0.9)
  updated <- update(spec, num_estimators = 16L, fresh = TRUE)

  expect_equal(rlang::quo_get_expr(updated$args$num_estimators), 16L)
  expect_null(rlang::quo_get_expr(updated$args$softmax_temperature))
})

test_that("check_args.tabular_pfn() passes valid arguments", {
  expect_no_error(tabular_pfn(mode = "classification") |> parsnip::check_args())
  expect_no_error(
    tabular_pfn(
      mode = "classification",
      num_estimators = 8L,
      softmax_temperature = 0.9,
      balance_probabilities = TRUE,
      average_before_softmax = FALSE
    ) |>
      parsnip::check_args()
  )
})

test_that("check_args.tabular_pfn() validates softmax_temperature", {
  spec <- tabular_pfn(mode = "classification", softmax_temperature = -1)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("check_args.tabular_pfn() validates num_estimators", {
  spec <- tabular_pfn(mode = "classification", num_estimators = -1)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("check_args.tabular_pfn() validates balance_probabilities", {
  spec <- tabular_pfn(mode = "classification", balance_probabilities = "yes")
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("check_args.tabular_pfn() validates average_before_softmax", {
  spec <- tabular_pfn(mode = "classification", average_before_softmax = 1)
  expect_snapshot(error = TRUE, parsnip::check_args(spec))
})

test_that("required_pkgs.tabular_pfn() returns expected packages", {
  spec <- tabular_pfn()
  expect_equal(required_pkgs(spec), c("tabpfn", "tabby"))
})
