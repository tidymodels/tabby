test_that("tabular_pfn() is registered with parsnip", {
  reregister_model("tabular_pfn")
  expect_no_error(make_tabular_pfn())

  engines <- parsnip::show_engines("tabular_pfn")
  expect_true("tabpfn" %in% engines$engine)
  expect_setequal(engines$mode, c("classification", "regression"))
})

test_that("required_pkgs.tabular_pfn() returns expected packages", {
  spec <- tabular_pfn()
  expect_equal(required_pkgs(spec), c("tabpfn", "tabby"))
})

test_that("tabular_pfn() registers brulee engine arguments for tuning", {
  reregister_model("tabular_pfn")
  make_tabular_pfn()
  make_tabular_brulee_pfn()

  res <- generics::tunable(parsnip::set_engine(tabular_pfn(), "brulee"))
  res <- res[res$component_id == "engine", ]
  expect_identical(res$name, "training_set_limit")
  expect_identical(res$call_info[[1]]$pkg, "dials")
  expect_identical(res$call_info[[1]]$fun, "training_set_limit")
})
