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
