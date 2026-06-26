test_that("tabular_resnet() is registered with parsnip", {
  reregister_model("tabular_resnet")
  expect_no_error(make_tabular_resnet())

  engines <- parsnip::show_engines("tabular_resnet")
  expect_true("brulee" %in% engines$engine)
  expect_setequal(engines$mode, c("classification", "regression"))
})
