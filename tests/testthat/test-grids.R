test_that("neural_net_grid_space_filling() builds list-column grids", {
  skip_if_not_installed("dials")

  spec <- tabular_resnet(
    hidden_units = hardhat::tune(),
    bottleneck_units = hardhat::tune(),
    penalty = hardhat::tune()
  )

  grd <- neural_net_grid_space_filling(spec, size = 5)

  expect_s3_class(grd, "tbl_df")
  expect_setequal(names(grd), c("hidden_units", "bottleneck_units", "penalty"))
  expect_true(is.list(grd$hidden_units))
  expect_true(is.list(grd$bottleneck_units))
  # one integer vector per layer (num_layers default is 3)
  expect_length(grd$hidden_units[[1]], 3)
  expect_length(grd$bottleneck_units[[1]], 3)
})

test_that("neural_net_grid_space_filling() respects num_layers", {
  skip_if_not_installed("dials")

  spec <- tabular_resnet(
    hidden_units = hardhat::tune(),
    penalty = hardhat::tune()
  )

  grd <- neural_net_grid_space_filling(spec, num_layers = 2, size = 5)

  expect_length(grd$hidden_units[[1]], 2)
})

test_that("neural_net_grid_space_filling(collapse = FALSE) keeps wide columns", {
  skip_if_not_installed("dials")

  spec <- tabular_resnet(
    hidden_units = hardhat::tune(),
    bottleneck_units = hardhat::tune(),
    penalty = hardhat::tune()
  )

  grd <- neural_net_grid_space_filling(spec, size = 5, collapse = FALSE)

  expect_s3_class(grd, "tbl_df")
  expect_true(any(grepl("^\\.hidden_", names(grd))))
  expect_true(any(grepl("^\\.bottleneck_", names(grd))))
  expect_false(any(c("hidden_units", "bottleneck_units") %in% names(grd)))
})

test_that("expand_list_parameters() widens list-columns", {
  skip_if_not_installed("dials")

  spec <- tabular_resnet(
    hidden_units = hardhat::tune(),
    bottleneck_units = hardhat::tune(),
    penalty = hardhat::tune()
  )

  grd <- neural_net_grid_space_filling(spec, size = 5)
  wide <- expand_list_parameters(grd)

  expect_s3_class(wide, "tbl_df")
  expect_equal(nrow(wide), nrow(grd))
  # list-columns are replaced by one column per layer
  expect_false(any(c("hidden_units", "bottleneck_units") %in% names(wide)))
  expect_true(all(paste0("hidden_units_", 1:3) %in% names(wide)))
  expect_true(all(paste0("bottleneck_units_", 1:3) %in% names(wide)))
})

test_that("expand_list_parameters() returns input unchanged when no list-columns", {
  df <- tibble::tibble(a = 1:3, b = letters[1:3])
  expect_identical(expand_list_parameters(df), df)
})

test_that("expand_list_parameters() can target columns with a pattern", {
  skip_if_not_installed("dials")

  spec <- tabular_resnet(
    hidden_units = hardhat::tune(),
    bottleneck_units = hardhat::tune(),
    penalty = hardhat::tune()
  )

  grd <- neural_net_grid_space_filling(spec, size = 5)
  wide <- expand_list_parameters(grd, pattern = "^hidden_units$")

  # only the matched list-column is expanded; the other is left intact
  expect_true(all(paste0("hidden_units_", 1:3) %in% names(wide)))
  expect_true("bottleneck_units" %in% names(wide))
  expect_false("hidden_units" %in% names(wide))
})
