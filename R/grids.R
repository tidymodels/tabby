#' Working with Tuning Parameters that are Lists
#'
#' `neural_net_grid_space_filling()` takes the number of layers in a neural
#' network as an input, creates a space-filling design with the multidimensional
#' layer parameter(s), and collapses the grid into list parameters that the
#' model function needs. `expand_list_parameters()` is a convenience function
#' that converts the list columns to a wide structure for printing,
#' visualization, etc.
#' @param wflow A model specification or [workflows::workflow()] object.
#' @param num_layers A single integer for the number of layers containing hidden
#' units.
#' @param size The _minimum_ grid size. It may be adjusted upwards to find a
#' feasible design.
#' @param param_info A [dials::parameters()] object or `NULL`. If none is given, a
#' parameters set is derived from other arguments. Passing this argument can be
#' useful when parameter ranges need to be customized.
#' @param range A range for the number of hidden units in each layer.
#' @param collapse A single logical for whether to collapse the parameters into
#' list columns.
#' @param x A data frame of grid points, some of which are list columns.
#' @param pattern A regular expression pattern to select which list columns
#' should be expanded to a wide format.
#' @return `neural_net_grid_space_filling()` returns a tibble with grid points,
#' some of which are list-columns containing integer vectors.
#' `expand_list_parameters()` returns a tibble where the selected list columns
#' are replaced by one integer column per layer.
#' @examples
#' rn_spec <-
#'   tabular_resnet(hidden_units = tune(),
#'              bottleneck_units = tune(),
#'              penalty = tune())
#'
#' rn_grid <- neural_net_grid_space_filling(rn_spec)
#' rn_grid
#'
#' rn_grid |> expand_list_parameters()
#' @export
neural_net_grid_space_filling <- function(
  wflow,
  num_layers = 3,
  size = 10,
  param_info = NULL,
  range = c(2, 50),
  collapse = TRUE
) {
  if (is.null(param_info)) {
    param_info <- wflow |> hardhat::extract_parameter_set_dials()
  }
  tune_hidden <- any(param_info$id == "hidden_units")
  tune_bottleneck <- any(param_info$id == "bottleneck_units")

  p <- num_layers * (tune_bottleneck + 1)

  param_expanded <- vector(mode = "list", length = p)
  names(param_expanded)[1:num_layers] <- names0(num_layers, ".hidden_")

  if (tune_bottleneck) {
    names(param_expanded)[(num_layers + 1):p] <- names0(
      num_layers,
      ".bottleneck_"
    )
  }

  for (i in 1:p) {
    param_expanded[[i]] <- dials::hidden_units(range)
  }

  param_expanded <- dials::parameters(param_expanded)

  param_expanded <- dplyr::bind_rows(param_expanded, param_info)
  if (tune_hidden) {
    param_expanded <- dplyr::filter(param_expanded, id != "hidden_units")
  }
  if (tune_bottleneck) {
    param_expanded <- dplyr::filter(param_expanded, id != "bottleneck_units")
  }

  class(param_expanded) <- class(param_info)

  grd <- dials::grid_space_filling(
    param_expanded,
    size = max(size, ncol(param_info))
  )
  if (collapse) {
    hidden_grid <-
      grd |>
      dplyr::select(dplyr::matches("^\\.hidden_")) |>
      apply(1, function(x) unname(x), simplify = FALSE)
    bottleneck_grid <- grd |>
      dplyr::select(dplyr::matches("^\\.bottleneck_")) |>
      apply(1, function(x) unname(x), simplify = FALSE)

    unit_grid <- dplyr::tibble(
      hidden_units = hidden_grid,
      bottleneck_units = bottleneck_grid
    )
    grd <-
      dplyr::bind_cols(
        unit_grid,
        grd |>
          dplyr::select(
            -dplyr::matches("^\\.hidden"),
            -dplyr::matches("^\\.bottleneck")
          )
      )
  }
  grd
}

#' @export
#' @name neural_net_grid_space_filling
expand_list_parameters <- function(x, pattern = "*") {
  is_lst_param <- purrr::map_lgl(x, is.list)
  if (!any(is_lst_param)) {
    return(x)
  }
  param_names <- grep(pattern, names(x)[is_lst_param], value = TRUE)
  for (i in param_names) {
    expanded <- purrr::map_dfr(1:nrow(x), ~ rename_lst_parameter(i, x[.x, ]))
    x <-
      dplyr::bind_cols(x, expanded) |>
      # dplyr::relocate(.after = c(dplyr::all_of(i))) |>
      dplyr::select(-!!i)
  }
  x
}

rename_lst_parameter <- function(nm, val) {
  x <- as.list(val[[nm]][[1]])
  names(x) <- names0(length(x), paste0(nm, "_"))
  tibble::as_tibble_row(x)
}
