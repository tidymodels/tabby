#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
## usethis namespace: end
NULL

#' @importFrom parsnip multi_predict
#' @export
parsnip::multi_predict

#' @importFrom parsnip check_args
#' @export
parsnip::check_args

#' @importFrom generics required_pkgs
#' @export
generics::required_pkgs

# ------------------------------------------------------------------------------

utils::globalVariables(c("id", "quantile_levels"))

# ------------------------------------------------------------------------------
# From other packages

# nocov start
# parsnip
load_libs <- function(x, quiet, attach = FALSE) {
  for (pkg in x$method$libs) {
    if (!attach) {
      suppressPackageStartupMessages(requireNamespace(pkg, quietly = quiet))
    } else {
      library(pkg, character.only = TRUE, quietly = quiet)
    }
  }
  invisible(x)
}

# recipes
names0 <- function(num, prefix = "x") {
  ind <- format(seq_len(num))
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}
# nocov end
