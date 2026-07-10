# Mocks brulee's TabICL checkpoint lookup / config / weight load / inference path
# so the parsnip wrapper's fit + predict can be exercised end-to-end without
# network access or the pretrained safetensors weights (fetched by
# `brulee::tab_icl_download_weights()`). Mirrors the approach in
# `helper-chronos.R`. testthat sources every `helper-*.R` before the test files,
# so this is available to all icl tests.

stub_tabicl_loaders <- function() {
  fake_dir <- file.path(
    tempdir(check = TRUE),
    paste0("tabicl-stub-", as.integer(Sys.time()))
  )
  dir.create(fake_dir, recursive = TRUE, showWarnings = FALSE)

  bindings <- list(
    # `tabicl_cache_lookup()` normally locates a cached checkpoint and errors if
    # none is available; return a throwaway directory instead.
    tabicl_cache_lookup = function(task, call = rlang::caller_env()) {
      fake_dir
    },
    # Only `config$max_classes` is consumed at fit time: `> 0` marks a
    # classification checkpoint, `<= 0` a regression one. `tabicl_bridge()`
    # passes `file.path(path, files$config)`, so branch on the file name.
    tabicl_parse_config = function(path) {
      list(
        max_classes = if (grepl("classification", path)) 10L else 0L
      )
    },
    # `tabicl_load_model()` reads the safetensors weights; the fake object it
    # returns is never inspected because the inference cores below are mocked.
    tabicl_load_model = function(
      model_dir,
      task,
      device = "cpu",
      verbose = FALSE
    ) {
      structure(list(task = task), class = "fake_tabicl_module")
    },
    # Deterministic class probabilities: one row per test case, one column per
    # class, rows summing to 1. The arg-max is rotated across rows so `class`
    # predictions are non-constant (guards against the column extraction
    # collapsing to a single level).
    tabicl_classifier_proba = function(
      loaded,
      x_train,
      y_train,
      x_test,
      members,
      temperature = 0.9,
      device = "cpu"
    ) {
      n <- nrow(x_test)
      n_classes <- length(unique(y_train))
      proba <- matrix(0, nrow = n, ncol = n_classes)
      for (i in seq_len(n)) {
        top <- ((i - 1L) %% n_classes) + 1L
        proba[i, ] <- (1 - 0.6) / (n_classes - 1)
        proba[i, top] <- 0.6
      }
      proba
    },
    # Deterministic, non-constant regression predictions.
    tabicl_regressor_mean = function(
      loaded,
      x_train,
      y_train,
      x_test,
      members,
      device = "cpu"
    ) {
      seq_len(nrow(x_test)) + mean(y_train)
    }
  )

  do.call(
    testthat::local_mocked_bindings,
    c(bindings, list(.package = "brulee", .env = parent.frame()))
  )
}
