# Mocks brulee's TabICL checkpoint lookup / load / forward-pass path so the
# parsnip wrapper's fit + predict can be exercised end-to-end without network
# access or the >200MB pretrained weights. Follows the same pattern as
# helper-chronos.R. testthat sources every `helper-*.R` before the test files,
# so this is available to all tab_icl tests.

stub_tabicl_loaders <- function() {
  fake_dir <- file.path(
    tempdir(check = TRUE),
    paste0("tabicl-stub-", as.integer(Sys.time()))
  )
  dir.create(fake_dir, recursive = TRUE, showWarnings = FALSE)

  # The fit-time bridge checks `max_classes` for task/config consistency: > 0
  # for classification, <= 0 for regression. The config file name is
  # task-prefixed (classification.config.json / regression.config.json).
  fake_config <- function(path) {
    if (grepl("^classification", basename(path))) {
      list(max_classes = 10L)
    } else {
      list(max_classes = 0L)
    }
  }

  testthat::local_mocked_bindings(
    tabicl_cache_lookup = function(task, call = rlang::caller_env()) {
      fake_dir
    },
    tabicl_parse_config = fake_config,
    # Mocking at the `tabicl_load_model()` level covers config parsing,
    # safetensors loading, module construction, `$eval()`, and `$to()` in one
    # stub, so no fake torch module methods are needed.
    tabicl_load_model = function(
      model_dir,
      task,
      device = "cpu",
      verbose = FALSE
    ) {
      list(
        model = structure(list(), class = "fake_tabicl_module"),
        config = fake_config(paste0(task, ".config.json"))
      )
    },
    # Deterministic [nrow(x_test), n_classes] probabilities that vary by row so
    # class/prob extraction and row alignment are actually exercised. The real
    # engine also takes the class count from the encoded 0-based `y_train`.
    tabicl_classifier_proba = function(
      loaded,
      x_train,
      y_train,
      x_test,
      members,
      temperature = 0.9,
      device = "cpu"
    ) {
      n_classes <- length(unique(y_train))
      n <- nrow(x_test)
      raw <- outer(
        seq_len(n),
        seq_len(n_classes),
        function(i, j) ((i + j) %% 5) + 1
      )
      raw / rowSums(raw)
    },
    # Deterministic non-constant numeric predictions, guarding against a
    # collapse to a constant in the column extraction.
    tabicl_regressor_mean = function(
      loaded,
      x_train,
      y_train,
      x_test,
      members,
      device = "cpu"
    ) {
      mean(y_train) + seq_len(nrow(x_test)) / 2
    },
    .package = "brulee",
    .env = parent.frame()
  )
}

# A small classification set with a factor predictor and numeric predictors
# containing missing values, so brulee's ordinal-encoding and mean-imputation
# paths run. Stratified so every species is present in the subset.
scat_subset <- function(n_per_class = 20) {
  data(scat, package = "modeldata", envir = environment())
  scat <- scat[, c("Species", "Month", "Length", "Diameter", "Mass")]
  idx <- unlist(lapply(levels(scat$Species), function(lvl) {
    pool <- which(scat$Species == lvl)
    pool[seq_len(min(n_per_class, length(pool)))]
  }))
  scat[idx, ]
}

# A small regression set with numeric and factor predictors.
deliveries_subset <- function(n = 100) {
  data(deliveries, package = "modeldata", envir = environment())
  deliveries[
    seq_len(n),
    c("time_to_delivery", "hour", "day", "distance", "item_01")
  ]
}
