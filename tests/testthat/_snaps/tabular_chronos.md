# tabular_chronos() errors on multiple series via the parsnip interface

    Code
      predict(reg_fit, multi)
    Condition
      Error in `object$spec$method$pred$numeric$post()`:
      ! The parsnip interface to `brulee::brulee_chronos()` forecasts a single series.
      i Multiple series were detected (id column "series_id").
      i For multi-series forecasting, use `brulee::brulee_chronos()` directly.

---

    Code
      predict(qr_fit, multi)
    Condition
      Error in `object$spec$method$pred$quantile$post()`:
      ! The parsnip interface to `brulee::brulee_chronos()` forecasts a single series.
      i Multiple series were detected (id column "series_id").
      i For multi-series forecasting, use `brulee::brulee_chronos()` directly.

# tabular_chronos() forecast length restiction

    Code
      predict(fit, Chi)
    Condition
      Error in `predict()`:
      ! Series "L": `new_data` has 200 rows, more than the prediction length (2).

