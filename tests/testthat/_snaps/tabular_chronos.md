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

