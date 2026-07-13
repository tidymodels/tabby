# tabular_saint() does not support multi_predict()

    Code
      parsnip::multi_predict(fit, mtcars[1:3, ], epochs = c(3L, 7L))
    Condition
      Error in `parsnip::multi_predict()`:
      ! No `multi_predict()` method exists for objects with classes <_brulee_saint/model_fit>.

