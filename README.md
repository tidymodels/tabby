
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabby <a href='https://tabby.tidymodels.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/tidymodels/tabby/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/tabby/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/tabby/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/tabby)
<!-- badges: end -->

tabby provides [parsnip](https://parsnip.tidymodels.org/) interfaces for
tabular deep learning models. Within
[tidymodels](https://www.tidymodels.org/), they follow the same `fit()`
and `predict()` flow as any other model. This means you can do things
like tune and resample them right alongside the rest of your workflow.
tabby supplies only the interface, and the fitting happens in engine
packages you install separately, namely `brulee` and `tabpfn`. All
supported models run on Torch.

Two of the supported models ask a little more of your setup. TabPFN and
Chronos-2 are pretrained, so they download their weights the first time
you use them, and TabPFN also needs a Python environment, created via
`reticulate`. Being pretrained, they also have few or no hyperparameters
to tune, whereas the other supported models carry the usual knobs to
optimize.

### Supported models

| Model | Function | Mode | Engine | Python |
|----|----|----|----|:--:|
| [TabPFN](https://tabpfn.tidymodels.org) | `tabular_pfn()` | `classification`, `regression` | `tabpfn` | ✔ |
| [TabICL](https://brulee.tidymodels.org/reference/brulee_tab_icl.html) | `tabular_icl()` | `classification`, `regression` | `brulee` |  |
| [ResNet](https://brulee.tidymodels.org/reference/brulee_resnet.html) | `tabular_resnet()` | `classification`, `regression` | `brulee` |  |
| [SAINT](https://brulee.tidymodels.org/reference/brulee_saint.html) | `tabular_saint()` | `classification`, `regression` | `brulee` |  |
| [AutoInt](https://brulee.tidymodels.org/reference/brulee_auto_int.html) | `tabular_auto_int()` | `classification`, `regression` | `brulee` |  |
| [Regularization Learning Network](https://brulee.tidymodels.org/reference/brulee_rln.html) | `tabular_rln()` | `regression` | `brulee` |  |
| [Chronos-2](https://brulee.tidymodels.org/reference/brulee_chronos.html) (*forecasting*) | `tabular_chronos()` | `quantile regression`, `regression` | `brulee` |  |

## Installation

You can install the released version of tabby from
[CRAN](https://cran.r-project.org/package=tabby) with:

``` r
install.packages("tabby")
```

You can install the development version of tabby from
[GitHub](https://github.com/tidymodels/tabby) with:

``` r
# install.packages("pak")
pak::pak("tidymodels/tabby")
```

## Example

Create a model specification the same way you would for any parsnip
model. Here is a ResNet for classification:

``` r
library(tabby)
#> Loading required package: parsnip

resnet_spec <-
  tabular_resnet(
    mode = "classification",
    hidden_units = 32L,
    epochs = 100L,
    penalty = 0.01
  )

resnet_spec
#> tabular resnet Model Specification (classification)
#> 
#> Main Arguments:
#>   hidden_units = 32
#>   penalty = 0.01
#>   epochs = 100
#> 
#> Computational engine: brulee
```

Fit and predict as usual:

``` r
resnet_fit <- fit(resnet_spec, class ~ ., data = train_data)
predict(resnet_fit, new_data = test_data)
```

Mark any argument with `tune()` to optimize it with the
[tune](https://tune.tidymodels.org/) package.

## Tuning grids for layered networks

Some models take *per-layer* parameters, such as a vector of hidden
units with one entry per layer. tabby provides helpers to build
space-filling grids over these list-valued parameters:

``` r
rn_spec <-
  tabular_resnet(
    hidden_units = tune(),
    bottleneck_units = tune(),
    penalty = tune()
  )

rn_grid <- neural_net_grid_space_filling(rn_spec)
rn_grid |> expand_list_parameters()
```

## Code of Conduct

Please note that the tabby project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
