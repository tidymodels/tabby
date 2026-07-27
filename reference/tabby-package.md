# tabby: Tidy Interfaces for Tabular Deep Learning

Model definitions for fitting, tuning, and predicting with deep learning
models for tabular data using the 'parsnip' and 'tidymodels' frameworks.
Supported models include the TabPFN foundation model of Hollmann et al.
(2025)
[doi:10.1038/s41586-024-08328-6](https://doi.org/10.1038/s41586-024-08328-6)
, TabICL by Qu et al. (2025)
[doi:10.48550/arXiv.2502.05564](https://doi.org/10.48550/arXiv.2502.05564)
, SAINT by Somepalli et al. (2021)
[doi:10.48550/arXiv.2106.01342](https://doi.org/10.48550/arXiv.2106.01342)
, and the tabular ResNet of Gorishniy et al. (2021)
[doi:10.48550/arXiv.2106.11959](https://doi.org/10.48550/arXiv.2106.11959)
, among others. Helper functions are also included to create
space-filling grids for tuning parameters that are defined on a
per-layer basis. These functions define the model interfaces; the
computations are carried out by separate engine packages built on
'torch'.

## See also

Useful links:

- <https://github.com/tidymodels/tabby>

- <https://tabby.tidymodels.org/>

- Report bugs at <https://github.com/tidymodels/tabby/issues>

## Author

**Maintainer**: Max Kuhn <max@posit.co>
([ORCID](https://orcid.org/0000-0003-2402-136X))

Authors:

- Max Kuhn <max@posit.co>
  ([ORCID](https://orcid.org/0000-0003-2402-136X))

- Edgar Ruiz <edgar@posit.co>

Other contributors:

- Posit Software, PBC ([ROR](https://ror.org/03wc8by49)) \[copyright
  holder, funder\]
