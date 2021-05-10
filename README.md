# cvlt
Cross-validation methods for LDATS

This is a package to extend and improve the performance of [LDATS](https://weecology.github.io/LDATS) when applied to short, high-dimensional timeseries (such as the Breeding Bird Survey). Specifically, it includes functions to implement model selection via leave-one-out crossvalidation, and summary functions for examining the number, timing, and magnitude of community reorganization events. The functions are designed to work with data obtained via the [MATSS package](https://weecology.github.io/MATSS) and to run at scale using [drake](https://docs.ropensci.org/drake/) or similar pipeline managers. 
