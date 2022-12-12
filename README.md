## Distance added to Random Forests (DistRF)

The R package `DistRF` calulates euclidean distance as well as log-likelihood to
improve classification when handling imbalanced data. In addition, the package provides tools to evaluate
the preformance of the classifier considering imbalanced data. The measures available are True positive rate,
True negative rate, Area under the curve and ROC curve.

To download and install the package, use `devtools`

```r
library(devtools)
devtools::install_github("ShiraMingelgrin/DistRF")
```

You can subsequently load the package with the usual R commands:

```r
library(iRF)
```

### Workflow Overview


