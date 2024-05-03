packages = c("metafor", "tidyverse", "readxl", "dplyr", "broom")

## Now load or install & load all iff necessary
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
