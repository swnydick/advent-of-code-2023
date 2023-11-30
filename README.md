# Introduction

This repo contains my solutions to the "Advent of Code 2023" programming logic puzzles. The structure of the repo is setup to facilitate my constraints and allow code to be easily replicated. Each day has a folder with solutions for both parts of the puzzle on that day containing code and data for that day. To run the code:

1. Make sure that [R](https://www.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/) are installed on your system.
2. Open the "advent-of-code-2023.Rproj" in RStudio, which should automatically download and install the `renv` package.
3. Open the folder corresponding to the puzzle day (e.g., "01" for the first day's puzzle, "02" for the second day's puzzle).
4. Open the code (e.g., "01-code.R") in that folder.
5. Run that code in RStudio by clicking the "source" button.

My restrictions this year are simple. Write my solution in R without using any but the packages that:

1. Are installed with base R: `base`, `complier`, `datasets`, `grDevices`, `graphics`, `grid`, `methods`, `parallel`, `splines`, `stats`, `stats4`, `tcltk`, `tools`, `translations`, `utils`.
2. Are included with the default binary distributions: `KernSmooth`, `MASS`, `Matrix`, `boot`, `class`, `cluster`, `codetools`, `foreign`, `lattice`, `mgcv`, `nlme`, `nnet`, `rpart`, `spatial`, `survival`.

Moreover, I will only try to use packages from the second set (especially `MASS` and `Matrix`) if I cannot solve the problem with packages from the first set (generally `base` and `stats`). And even if I violate 1 and 2, I cannot use packages from the `tidyverse`. The `renv` package will enforce those constraints, as no other packages will be included in `renv.lock`.

# Completed Puzzles
