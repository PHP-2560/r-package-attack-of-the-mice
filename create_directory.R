#STEP 0: Packages
install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

#STEP 1: Create your package directory
set_wd("parent-directory")
create("r-package")