check_packages = function(names){
  for(name in names){
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org",quiet=TRUE,dependencies=FALSE) #if package not installed, install the package
    library(name, character.only=TRUE,warn.conflicts=FALSE,quietly=TRUE)
  }
}


# how to load:
# check_packages(c("dplyr","ggplot2"))