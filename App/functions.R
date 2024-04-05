install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
}

hasConverged <- function (mm) {
  if (!inherits(mm, "merMod") && !inherits(mm, "lm")) stop("Error: must pass a lmerMod or lm object")
  retval <- NULL
  if (inherits(mm, "merMod")) {
    if(is.null(unlist(mm@optinfo$conv$lme4))) {
      retval = 1
    }
    else {
      if (isSingular(mm)) {
        retval = 0
      } else {
        retval = -1
      }
    }
  }
  return(retval)
}