# Conversion function suggested by R Bivand between sgbp and nb objects
# https://cran.r-hub.io/web/packages/spdep/vignettes/nb_sf.html
as.nb.sgbp <- function(x, ...) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}