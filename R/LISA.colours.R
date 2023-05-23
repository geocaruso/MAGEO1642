#' LISA.colours.R
#'
#'Function to obtain a named vector of 5 standard colors for LISA maps
#' mimicking geoda colours.
#'
#' @param col_ns colour for non.significant LISA values
#' @param col_LL colour for significant values in Low-Low (low z, low lagged z) quadrant
#' @param col_LH colour ... Low-High ... quadrant
#' @param col_HL colour ... High-Low ... quadrant
#' @param col_HH colour ... High-High ... quadrant
#'
#' @return
#' @export
#'
#' @examples
LISA.colours<-function(col_ns = "white",
                       col_LL = "darkblue",
                       col_LH="lightblue",
                       col_HL = "pink",
                       col_HH="darkred"){
  return(c("n.s." = col_ns,
           "L.L." = col_LL,
           "L.H." = col_LH,
           "H.L." = col_HL,
           "H.H." = col_HH))
}
