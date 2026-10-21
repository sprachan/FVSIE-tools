#' Convert inside bark to outside bark diameter
#'
#' Uses the FVS-IE bark ratio equation (bark ratio = b1+b2/dob) to calculate
#' inside bark diameter (dib) as bark ratio*dob
#'
#' @param sp Character. FIA species code, with any leading zeroes.
#' @param dob Numeric. Outside bark diameter.
#'
#' @returns Numeric, length matches length of input. The inside bark diameter
#'   calculated using species-specific bark ratio relationships.
#'
#' @description For all species except Pinyon pine, rocky mountain maple,
#'   cottonwood, and other hardwood, the bark ratio is not diameter-dependent,
#'   so DIB = k*DOB where k is the constant species-specific bark ratio.
#' @export
#'
dob_to_dib <- function(sp, dob){
  stopifnot(length(sp) == length(dob))
  idx <- match(sp, rFVSIEtools::bark_ratios$fia_species)
  bratio <- rFVSIEtools::bark_ratios$b1[idx] + (rFVSIEtools::bark_ratios$b2[idx]/dob)
  bratio[bratio > 0.99] <- 0.99
  bratio[bratio < 0.8] <- 0.8
  dib <- bratio*dob
  dib
}
