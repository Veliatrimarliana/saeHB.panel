#' @title Sample Data for Small Area Estimation using Hierarchical Bayesian Method for Rao Yu Model when \code{rho = 0} with Non Sampled Area
#'
#' @description
#' \enumerate{
#'   \item A dataset to simulate Small Area Estimation using Hierarchical Bayesian method for Rao-Yu Model with Non-sampled area
#'   \item This data contains NA values that indicates no sampled in at least one area.
#'  }
#'
#' @format A data frame with 100 row and 6 column:
#' \describe{
#'   \item{ydi}{Direct Estimation of y}
#'   \item{area}{Area (domain) of the data}
#'   \item{period}{Period (subdomain) of the data}
#'   \item{vardir}{Sampling Variance of y}
#'   \item{xdi1}{Auxiliary variable of xdi1}
#'   \item{xdi2}{Auxiliary variable of xdi2}
#' }
#'
"dataPanelNs"
