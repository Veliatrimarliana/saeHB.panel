#' @title Sample Data for Small Area Estimation using Hierarchical Bayesian Method for Rao Yu Model when \code{rho = 0}
#'
#' @description Dataset to simulate Small Area Estimation using Hierarchical Bayesian Method for Rao-Yu Model with rho = 0
#' This data is generated by these following steps:
#' \enumerate{
#' \item Generate random effect area \code{v}, random effect for area i at time point j \code{u}, epsilon \eqn{\epsilon}, variance of ydi \code{vardir}, sampling error \code{e}, auxiliary \code{xdi1} and \code{xdi2}
#' \itemize{
#' \item Set coefficient \eqn{\beta_{0}=\beta_{1}=\beta_{2}=2} and \eqn{\rho = -0,5}
#' \item Generate random effect area \code{v_{i}~N(0,1)}
#' \item Generate auxiliary variable \code{xdi1_{ij}~U(1,2)}
#' \item Generate auxiliary variable \code{xdi2_{ij}~U(1,3)}
#' \item Generate epsilon \eqn{\epsilon_{ij}}\code{~N(0,1)}
#' \item Calculate variance of ydi with \code{vardir_{ij}~IG(10,6)}
#' \item Generate sampling error \code{e_{ij}~N(0,vardir_{ij})}
#' \item Calculate \eqn{\mu_{ij}=\beta_{0}+\beta_{1}xdi1_{ij}+\beta_{2}xdi2_{ij}+v_{i}+\epsilon_{ij}+e_{ij}}
#' \item Set \code{area=50} and \code{period=10}
#' }
#' \item Auxiliary variables \code{xdi1,xdi2}, direct estimation \code{y}, \code{area}, \code{period}, and \code{vardir} are combined in a dataframe called \code{dataPanel}
#' }
#' @format A data frame with 100 rows and 6 variables::
#' \describe{
#'   \item{ydi}{Direct Estimation of y}
#'   \item{area}{Area (domain) of the data}
#'   \item{period}{Period (subdomain) of the data}
#'   \item{vardir}{Sampling Variance of y}
#'   \item{xdi1}{Auxiliary variable of xdi1}
#'   \item{xdi2}{Auxiliary variable of xdi2}
#' }
#'
"dataPanel"
