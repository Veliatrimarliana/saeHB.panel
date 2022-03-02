#' @title Small Area Estimation using Hierarchical Bayesian under Rao-Yu Model with \code{rho=0}
#' @description This function is implemented to variable of interest \code{ydi}
#' @param formula Formula that describe the fitted model
#' @param area Number of areas (domain) of the data
#' @param period Number of periods (subdomains) for each area of the data
#' @param vardir Sampling variances of direct estimations
#' @param iter.update Number of updates with default \code{3}
#' @param iter.mcmc Number of total iterations per chain with default \code{2000}
#' @param thin Thinning rate, must be a positive integer with default \code{1}
#' @param burn.in Number of iterations to discard at the beginning with default \code{1000}
#' @param tau.e Variance of area-by-time effect of variable interest with default \code{1}
#' @param tau.v Variance of random area effect of variable interest with default \code{1}
#' @param data The data frame
#' @export Panel
#' @import stringr
#' @import coda
#' @import rjags
#' @import stats
#' @import grDevices
#' @import graphics
#' @return This function returns a list of the following objects:
#'    \item{Est}{A vector with the values of Small Area mean Estimates using Hierarchical bayesian method}
#'    \item{refVar}{Estimated random effect variances}
#'    \item{coef}{A dataframe with the estimated model coefficient}
#'    \item{plot}{Trace, Density, Autocorrelation Function Plot of MCMC samples}
#'@examples
#' ##For data without any non-sampled area
#' data(dataPanel)     # Load dataset
#' formula = ydi ~ xdi1 + xdi2
#' area = max(dataPanel[, "area"])
#' period = max(dataPanel[,"period"])
#' vardir = dataPanel[,"vardir"]
#'
#' result <- Panel(formula, area, period, vardir, data = dataPanel)
#'
#' result$Est
#' result$refVar
#' result$coef
#' result$plot
#'
#'
#' ## For data with non-sampled area use dataPanelNs
#'

Panel<-function( formula, area, period, vardir, iter.update=3, iter.mcmc=2000,
                 thin = 1, burn.in =1000, tau.e = 1, tau.v=1, data){

  result <- list(Est = NA, refVar = NA, coefficient = NA, plot = NA)
  formuladata <- model.frame(formula, data, na.action = NULL)
  formuladata <- data.frame(vardir, formuladata)
  m = area
  t = period
  n_formuladata <- nrow(formuladata)

  if (any(is.na(formuladata[,-c(1:2)]))){ stop("Auxiliary Variables contains NA values.")
  } else if (iter.update < 3) {stop("the number of iteration updates at least 3 times")
  } else if (n_formuladata!=m*t) stop("length of variabel must be multiply of area and period")
  for (i in 1:n_formuladata) {
    if(!any(is.na(formuladata[i,2])) & any(is.na(formuladata[i, 1])))
      stop("any vardir contain NA when direct estimate are available")
  }

  vardirr = as.matrix(formuladata[,1])
  ydir = as.matrix(formuladata[,2])
  xdir = as.matrix(formuladata[,-c(1:2)])
  if(ncol(formuladata)>3){
    aux = ncol(formuladata[,-c(1:2)])
  }else{
    aux=1
  }
  nvar=aux+1
  y = matrix(0, nrow = m, ncol = t)
  vardir = matrix(0, nrow = m, ncol = t)
  k=0
  for (i in 1:m){
    for(j in 1:t){
      k = k+1
      vardir[i,j] = vardirr[k]
      y[i,j] = ydir[k,1]
    }
  }
  if (!any(is.na(formuladata[,2]))){
    x = list()
    for (i in 1:aux) {
      x[[i]] <- matrix(0, nrow = m, ncol = t)
    }
    kx=0
    for (i in 1:m){
      for(j in 1:t){
        kx = kx+1
        for (h in 1:aux){
          x[[h]][i,j] <- xdir[kx,h]
        }
      }
    }
    x_aux = c()
    for (r in 1:aux) {
      x_aux =  c(x_aux,x[[r]])
    }
    dim(x_aux) = c(m,t,aux)

    mu.b = rep(0, nvar)
    tau.b = rep(1, nvar)
    tau.va=tau.vb=1
    tau.ea=tau.eb=1
    a.var=1

    for(iter in 1:iter.update){
      dat <- list("m"= m, "t"= t, "x" = x_aux, "vardir" = vardir, "y" = y, "nvar"=nvar, "aux"=aux,
                  "mu.b" = mu.b, "tau.b"=tau.b, "tau.va" = tau.va,
                  "tau.vb" = tau.vb, "tau.ea" = tau.ea,"tau.eb" = tau.eb)
      inits <- list(eps = matrix(0,m,t), b = mu.b, tau.v=1, tau.e = 1)
      cat("model {
        for (i in 1:m) {
				    v[i]~dnorm(0,tau.v)
				    for (j in 1:t){
				      y[i,j]~dnorm(mu[i,j],tau[i,j])
				      mu[i,j]<-b[1] + sum(b[2:nvar]*(x[i, j, 1:aux])) + v[i] + eps[i,j]
				      eps[i,j]~dnorm(0,tau.e)
				    }
        }

				#Priors
				for (k in 1:nvar){
				    b[k] ~ dnorm(mu.b[k],tau.b[k])
				}
				tau.v ~ dgamma(tau.va,tau.vb)
        tau.e ~ dgamma(tau.ea,tau.eb)
        a.var<-1/tau.v
        tau <-1/vardir
      }", file = "rao_yu.txt")

      jags.m <- jags.model( file = "rao_yu.txt", data=dat, inits=inits, n.chains=1, n.adapt=500 )
      file.remove("rao_yu.txt")
      params <- c("mu", "a.var", "b", "tau.v", "tau.e")
      samps <- coda.samples( jags.m, params, n.iter = iter.mcmc, thin = thin)
      samps1 <- window(samps, start = burn.in + 1, end = iter.mcmc)
      result_samps=summary(samps1)
      a.var=result_samps$statistics[1]
      beta=result_samps$statistics[2:(nvar+1),1:2]
      for (i in 1:nvar){
        mu.b[i]  = beta[i,1]
        tau.b[i] = 1/(beta[i,2]^2)
      }
      tau.ea = result_samps$statistics[m*t+nvar+2,1]^2/result_samps$statistics[m*t+nvar+2,1]^2
      tau.eb = result_samps$statistics[m*t+nvar+2,1]/result_samps$statistics[m*t+nvar+2,1]^2
      tau.va = result_samps$statistics[m*t+nvar+3,1]^2/result_samps$statistics[m*t+nvar+3,1]^2
      tau.vb = result_samps$statistics[m*t+nvar+3,1]/result_samps$statistics[m*t+nvar+3,1]^2
    }
    result_samps = summary(samps1)
    b.varnames <- list()
    for (i in 1:(nvar)) {
      idx.b.varnames <- as.character(i-1)
      b.varnames[i] <-str_replace_all(paste("b[",idx.b.varnames,"]"),pattern=" ", replacement="")
    }

    result_mcmc <- samps1[,c(2:(nvar+1))]
    colnames(result_mcmc[[1]]) <- b.varnames
    a.var=result_samps$statistics[1]
    beta=result_samps$statistics[2:(nvar+1),1:2]
    rownames(beta) <- b.varnames
    mu=result_samps$statistics[(nvar+2):(1+nvar+(m*t)),1:2]
    Estimation=data.frame(mu)
    rownames(beta) <- b.varnames

    Quantiles <- as.data.frame(result_samps$quantiles)
    q_mu <- Quantiles[(nvar+2):(1+nvar+(m*t)),]
    q_beta <- (Quantiles[2:(nvar+1),])
    beta <- cbind(beta, q_beta)
    Estimation <- data.frame(Estimation,q_mu)
    colnames(Estimation) <- c("MEAN","SD","2.5%","25%","50%","75%","97.5%")
  }else{
    Y = as.matrix(na.omit(y))
    V = na.omit(vardir)
    rowNA <- c()
    a=0
    for (i in 1:m) {
      if(is.na(y[i,1])){
        a = a+1
        rowNA[a] <- i
      }
    }

    x = list()
    for (i in 1:aux) {
      x[[i]] <- matrix(0, nrow = m, ncol = t)
    }
    k=0
    for (j in 1:m) {
      for(l in 1:t){
        k = k+1
        for (i in 1:aux) {
          x[[i]][j,l] <- xdir[k,i]
        }
      }
    }
    x_aux = c()
    for (r in 1:aux) {
      x_aux =  c(x_aux,x[[r]])
    }
    dim(x_aux) = c(m,t,aux)
    NTS = length(rowNA)
    NS = m-NTS
    x_auxS <- x_aux[-rowNA,,]
    dim(x_auxS) = c(NS,t,aux)
    x_auxTS <- x_aux[rowNA,,]
    dim(x_auxTS) = c(NTS,t,aux)

    mu.b = rep(0, nvar)
    tau.b = rep(1, nvar)
    tau.va=tau.vb=1
    tau.ea=tau.eb=1
    a.var=1

    for (iter in 1:iter.update) {
      dat <- list("NS"=NS,"NTS"=NTS,"t"=t, "nvar"=nvar, "aux"=aux, "y"=Y,
                  "xS"=x_auxS, "xTS"=x_auxTS, "vardir"=V, "mu.b"=mu.b,"tau.b"=tau.b,
                  "tau.va"=tau.va, "tau.vb"=tau.vb, "tau.ea"=tau.ea, "tau.eb"=tau.eb)  # names list of numbers
      inits <- list(eps = matrix(0,NS,t), b = mu.b)
      cat("model {
					for (i in 1:NS) {
					v[i]~dnorm(0,tau.v)
					for (j in 1:t){
		  			y[i,j]~dnorm(mu[i,j],tau[i,j])
				  	mu[i,j]<-b[1] + sum(b[2:nvar]*(xS[i, j, 1:aux])) + v[i] + eps[i,j]
					  eps[i,j]~dnorm(0,tau.e)
					 }
					}

					for (i in 1:NTS) {
					  vT[i]~dnorm(0,tau.v)
					  for (j in 1:t){
					    muT[i,j]<-mu.b[1] + sum(mu.b[2:nvar]*(xTS[i, j, 1:aux])) + v[i]+epsT[i,j]
					    epsT[i,j]~dnorm(0,tau.e)
					  }
					}

					#priors
					for (k in 1:nvar){
					    b[k] ~ dnorm(mu.b[k],tau.b[k])
					}
					  tau.e~dgamma(tau.ea,tau.eb)
					  tau.v~dgamma(tau.va,tau.vb)
					  a.var <- 1/tau.v
					  tau <-1/vardir
			  }", file="rao_yu.txt")
      jags.m <- jags.model( file = "rao_yu.txt", data=dat, inits=inits, n.chains=1, n.adapt=500 )
      file.remove("rao_yu.txt")
      params <- c("mu","muT", "a.var", "b", "tau.v", "tau.e")
      samps <- coda.samples( jags.m, params, n.iter = iter.mcmc, thin = thin)
      samps1 <- window(samps, start = burn.in, end = iter.mcmc)
      result_samps=summary(samps1)
      a.var=result_samps$statistics[1]
      beta=result_samps$statistics[2:(nvar+1),1:2]
      for (i in 1:nvar){
        mu.b[i]  = beta[i,1]
        tau.b[i] = 1/(beta[i,2]^2)
      }
      tau.ea = result_samps$statistics[m*t+nvar+2,1]^2/result_samps$statistics[m*t+nvar+2,1]^2
      tau.eb = result_samps$statistics[m*t+nvar+2,1]/result_samps$statistics[m*t+nvar+2,1]^2
      tau.va = result_samps$statistics[m*t+nvar+3,1]^2/result_samps$statistics[m*t+nvar+3,1]^2
      tau.vb = result_samps$statistics[m*t+nvar+3,1]/result_samps$statistics[m*t+nvar+3,1]^2
    }
    result_samps = summary(samps1)
    b.varnames <- list()
    for (i in 1:(nvar)) {
      idx.b.varnames <- as.character(i-1)
      b.varnames[i] <-str_replace_all(paste("b[",idx.b.varnames,"]"),pattern=" ", replacement="")
    }
    result_mcmc <- samps1[,c(2:(nvar+1))]
    colnames(result_mcmc[[1]]) <- b.varnames
    a.var=result_samps$statistics[1]
    beta=result_samps$statistics[2:(nvar+1),1:2]
    rownames(beta) <- b.varnames
    mu=result_samps$statistics[(nvar+2):(NS*t+nvar+1),1:2]
    muT=result_samps$statistics[(NS*t+nvar+2):(m*t+nvar+1),1:2]
    result_s =merge(result_samps$statistics, result_samps$quantiles,by=0)

    mu.start = nvar+2
    muT.end = nrow(data) + mu.start -1
    muT.start = muT.end - NTS*t +1
    result_all = result_s[mu.start:muT.end,-1]
    result_all= data.frame(data,result_all)
    idx.mu = mu.start
    idx.muT = muT.start
    idx = 0
    for(i in 1:m){
      for(j in 1:t){
        idx=idx+1
        if(data[idx,2] %in% rowNA){
          result_all[idx,(ncol(data)+1):ncol(result_all)]<-result_s[idx.muT,-1]
          idx.muT = idx.muT+1
        }else{
          result_all[idx,(ncol(data)+1):ncol(result_all)]<-result_s[idx.mu,-1]
          idx.mu = idx.mu+1
        }
      }
    }
    Mu = result_all[, c("Mean", "SD", "X2.5.", "X25.", "X50.", "X75.", "X97.5.")]
    Estimation = data.frame(Mu)
    colnames(Estimation) <- c("MEAN", "SD", "2.5%", "25%", "50%", "75%", "97.5%")
    rownames(Estimation) <- c(1:(m*t))
    q_beta <- result_samps$quantiles[2:(nvar+1),]
    beta <- cbind(beta,q_beta)

  }
  result$Est = Estimation
  result$refVar = a.var
  result$coefficient = beta
  result$plot = list(graphics.off(), par(mar = c(2, 2, 2, 2)),
                     autocorr.plot(result_mcmc, col = "brown2", lwd = 2),
                     plot(result_mcmc, col = "brown2", lwd = 2))
  return(result)
}
