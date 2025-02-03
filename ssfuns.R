rm(list=ls())
library(parallel)
if (!require("extraDistr")) 
  install.packages("extraDistr")

library(extraDistr)

nclus <- max(1,detectCores()-4) 


gen<-function(n,k,rho, R, nsim=1e4){
  sd.2.e <- 1 
  sd.2.r <- R
  sd.2.s <- (R+1)/(1/rho-1)
  
  MSS <- rchisq(nsim, df=n-1)*(k*sd.2.s+sd.2.e)/(n-1)
  MSR <- rchisq(nsim, df=k-1)*(n*sd.2.r+sd.2.e)/(k-1)
  MSE <- rchisq(nsim, df=(n-1)*(k-1))*sd.2.e/((n-1)*(k-1))
  
  rho <- sapply(1:nsim, function(x)
    (MSS[x] - MSE[x])/
      (MSS[x] + (k/n)*MSR[x] + (k-1-k/n)*MSE[x]))
  
  return(list("ICC.agree"=rho,
              "MS.subj"  =MSS,
              "MS.ratr"  =MSR,
              "MS.errr"  =MSE
  ))
}

data_gen<-function(n,k,rho, R){
  
  sd.2.rater <- R
  sd.2.subject <- (R+1)/(1/rho-1)
  
  
  e_ij <- matrix(rnorm(n*k, sd = 1), nrow=n, ncol=k)            ## errors
  r_j <- matrix(rep(rnorm(k, sd=sqrt(sd.2.rater)),n), nrow=n, byrow =TRUE) ## rater effects
  s_i <- matrix(rep(rnorm(n, sd=sqrt(sd.2.subject)),k), nrow=n) ## subject effects
  return(s_i + r_j + e_ij)
}

ci.MLSA <- function(MSS, MSR, MSE, n, k, alpha = 0.05) {
  d2 <- k/n
  d3 <- k-1-k/n
  
  F1 <- MSS/MSE
  F2 <- MSR/MSE
  
  
  F.n <- function(alpha){qf(alpha, n-1, Inf)}
  F.nk <- function(alpha){qf(alpha, n-1, k-1)}
  F.nnk <- function(alpha){qf(alpha, n-1, (n-1)*(k-1))}
  
  A<- function(alpha){
    (-1+1/F.n(alpha)*F1 + (1-1/F.n(alpha)*F.nnk(alpha))* F.nnk(alpha)/F1)/(n-1 + 1/F.n(alpha)*F.nk(alpha)*F2)
  }
  
  L <- n*max(0,A(1-alpha))/(k+n*max(0,A(1-alpha)))
  U <- 1
  
  return(c("Lower"=L,            
           "Upper"=U))
}

ci.MLSG <- function(MSS, MSR, MSE, n, k, alpha = 0.05){
  d2 <- k/n
  d3 <- k-1-k/n
  
  # calculate quantities in the Appendix of Cappelleri & Ting
  H1 <- (1/qf(alpha, n-1, Inf))-1
  H2 <- (1/qf(alpha, k-1, Inf))-1
  H3 <- (1/qf(alpha, (n-1)*(k-1), Inf))-1
  G1 <- 1-(1/qf(1-alpha, n-1, Inf))
  G2 <- 1-(1/qf(1-alpha, k-1, Inf))
  G3 <- 1-(1/qf(1-alpha, (n-1)*(k-1), Inf))
  H12 <- ((1-qf(alpha, n-1, k-1))^2 - (H1*qf(alpha, n-1, k-1))^2 - G2^2)/(qf(alpha, n-1, k-1))
  H13 <- ((1-qf(alpha, n-1, (n-1)*(k-1)))^2 - (H1*qf(alpha, n-1, (n-1)*(k-1)))^2 - G3^2)/(qf(alpha, n-1, (n-1)*(k-1)))
  G12 <- ((qf(1-alpha, n-1, k-1)-1)^2 - (G1*qf(1-alpha, n-1, k-1))^2 - H2^2)/(qf(1-alpha, n-1, k-1))
  G13 <- ((qf(1-alpha, n-1, (n-1)*(k-1))-1)^2 - (G1*qf(1-alpha, n-1, (n-1)*(k-1)))^2 - H3^2)/(qf(1-alpha, n-1, (n-1)*(k-1)))
  
  # Upper Limit = 1
  U <- 1
  
  # 100(1-alpha) percent lower confidence limit L
  Al <- (1-G1^2)*(MSS^2) + (1-H2^2)*(d2^2)*(MSR^2) + (1-H3^2)*(d3^2)*(MSE^2) + (2+G12)*d2*MSS*MSR + (2+G13)*d3*MSS*MSE + 2*d2*d3*MSR*MSE
  Bl <- (-2)*(1-G1^2)*(MSS^2) + 2*(1-H3^2)*d3*(MSE^2) - (2+G12)*d2*MSS*MSR - (2+G13)*(d3-1)*MSS*MSE + 2*d2*MSR*MSE
  Cl <- (1-G1^2)*(MSS^2) + (1-H3^2)*(MSE^2) - (2+G13)*MSS*MSE
  Q2 <- max(0, (Bl^2-4*Al*Cl))
  L <- (-Bl - sqrt(Q2))/(2*Al)
  
  return(c("Lower"=L,
           "Upper"=U))
  
}

ci.ASN <- function(MSS, MSR, MSE, n, k, alpha=0.05){
  rho <- (MSS - MSE)/(MSS + (k/n)*MSR + (k-1-k/n)*MSE)
  sd.2.r <- 1/n*(MSR-MSE)
  sd.2.s <- 1/k*(MSS-MSE)
  sd.2.e <- MSE
  u <- sd.2.r/sd.2.s
  
  sig <- sqrt(2*rho^4*((1/rho-1)^2+n/k*u^2))
  L <- rho-qnorm(1-alpha/2)*sig/sqrt(n)
  U <- rho+qnorm(1-alpha/2)*sig/sqrt(n)
  return(c("Lower"=L,
           "Upper"=U))
}

ci.VPF <- function(MSS, MSR, MSE, n, k, alpha = 0.05){
  
  sd.2.s.est <- 1/k*(MSS - MSE)
  sd.2.r.est <- 1/n*(MSR - MSE)
  sd.2.e.est <- MSE
  
  sig.2.G <- sd.2.s.est
  sig.2.E <- sd.2.r.est + sd.2.e.est
  sig.2.T <- sd.2.s.est + sd.2.r.est + sd.2.e.est
  
  tau.2.G <- 2/k^2*((k*sd.2.s.est+sd.2.e.est)^2/(n-1)+sd.2.e.est^2/((n-1)*(k-1)))
  tau.2.E <- 2/n^2*((n*sd.2.r.est+sd.2.e.est)^2/(k-1)+sd.2.e.est^2/((n-1)*(k-1)))+
    2*sd.2.e.est^2/((n-1)*(k-1))
  
  tau.2.ICC <- sig.2.E^2/sig.2.T^4*tau.2.G +
    sig.2.G^2/sig.2.T^4*tau.2.E
  
  df.G <- max(1,2*sig.2.G^2/tau.2.G)
  df.E <- 2*sig.2.E^2/tau.2.E
  
  L <- sig.2.G*qf(p=alpha, df1=df.G, df2=df.E)/
    (sig.2.G*qf(p=alpha, df1=df.G, df2=df.E) + sig.2.E)
  return(c("Lower"=L,            
           "Upper"=1))
}

ci.VPB <- function(MSS, MSR, MSE, n, k, alpha = 0.05){
  sd.2.s.est <- 1/k*(MSS - MSE)
  sd.2.r.est <- 1/n*(MSR - MSE)
  sd.2.e.est <- MSE
  
  v.mse      <- 2*sd.2.e.est^2/((k-1)*(n-1))
  v.mss      <- 2*(k*sd.2.s.est+sd.2.e.est)^2/(n-1)
  v.msr      <- 2*(n*sd.2.r.est+sd.2.e.est)^2/(k-1)
  
  v.sd.2.s.est <- 1/k^2*(v.mss + v.mse)
  v.sd.2.r.est <- 1/n^2*(v.msr + v.mse)
  
  
  cov.s.r.est <- 1/(k*n)*v.mse
  cov.s.e.est <- -1/k*v.mse
  cov.r.e.est <- -1/n*v.mse
  
  sd.2.E      <- (sd.2.r.est + sd.2.e.est)
  sd.2.T      <- (sd.2.s.est + sd.2.r.est + sd.2.e.est)
  
  v.sd.2.E    <- v.sd.2.r.est + v.mse + 2*cov.r.e.est
  cov.s.E     <- cov.s.r.est + cov.s.e.est
  
  tau.ICC <- sd.2.E^2/sd.2.T^4*v.sd.2.s.est+
    sd.2.s.est^2/sd.2.T^4*v.sd.2.E-
    2*sd.2.s.est*sd.2.E/sd.2.T^4*cov.s.E
  
  mu <- (MSS - MSE)/(MSS + (k/n)*MSR + (k-1-k/n)*MSE)
  
  a = mu*(mu*(1-mu)-tau.ICC)/tau.ICC
  b = (1-mu)*(mu*(1-mu)-tau.ICC)/tau.ICC
  
  if(a<0){
    b = 1
    a = mu/(1-mu)
  }else if(b<0){
    a = 1
    b = mu/(1-mu)
  }
  
  if(a<1 & b<1){
    if(mu<0.5){
      b=1
    }else{
      a=1
    }
  }
  a <- max(0.01, a)
  b <- max(0.01, b)
  L = qbeta(p=alpha, shape1 = a, shape2=b)
  
  return(c("Lower"=L,            
           "Upper"=1))
  
}

ci.delta.mat <- function(MSS, MSR, MSE, n, k, alpha = 0.05){
  
  icc.agree <- (MSS - MSE)/(MSS + (k/n)*MSR + (k-1-k/n)*MSE)
  
  sd.2.s <- 1/k*(MSS-MSE)
  sd.2.r <- 1/n*(MSR-MSE)
  sd.2.e <- MSE
  
  var.s.2 <- 2/(k^2*(n-1))*((k*sd.2.s+sd.2.e)^2 + sd.2.e^2/(k-1))
  var.r.2 <- 2/(n^2*(k-1))*((n*sd.2.r+sd.2.e)^2 + sd.2.e^2/(n-1))
  var.e.2 <- 2*sd.2.e^2/((n-1)*(k-1))
  
  ydyd      <- (k-1)/n*MSR
  s11       <- k*MSS
  trs       <- MSS+ (k-1)*MSE
  trs2      <- (n-1)^2*k^2/(2*n)*(var.s.2+ var.e.2 - 2/k*var.e.2)
  s211      <- (n-1)^2*k^2/(2*n)*(k*var.s.2+ var.e.2 - (k+1)/k*var.e.2)
  
  elem1 <- (1+(k-1-k/n)*icc.agree)^2*trs2
  elem2 <- (1-icc.agree/n)^2*s11^2
  elem3 <- 2*(1+(k-1-k/n)*icc.agree)*(1-icc.agree/n)*s211
  elem4 <- 4*k*icc.agree^2*ydyd*(n+1)
  elem5 <- (n+1)*(s11-trs)^2
  
  SE.2.ICC <- 2*icc.agree^2*(elem1+elem2-elem3+elem4)/elem5
  L = icc.agree-qt(p=1-alpha,n+1)*sqrt(SE.2.ICC)
  return(c("Lower"=L,            
           "Upper"=1))
  
}

ci.delta.matrix <- function(data,      ## A 2d matrix with columns representing raters and rows representing subjects
                            alpha=0.05
){
  
  k <- ncol(data)
  n <- nrow(data)
  
  vec1 <- integer(k)+1
  vmat <- cov(data)
  num<-t(vec1)%*%vmat%*%vec1-sum(diag(vmat))
  den <- 1/n*t(vec1)%*%vmat%*%vec1+(n*k-n-k)/n*sum(diag(vmat))+
    k*sum((colMeans(data)-mean(data))^2)
  icc.agree <- num/den
  
  
  s211.g2 <<- t(vec1)%*%(vmat%*%vmat)%*%vec1
  trs2.g2 <<- sum(diag(vmat%*%vmat))
  
  elem1 <- (1+(k-1-k/n)*icc.agree)^2*sum(diag(vmat%*%vmat))
  elem2 <- (1-icc.agree/n)^2*(t(vec1)%*%vmat%*%vec1)^2
  elem3 <- 2*(1+(k-1-k/n)*icc.agree)*(1-icc.agree/n)*
    (t(vec1)%*%(vmat%*%vmat)%*%vec1)
  elem4 <- 4*k*icc.agree^2*(sum((colMeans(data)-mean(data))^2)^2)*(n+1)
  elem5 <- (n+1)*(t(vec1)%*%vmat%*%vec1-sum(diag(vmat)))^2
  
  
  SE.2.ICC <- 2*icc.agree^2*(elem1+elem2-elem3+elem4)/elem5
  L = icc.agree-qt(p=1-alpha,n+1)*sqrt(SE.2.ICC)
  return(c("Lower"=L,            
           "Upper"=1))
  
}

ci.GCI <- function(MSS, MSR, MSE, n, k, alpha = 0.05, MC=1e5) {
  set.seed(0)
  
  W11 <- rchisq(MC,df=k-1)
  W21 <- rchisq(MC,df=n-1)
  W31 <- rchisq(MC,df=(n-1)*(k-1))
  
  num <- pmax(0, (n - 1) * MSS / (k * W21) - (n*k-n-k+1)*MSE/(k*W31))
  den <- (k - 1) * MSR/(n*W11) + (n-1)*MSS/(k*W21) + (n*k-n-k)*(n*k-n-k+1)*MSE/(n*k*W31)
  
  L <- unname(quantile(num/den,alpha, na.rm=TRUE))
  
  return(c("Lower"=L,
           "Upper"=1))
  
}

pow.GCI <- function(n,k,rho,R,rho.0, alpha=0.05, nsim=100, nsimW=100, seed=2) {
  
  set.seed(seed);
  
  sd.2.e <- 1
  sd.2.r <- R
  sd.2.s <- (R+1)/(1/rho-1)
  
  lam1.2 <- sd.2.e + n*sd.2.r
  lam2.2 <- sd.2.e + k*sd.2.s
  
  MSS <- rchisq(nsim, df=n-1)*(k*sd.2.s+sd.2.e)/(n-1)
  MSR <- rchisq(nsim, df=k-1)*(n*sd.2.r+sd.2.e)/(k-1)
  MSE <- rchisq(nsim, df=(n-1)*(k-1))*sd.2.e/((n-1)*(k-1))
  
  a <- (n-1)/k*MSS
  b <- (n-1)*(k-1)/k*MSE
  c <- (k-1)/n*MSR
  d <- b*(k-1-k/n)
  
  QRB <- function(alpha, M1, M2, M3, a, b, c, d, k){
    RB <- function(g0) {
      inputvals <- (pmax(M2 * a - M3 * b, 0) - M2 * a * g0 - M3 * d * g0) / (c * g0)
      fval <- 1 - sapply(inputvals, function(x){
        ifelse(is.na(x)|x<=0, 0,pinvchisq(x, (k - 1)))
      })
      return(mean(fval))
    }
    
    fun <- function(x){RB(x)-alpha}
    rt  <- ifelse(fun(1e-6)>=0,0,
                  uniroot(fun, lower=1e-6, upper=1-1e-6)$root)
    return(rt)
  }
  
  Lw <- sapply(1:nsim, function(x){
    M1 <- 1/rchisq(nsimW, k-1)
    M2 <- 1/rchisq(nsimW, n-1)
    M3 <- 1/rchisq(nsimW, (n-1)*(k-1))
    return(QRB(alpha,M1,M2,M3,a[x],b[x],c[x],d[x],k))
  })
  
  return(mean(Lw>rho.0))
}


pow.Wmat <- function(n,k,rho,R,rho.0,alpha=0.05){
  sd.2.e <- 1
  sd.2.r <- R
  sd.2.s <- (R+1)/(1/rho-1)
  
  var.s.2 <- 2/(k^2*(n-1))*((k*sd.2.s+sd.2.e)^2 + sd.2.e^2/(k-1))
  var.r.2 <- 2/(n^2*(k-1))*((n*sd.2.r+sd.2.e)^2 + sd.2.e^2/(n-1))
  var.e.2 <- 2*sd.2.e^2/((n-1)*(k-1))
  
  trS2 <- (n-1)^2*k^2/(2*n)*(var.s.2+ var.e.2 - 1/k*var.e.2)
  trS  <- k*(sd.2.s+sd.2.e)
  S11  <- k^2*sd.2.s + k*sd.2.e
  S211 <- (n-1)^2*k^2/(2*n)*(k*var.s.2+ var.e.2 - (k+1)/k*var.e.2)
  ydd  <- (k-1)/n*(n*sd.2.r+sd.2.e)
  
  elem1 <- (1+(k-1-k/n)*rho)^2*trS2
  elem2 <- (1-rho/n)^2*S11^2
  elem3 <- 2*(1+(k-1-k/n)*rho)*(1-rho/n)*S211
  elem4 <- 4*k*rho^2*(n+1)*ydd
  elem5 <- (n+1)*(S11-trS)^2
  
  SE.2.ICC <- 2*rho^2*(elem1+elem2-elem3+elem4)/elem5
  val <- (rho-rho.0)/sqrt(SE.2.ICC) - qt(1-alpha, n+1)
  return(pt(val, n+1))
}

pow.VPF <- function(n, k, rho, R, rho.0, alpha = 0.05){
  sd.2.e <- 1
  sd.2.r <- R
  sd.2.s <- (R+1)/(1/rho-1)
  
  sig.2.G <- sd.2.s
  sig.2.E <- sd.2.r + sd.2.e
  
  tau.2.G <- 2/k^2*((k*sd.2.s+sd.2.e)^2/(n-1)+sd.2.e^2/((n-1)*(k-1)))
  tau.2.E <- 2/n^2*((n*sd.2.r+sd.2.e)^2/(k-1)+sd.2.e^2/((n-1)*(k-1)))+
    2*sd.2.e^2/((n-1)*(k-1))
  df.G <- max(1,2*sig.2.G^2/tau.2.G)
  df.E <- 2*sig.2.E^2/tau.2.E
  
  tau <- function(x){1/x -1}
  return(pf(tau(rho.0)/tau(rho)*qf(alpha,df.G, df.E), df.E, df.G))
  
  
}

pow.VPB <- function(n, k, rho, R, rho.0, nsim=1e4, alpha = 0.05, seed=0){
  
  set.seed(seed)
  sd.2.e <- 1 
  sd.2.r <- R
  sd.2.s <- (R+1)/(1/rho-1)
  
  MSS <- rchisq(nsim, df=n-1)*(k*sd.2.s+sd.2.e)/(n-1)
  MSR <- rchisq(nsim, df=k-1)*(n*sd.2.r+sd.2.e)/(k-1)
  MSE <- rchisq(nsim, df=(n-1)*(k-1))*sd.2.e/((n-1)*(k-1))
  
  cond.a.b<- function(a,b){
    if(a<0){
      b = 1
      a = rho/(1-rho)
    }else if(b<0){
      a = 1
      b = rho/(1-rho)
    }
    
    if(a<1 & b<1){
      if(rho<0.5){
        b=1
      }else{
        a=1
      }
    }
    a <- max(0.01, a)
    b <- max(0.01, b)
    return(c(a,b))
  }
  
  Ls <- sapply(1:nsim, function(x){
    sd.2.s.est <- 1/k*(MSS[x] - MSE[x])
    sd.2.r.est <- 1/n*(MSR[x] - MSE[x])
    sd.2.e.est <- MSE[x]
    
    v.mse      <- 2*sd.2.e.est^2/((k-1)*(n-1))
    v.mss      <- 2*(k*sd.2.s.est+sd.2.e.est)^2/(n-1)
    v.msr      <- 2*(n*sd.2.r.est+sd.2.e.est)^2/(k-1)
    
    v.sd.2.s.est <- 1/k^2*(v.mss + v.mse)
    v.sd.2.r.est <- 1/n^2*(v.msr + v.mse)
    
    
    cov.s.r.est <- 1/(k*n)*v.mse
    cov.s.e.est <- -1/k*v.mse
    cov.r.e.est <- -1/n*v.mse
    
    sd.2.E      <- (sd.2.r.est + sd.2.e.est)
    sd.2.T      <- (sd.2.s.est + sd.2.r.est + sd.2.e.est)
    
    v.sd.2.E    <- v.sd.2.r.est + v.mse + 2*cov.r.e.est
    cov.s.E     <- cov.s.r.est + cov.s.e.est
    
    tau.ICC <- sd.2.E^2/sd.2.T^4*v.sd.2.s.est+
      sd.2.s.est^2/sd.2.T^4*v.sd.2.E-
      2*sd.2.s.est*sd.2.E/sd.2.T^4*cov.s.E
    
    mu <- (MSS[x] - MSE[x])/(MSS[x] + (k/n)*MSR[x] + (k-1-k/n)*MSE[x])
    a = mu*(mu*(1-mu)-tau.ICC)/tau.ICC
    b = (1-mu)*(mu*(1-mu)-tau.ICC)/tau.ICC
    ab = cond.a.b(a,b)
    return(qbeta(p=alpha, shape1 = ab[1], shape2 = ab[2]))
  })
  return(mean(Ls>rho.0))
}

pow.VPB2 <- function(n, k, rho, R, rho.0, nsim=1e4, alpha = 0.05, seed=0){
  set.seed(seed)
  gdt <- gen(n=n, k=k, rho=rho, R=R, nsim = nsim)
  Ls <- sapply(1:nsim, function(x){
    ci.VPB(MSS= gdt$MS.subj[x],
           MSR= gdt$MS.ratr[x],
           MSE= gdt$MS.errr[x],
           n  = n,
           k  = k,
           alpha = alpha)[["Lower"]]
  })
  power = mean(Ls>rho.0)
  return(power)
}

pow.MLSG2 <- function(n, k, rho, R, rho.0, nsim=1e4, alpha = 0.05, seed= 0){
  set.seed(seed)
  gdt <- gen(n=n, k=k, rho=rho, R=R, nsim = nsim)
  Ls <- sapply(1:nsim, function(x){
    ci.MLSG(MSS= gdt$MS.subj[x],
            MSR= gdt$MS.ratr[x],
            MSE= gdt$MS.errr[x],
            n  = n,
            k  = k,
            alpha = alpha)[["Lower"]]
  })
  power = mean(Ls>rho.0)
  return(power)
}

pow.MLSA <- function(n, k, rho, R, rho.0, nsim =1e4, alpha = 0.05, seed=0){
  set.seed(seed)
  sd.2.e <- 1
  sd.2.r <- R
  sd.2.s <- (R+1)/(1/rho-1)
  
  MSS <- rchisq(nsim, df=n-1)*(k*sd.2.s+sd.2.e)/(n-1)
  MSR <- rchisq(nsim, df=k-1)*(n*sd.2.r+sd.2.e)/(k-1)
  MSE <- rchisq(nsim, df=(n-1)*(k-1))*sd.2.e/((n-1)*(k-1))
  
  d2 <- k/n
  d3 <- k-1-k/n
  
  
  F.n <- function(alpha){qf(alpha, n-1, Inf)}
  F.nk <- function(alpha){qf(alpha, n-1, k-1)}
  F.nnk <- function(alpha){qf(alpha, n-1, (n-1)*(k-1))}
  A<- function(alpha, F1,F2){
    (-1+1/F.n(alpha)*F1 + (1-1/F.n(alpha)*F.nnk(alpha))* F.nnk(alpha)/F1)/(n-1 + 1/F.n(alpha)*F.nk(alpha)*F2)
  }
  
  L <- sapply(1:nsim, function(x){
    F1 <- MSS[x]/MSE[x]
    F2 <- MSR[x]/MSE[x]
    return(n*max(0,A(1-alpha, F1, F2))/(k+n*max(0,A(1-alpha, F1, F2))))
  })
  return("Power"=mean(L>rho.0))
}

pow.MLSG <- function(n, k, rho, R, rho.0, nsim =1e4, alpha = 0.05, seed=0){
  set.seed(seed)
  sd.2.e <- 1
  sd.2.r <- R
  sd.2.s <- (R+1)/(1/rho-1)
  
  MSS <- rchisq(nsim, df=n-1)*(k*sd.2.s+sd.2.e)/(n-1)
  MSR <- rchisq(nsim, df=k-1)*(n*sd.2.r+sd.2.e)/(k-1)
  MSE <- rchisq(nsim, df=(n-1)*(k-1))*sd.2.e/((n-1)*(k-1))
  
  d2 <- k/n
  d3 <- k-1-k/n
  
  
  # calculate quantities in the Appendix of Cappelleri & Ting
  H1 <- (1/qf(alpha, n-1, Inf))-1
  H2 <- (1/qf(alpha, k-1, Inf))-1
  H3 <- (1/qf(alpha, (n-1)*(k-1), Inf))-1
  G1 <- 1-(1/qf(1-alpha, n-1, Inf))
  G2 <- 1-(1/qf(1-alpha, k-1, Inf))
  G3 <- 1-(1/qf(1-alpha, (n-1)*(k-1), Inf))
  H12 <- ((1-qf(alpha, n-1, k-1))^2 - (H1*qf(alpha, n-1, k-1))^2 - G2^2)/(qf(alpha, n-1, k-1))
  H13 <- ((1-qf(alpha, n-1, (n-1)*(k-1)))^2 - (H1*qf(alpha, n-1, (n-1)*(k-1)))^2 - G3^2)/(qf(alpha, n-1, (n-1)*(k-1)))
  G12 <- ((qf(1-alpha, n-1, k-1)-1)^2 - (G1*qf(1-alpha, n-1, k-1))^2 - H2^2)/(qf(1-alpha, n-1, k-1))
  G13 <- ((qf(1-alpha, n-1, (n-1)*(k-1))-1)^2 - (G1*qf(1-alpha, n-1, (n-1)*(k-1)))^2 - H3^2)/(qf(1-alpha, n-1, (n-1)*(k-1)))
  
  # Upper Limit = 1
  U <- 1
  
  # 100(1-alpha) percent lower confidence limit L
  
  L <- sapply(1:nsim, function(x){
    Al <- (1-G1^2)*(MSS[x]^2) + (1-H2^2)*(d2^2)*(MSR[x]^2) + (1-H3^2)*(d3^2)*(MSE[x]^2) + 
      (2+G12)*d2*MSS[x]*MSR[x] + (2+G13)*d3*MSS[x]*MSE[x] + 2*d2*d3*MSR[x]*MSE[x]
    Bl <- (-2)*(1-G1^2)*(MSS[x]^2) + 2*(1-H3^2)*d3*(MSE[x]^2) - (2+G12)*d2*MSS[x]*MSR[x] - 
      (2+G13)*(d3-1)*MSS[x]*MSE[x] + 2*d2*MSR[x]*MSE[x]
    Cl <- (1-G1^2)*(MSS[x]^2) + (1-H3^2)*(MSE[x]^2) - (2+G13)*MSS[x]*MSE[x]
    Q2 <- max(0, (Bl^2-4*Al*Cl))
    return( (-Bl - sqrt(Q2))/(2*Al))
  })
  
  
  return("Power"=mean(L>rho.0))
  
}

bisection <- function(f, a, b,
                      tol=1e-5,
                      max.iter=1e4){
  
  if(f(a) * f(b) > 0){
    stop("Functional values at endpoints not of opposite signs.")
  }
  
  vals.search <- c()
  fvals.search <- c()
  
  for(i in 1:max.iter){
    d <- as.integer((a + b)/2)
    val.it <- f(d)
    #cat("Values: n=", d, "f(n)=", val.it, "bisection", "\n")
    
    vals.search <- c(vals.search, d)
    fvals.search <- c(fvals.search, val.it)
    
    if(length(vals.search)>2 & tail(vals.search,1)==tail(vals.search,2)[1]){
      if(tail(fvals.search, 1) > 0){
        op<-tail(vals.search,1)
      }else{
        #dvals <- vals.search - tail(vals.search, 1)
        op <- tail(vals.search,1)+1
      }
      break
    }
    
    if (abs(b - a) < 1) {
      op <- d
      break
    }else if(val.it>=0 & val.it < tol){
      op <- d
      break
    }
    
    if (f(a) * f(d) < 0) {
      b <- d
    } else {
      a <- d
    }
  }
  
  return(list("Search" = vals.search,
              "Search.vals" = fvals.search,
              "final" = op,
              "final.val" = f(op)))
}



SampleSize <- function(k, 
                       rho, 
                       R, 
                       rho.0, 
                       alpha = 0.05,
                       power=0.8, 
                       nsim=1e4, 
                       seed =0, 
                       method= "MLSG", 
                       n_max=1e3, 
                       nsimW=100,
                       n_min=4){
  
  fun.pow = NULL
  
  if(method=="delta.mat"|method=="Wmat"){
    fun.pow <- function(x){pow.Wmat(n=x,
                                    k = k,
                                    rho = rho,
                                    R = R,
                                    rho.0 = rho.0,
                                    alpha= alpha)-power}
  }else if(method=="VPF"){
    fun.pow <- function(x){pow.VPF(n=x,
                                   k = k,
                                   rho = rho,
                                   R = R,
                                   rho.0 = rho.0,
                                   alpha= alpha)-power}
  }else if(method=="VPB"){
    fun.pow <- function(x){pow.VPB(n=x,
                                   k = k,
                                   rho = rho,
                                   R = R,
                                   rho.0 = rho.0,
                                   nsim = nsim,
                                   seed = seed,
                                   alpha= alpha)-power}
  }else if(method=="MLSG"){
    fun.pow <- function(x){pow.MLSG(n=x,
                                    k = k,
                                    rho = rho,
                                    R = R,
                                    rho.0 = rho.0,
                                    nsim = nsim,
                                    seed = seed,
                                    alpha= alpha)-power}
  }else if(method=="MLSA"){
    fun.pow <- function(x){pow.MLSA(n=x,
                                    k = k,
                                    rho = rho,
                                    R = R,
                                    rho.0 = rho.0,
                                    nsim = nsim,
                                    seed = seed,
                                    alpha= alpha)-power}
    
  }else if(method=="GCI"){
    fun.pow <- function(x){pow.GCI(n=x,
                                   k = k,
                                   rho = rho,
                                   R = R,
                                   rho.0 = rho.0,
                                   nsim = nsim,
                                   nsimW = nsimW,
                                   seed = seed,
                                   alpha= alpha)-power}
  }

  bis <-tryCatch({
    bisection(f = fun.pow,
              a = n_min,
              b = n_max
    )
    
  }, error = function(e) {
    if(grepl("Functional values at endpoints not of opposite signs.", e$message)){
      fpw_min <- fun.pow(n_min)
      
      return(list("Search" = ifelse(fpw_min>0,n_min,n_max),
                  "Search.vals" = NA,
                  "final" = ifelse(fpw_min>0,n_min,n_max),
                  "final.val" = ifelse(fpw_min>0,fpw_min+power,fun.pow(n_max)+power)))  
    }
  })
  
  return(list("method"   = method,
              "k" = k,
              "rho.A"=rho,
              "rho.0"=rho.0,
              "R" = R,
              "bisection"=bis))
}

MC_SimL <- function(n, k, rho, R, method="MLSG", nsim=1e4, alpha=0.05, seed=0){
  st <- Sys.time()
  set.seed(seed)
  
  g <- gen(n=n,k=k,rho=rho,R=R,nsim = nsim)
  cis <- c()
  if(method=="Wmat"|method=="delta.mat"|method=="delta.matrix"){
    cis <- sapply(1:nsim, function(x){
      set.seed(x+seed-1)
      dat <- data_gen(n =n, k =k, rho=rho, R= R)
      return(ci.delta.matrix(data=dat, alpha=alpha)[['Lower']])
    })
  }else{
    fun.ci <- eval(parse(text=paste0("ci.",method)))
    
    cis<- sapply(1:nsim, function(x){
      fun.ci(MSS = g$MS.subj[[x]],
             MSR = g$MS.ratr[[x]],
             MSE = g$MS.errr[[x]],
             n = n,
             k = k,
             alpha = alpha)[['Lower']]
    })
  }
  return(cis)
}

pow_cal <- function(Lows, rho.0){
  return(mean(Lows>rho.0))
}

SampleSize.wrap<- function(k, 
                           rho, 
                           R, 
                           rho.0, 
                           alpha = 0.05,
                           nsim=1e4, 
                           power=0.8, 
                           seed =0, 
                           method= "MLSG", 
                           n_max=1e3, 
                           nsimW=100,
                           n_min=4,
                           reps =10){
  cl <- makeCluster(nclus, outfile= 'temp.txt')
  
  
  clusterExport(cl, list("k",
                         "rho",
                         "rho.0",
                         "R",
                         "alpha",
                         "nsim",
                         "nsimW",
                         "seed",
                         "method",
                         "power",
                         "n_min",
                         "n_max",
                         "bisection",
                         "pow.VPF",
                         "pow.Wmat",
                         "pow.MLSA",
                         "pow.MLSG",
                         "pow.GCI",
                         "pow.VPB",
                         "SampleSize",
                         "pinvchisq"),
                envir = environment())

  ss <- parSapply(cl, 1:reps, function(x){
    SampleSize(k      = k, 
               rho    = rho, 
               R      = R, 
               rho.0  = rho.0, 
               alpha  = alpha,
               nsim   = nsim, 
               power  = power, 
               seed   = seed+x, 
               method = method, 
               n_max  = n_max, 
               nsimW  = nsimW,
               n_min  = n_min)[['bisection']][['final']]

  })
  on.exit(stopCluster(cl), add = TRUE)
  return(ss)
}

