## Formatiing results from models


### odds Ratio
orRes <- function(est,se){
r1 <- paste(round(est,2)," (",round(se,3),")",sep="")

lo <- est - 1.96*se
hi <- est + 1.96*se

r2 <- paste(round(exp(est),2)," (",round(exp(lo),2),", ",round(exp(hi),2),")",sep="")
ret <- c(r1,r2)
ret

}




expRes <- function(est,se) {
  mn <- round(exp(est),2)
  lo <- round(exp(est - 1.96*se),2)
  up <- round(exp(est + 1.96*se),2)
  paste(mn, "  (", lo, ", ", up, ")",sep="")
}




summCox <- function(mod){
  co.tab <- summary(mod)$coef
  co.tab
  hr <- exp(co.tab[,1])
  up <- exp(co.tab[,1]+1.96*co.tab[,3])
  lo <- exp(co.tab[,1]-1.96*co.tab[,3])
  c1 <- paste(round(co.tab[,1],2)," (",round(co.tab[,3],3),")",sep="")
  c2 <- paste(round(hr,2)," (",round(lo,3),", ",round(up,3),")",sep="")
  c3 <- round(co.tab[,5],3)

  zero.id <- which(c3==0)
  if(length(zero.id)>0) c3[zero.id] <- "<0.001"
  res <- data.frame(cbind(c1,c2,c3))
  names(res) <- c("est (se)","HR (95% CI)","Pval")
  res
}



summLogistic <- function(mod){
co.tab <- summary(mod)$coef

or <- exp(co.tab[,1])
up <- exp(co.tab[,1]+1.96*co.tab[,2])
lo <- exp(co.tab[,1]-1.96*co.tab[,2])

c1 <- paste(round(co.tab[,1],2)," (",round(co.tab[,2],3),")",sep="")
c2 <- paste(round(or,2)," (",round(lo,3),", ",round(up,3),")",sep="")
c3 <- round(co.tab[,4],3)

zero.id <- which(c3==0)
if(length(zero.id)>0) c3[zero.id] <- "<0.001"

res <- data.frame(cbind(c1,c2,c3))
names(res) <- c("est (se)","OR (95% CI)","Pval")

res
}
