### Function to carry out univariate survival analysis


uniSurv <- function(s.ob,cov){

if(class(cov)=="numeric") {
	med <- median(cov,na.rm=T)
	cov.fac <- cut(cov,c(-Inf,med,Inf),labels=c("Low","High"))}

if(class(cov)=="factor")  cov.fac <- cov

km <-survfit(s.ob~cov.fac)
sd <- survdiff(s.ob~cov)
cm <- coxph(s.ob~cov)

## Median (95%)
km.tab <- round(data.frame(summary(km)$table),2)
rn <- rownames(km.tab)

nEv <- paste(km.tab$n.start," (",km.tab$events,")",sep="")
med95 <- paste(km.tab$median," (",km.tab$X0.95LCL,", ",km.tab$X0.95UCL,")",sep="")

##
cm.tab <- round(as.data.frame(summary(cm)$conf.int),3)
hr <- paste(cm.tab$"exp(coef)"," (",cm.tab$"lower .95",", ",cm.tab$"upper .95",")",sep="")
hr <- c(NA,hr)

## sd
p <- round(summary(cm)$logtest[3],3)
if(p==0) p <- "< 0.001"
p <- c(rep(NA,length(hr)-1),p)

## result
data.frame("levels"=rn,"N.events"=nEv,"Med.95%CI"=med95,"HR.95%CI"=hr,"P.val"=p)

}



