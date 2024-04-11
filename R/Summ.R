## colSumm: provides a summary of the columsn of a matrix

### Summary with medians
colSumm <- function(x,method="median"){

	if(method=="median"){
		qu<-round(quantile(x,p=c(0.5,0.025,0.975),na.rm=TRUE),2)
		res<-paste(qu[1],"  (",qu[2],", ",qu[3],")",sep="")
	}

	if(method=="mean"){
		mn<-round(mean(x,na.rm=TRUE),2)
		sd<-round(sd(x,na.rm=TRUE),2)
		paste(mn,"  (",mn - 1.96*sd,", ",mn + 1.96*sd,")",sep="")

	}

	res

}


### Summary with medians
summ.med <- function(x){
	quant<-round(quantile(x,p=c(.5,.25,.75),na.rm=TRUE),3)
	paste(quant[1]," (",quant[2],", ",quant[3],")",sep="")
}

### Summary with means
summ.mean <- function(x,se=TRUE,na.rm=TRUE){

	me <- round(mean(x,na.rm=na.rm),1)
	st.dev <- sd(x,na.rm=na.rm)
	if(se) {
		id <- which(is.na(x))
		if(length(id)>0) st.dev <- st.dev/sqrt(length(x[-id])) else st.dev <- st.dev/sqrt(length(x)) }
	st.dev <- round(st.dev,2)
	paste(me," (",st.dev,")",sep="")
}




### Summ Fac
summ.fac <- function(x,by,perc=TRUE,row=TRUE,test=FALSE){

	### P-value based on Chi-square of Fishers Test as appropriate
	pval <- NA
	if(test){
		np <- table(x,by)
		if(nrow(np)>1){
			if(min(np)>=5) pval <- round(chisq.test(np)$p.value,3)
			if(min(np)<5) pval <- round(fisher.test(np)$p.value,3)
		}
		if(nrow(np)==1) pval <- NA
	}

	### Creating Table
	n<-table(x,by,useNA="always")
	rs <- rowSums(n)
	tab<-cbind(n,rs)


	if(perc){
		if(row){
			p <- round(100*(n/rs),0)
			tab <- matrix(paste(n," (",p,"%)",sep=""),dim(n))
			tab <- cbind(tab,rs)
			}

		if(!row){
			p <- round( 100*(t(t(tab)/colSums(tab))) ,0)
			tab<-matrix(paste(tab," (",p,"%)",sep=""),dim(n))
			}
	}

	if(test) tab <- cbind(tab, c(rep("",(nrow(tab)-2)),pval,"") )
	tab
}




rowMedian <- function(x,na.rm=TRUE){
res <- rep(NA,nrow(x))
for(i in 1:nrow(x)){
res[i] <- median(x[i,],na.rm=na.rm)
}
res
}

colMedian <- function(x,na.rm=TRUE){
res <- rep(NA,ncol(x))
for(i in 1: ncol(x)){
res[i] <- median(x[,i],na.rm=na.rm)
}
res
}


percTab <- function(a,b,margin=2){
tab <-table(a,b)
ptab <-round(prop.table(tab,margin=2)*100);ptab
res <-matrix(paste(tab," (",ptab,"%)",sep=""),dim(tab))
rownames(res)<-rownames(tab)
colnames(res)<-colnames(tab)
res
}



rowMax <- function(x,na.rm=T){
	rm <- rep(NA,nrow(x))
	for(i in 1:nrow(x)){
		rm[i] <- max(as.numeric(x[i,]),na.rm=na.rm)
	}
	rm
}

rowMin <- function(x,na.rm=T){
	rm <- rep(NA,nrow(x))
	for(i in 1:nrow(x)){
		rm[i] <- min(as.numeric(x[i,]),na.rm=na.rm)
	}
	rm
}



summNorm <- function(mod){

	co.tab <- summary(mod)$coef
	co.tab
	c1 <- paste(round(co.tab[,1],2)," (",round(co.tab[,2],3),")",sep="")
	c3 <- round(co.tab[,4],3)

	zero.id <- which(c3==0)
	if(length(zero.id)>0) c3[zero.id] <- "<0.001"

	res <- data.frame(cbind(c1,c3))
	names(res) <- c("est (se)","Pval")
	res
}

summGLM <- function(mod){

	## getting family
	fm <- mod$family$family

	if(fm=="gaussian") res <- summNorm(mod)
	if(fm=="binomial") res <- summLogistic(mod)

	res
}

