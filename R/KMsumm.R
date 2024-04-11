
KMsumm <- function(sfit,level=0.5,rnd=2,event=F){
	
	# Setting up levels
	lev <- levels(summary(sfit)$strata)
	if(is.null(lev)) n.lev <- 1
	if(!is.null(lev)) n.lev <- length(lev)
	
	quant <- quantile(sfit,level)
	
	output <- matrix(NA,n.lev,2)
	output[,1]<- "All"
	for(i in 1:n.lev){
		out.i <- paste(round(quant$quantile[i],rnd)," (",
		round(quant$lower[i],rnd),", ",
		round(quant$upper[i],rnd),")",sep="")
		
		output[i,2] <- out.i
	
	}
	if(n.lev>1) output[,1]<-lev
	
	if(event){
		n <- summary(sfit)$table[,1]
		ne <- summary(sfit)$table[,4]
		ev <- paste(n," (",ne,")",sep="")
		output <- cbind(output[,1],ev,output[,2])
	}
	
	output
}

