#### Cumulative Recruitment Graph


cumRecruitment <- function(date,expected=NULL,exp.bar="FALSE"){
	
	date <- as.Date(substr(date,1,10))
	
	#####
	### monthly recruitment
	## adds on a sudo patient for every month (neccessary for 0's)
	miDt <- as.Date(paste(substr(min(date),1,7),"-15",sep=""))
	maDt <- as.Date(paste(substr(max(date),1,7),"-15",sep=""))
	pseudo.dt <- as.Date(seq(miDt,maDt,by=30.44))
	date <- c(date,pseudo.dt)
	mnth.rec <- table(substr(date,1,7))-1
	cum.exp <- cumsum(expected)
	cum.rec <- cumsum(mnth.rec)
	#####
	
	
	#####
	## Plot
	par(mar=c(5,5,5,4)) 
	barplot(mnth.rec,col=trans("lightblue",75),border=trans("lightblue",75),las=2,ylab="Monthly Recruitment",main="Monthly/Cumulative Recruitment",ylim=c(0,max(mnth.rec)+1))
	par(new=TRUE)
	
	
	yli <- c(0,max(cum.rec))
	if(!is.null(expected)){
		yli <- c(0,max(cum.rec,cum.exp))
	}
	
	
	len <- min(10,max(cum.rec))
	plot(cum.rec,typ="o",col="hotpink",pch=20,lwd=5,cex=1.5,xaxt="n",yaxt="n",bty="u",xlab="",ylab="",xlim=c(0.6,length(mnth.rec)+0.4),ylim=yli)
	mtext(side=4,"Cumulative Recruitment",padj=4)
	axis(side=4,round(seq(0,max(yli),length=len)),round(seq(0,max(yli),length=len)))
	
	
	### adding in exected
	if(!is.null(expected)){
		if(length(cum.exp)>length(cum.rec)) cum.exp <- cum.exp[1:length(cum.rec)]
		lines(cum.exp,typ="o",col="forestgreen",lwd=5)
	}

}
