
### Function for rand by Site table

randBySite <- function(date,site,arm,greenlight=NULL){

	arm <- as.factor(arm)
	tab <- table(site,arm)
	tot <- rowSums(tab)
	
	firstRand <- tapply(date,site,min)
	lastRand <- tapply(date,site,max)
	firstRand <- as.character(as.Date(firstRand,origin="1970-01-01"))
	lastRand <- as.character(as.Date(lastRand,origin="1970-01-01"))
	site.nam <- rownames(tab)
	
	
	randBySite <- as.data.frame(cbind(site.nam,firstRand,lastRand,tab,tot))
	names(randBySite)<-c("Site","firstRand","lastRand",levels(arm),"Total")
	
	if(!is.null(greenlight)){
		gl1 <- tapply(greenlight,site,min)
		gl2 <- tapply(greenlight,site,max)
		if(max(gl2-gl1)!=0) warning("Possible Error in Greenlight dates - earliest date for each site used")
		randBySite$greenlight <- as.character(as.Date(gl1,origin="1970-01-01"))
		randBySite <- randBySite[,c(1,ncol(randBySite),2:(ncol(randBySite)-1))]
	}

	randBySite

}
