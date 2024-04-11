####  Clinical Charcteristics table

## input:	x - data frame of covariates to be summarised
#			by - what they being summarised by


summaryTable <- function(x,by="none",cont.sum="med",se=TRUE,perc=TRUE,missing=FALSE,row=TRUE,test=FALSE,flex=FALSE,nam=colnames(x),...){

  if(by[1]=="none") stop("No 'by'. I will do this bit later - Rich")
  if(class(by)!="factor") by <- as.factor(by)
  if(class(x)=="matrix") x <- as.data.frame(x)

  missing.by <- length(which(is.na(by)))>0

  lev <- levels(by)
	#nam <- names(cov)
  TAB<-table(by,useNA="always")
  TAB<-c("Total","",TAB,sum(TAB))

	if(test) TAB <- c(TAB,"")

  for(i in 1:ncol(x)){


		## reclassifyign character covariates
		if(class(x[,i])=="character"){
			fac.int <- length(unique(x[,i]))/length(x[,i])
			if(fac.int>0.1) {
				x[,i] <- as.numeric(x[,i])
				warning(paste(nam[i],"converted to numeric"))
				}
			if(fac.int<=0.1) {
				x[,i] <- as.factor(x[,i])
				warning(paste(nam[i],"converted to factor"))
				}
		}


  ## Factor variables
  if(class(x[,i])=="factor"){
  x[,i]<-as.factor(as.character(x[,i]))
  tab<- summ.fac(x[,i],by=by,row=row,perc=perc,test=test)
  tab <- cbind(rep(nam[i],nrow(tab)),c(levels(x[,i]),"Missing"),tab)

  if(!missing) tab <- tab[-nrow(tab),]
  TAB<-rbind(TAB,tab)

  }

  ## Continuous Variables
  if(class(x[,i])=="integer"|class(x[,i])=="numeric"){

  if(cont.sum=="med") {
  tab<-tapply(x[,i],by,summ.med)
  tab<-c(nam[i],"median (IQR)",tab,NA, summ.med(x[,i]))
  }

  if(cont.sum=="mean") {
  tab<-tapply(x[,i],by,summ.mean,se=se)
  tab<-c(nam[i],"mean (se)",tab,NA, summ.med(x[,i]))
  }

			if(missing){
  na.tab<-table((is.na(x[,i])|x[,i]=="-Inf"|x[,i]=="Inf"),by)
  na.id<-which(row.names(na.tab)=="TRUE")

				### only add missing column if any observed
  if(length(na.id)>0)	{
					na.tab<-na.tab[na.id,]
  na.tab<-c(na.tab,0,sum(na.tab))
					na.tab <- c(nam[i],"Missing",na.tab)
					tab<-rbind(tab,na.tab)
				}

			}

			if(test){
  na.tab<-table((is.na(x[,i])|x[,i]=="-Inf"|x[,i]=="Inf"),by)
				if(min(na.tab[1,])>15) pval <- round(t.test(x[,i]~by)$p.value,3)
				if(min(na.tab[1,])<=15) pval <- round(wilcox.test(x[,i]~by)$p.value,3)
				tab <- c(tab,pval)
			}

  TAB<-rbind(TAB,tab)
  }


  ## Neither Factor or Continuous
  if(class(x[,i])!="integer"&class(x[,i])!="numeric"&class(x[,i])!="factor"&class(x[,i])!="character"){
			warning(paste(nam[i],"not included due to uncompatible class"))
  }

  }

	### Adding in column names
	cnam <- c("Covariate","Level", lev ,"Missing","Total")
	if(test) cnam <- c(cnam,"p-value")
	colnames(TAB) <- cnam

	### Removing missing column (if required)
	if(!missing|!missing.by) TAB <- TAB[,-which(colnames(TAB)=="Missing")]

	### Setting return object
	ret <- TAB

  ### Formatted flex table (if required)
	if(flex){

		flexTAB <- FlexTable(TAB,...)

		### merging column for factors
		nam.tab <-which(table(TAB[,1])>1)

		for(m in names(nam.tab)){
			row.id <- which(TAB[,1]==m)
			flexTAB <- spanFlexTableRows(flexTAB,1,min(row.id),max(row.id))
		}

		ret <- flexTAB

	}

	ret

}







