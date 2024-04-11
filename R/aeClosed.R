### Closed table of AEs for DMC

## input:	ID - Patient id's
#			Cat - Category of AEs
#			AE - Toxicities
#			Grade - Grade
#			Arm - Arm


aeClosed <- function(ID,Cat,AE,Grade,Arm){


### Creating Dataset for analysis
data<-data.frame("PatID"=ID,"Cat"=Cat,"AE"=AE,"Grade"=Grade,"Arm"=Arm)	

##### Error Trapping #####
### NA for AE grades
na.id<-which(is.na(data$Grade))
if(length(na.id)>0){
	data<-data[-na.id,]
	warn.id<-paste("Waring: ",length(na.id),"adverse events with missing grade")
	warning(warn.id)}
rm(na.id)

### NA for AE Category
na.id<-which(is.na(data$Cat))
if(length(na.id)>0){
	data<-data[-na.id,]
	warn.id<-paste("Waring: ",length(na.id),"adverse events with missing Category")
	warning(warn.id)}
rm(na.id)	
	
### NA for AE descriptions
na.id<-which(is.na(data$AE))
if(length(na.id)>0){
	data<-data[-na.id,]
	warn.id<-paste("Waring: ",length(na.id),"adverse events with missing Definition")
	warning(warn.id)}
rm(na.id)
############################


############################
### Getting dataset of worst events
un.pat<-unique(data$PatID)
n.pat<-length(un.pat)
Wdata<-NULL

for(i in 1:n.pat){
	p.dat<-data[data$PatID==un.pat[i],]
	tap<-tapply(p.dat$Grade,as.character(p.dat$AE),max)
	id <- which(paste(p.dat$AE,p.dat$Grade)%in%paste(names(tap),tap))
	Wdata<-rbind(Wdata,p.dat[id,])
}


## table for worst events
Wdata$CatAE<-paste(Wdata$Cat,Wdata$AE,sep="_")
Wdata<-Wdata[order(Wdata$CatAE),]
WtabC<-ftable(Wdata$CatAE,Wdata$Arm,Wdata$Grade)

## table for total events
data$CatAE<-paste(data$Cat,data$AE,sep="_")
data<-data[order(data$CatAE),]
FtabC<-ftable(data$CatAE,data$Arm,data$Grade)

## merging together closed information
Closed <- matrix(paste(WtabC,"  (",FtabC,")",sep=""),dim(WtabC))
############################

## sorting out names and columns
AEnam <- sort(unique(Wdata$CatAE))
names <- AEnam[gl(nlevels(factor(AEnam)),length(unique(Arm)))]


stsp<-strsplit(names,split="_")
col.names<-t(matrix(unlist(stsp),2,length(stsp)))
Arm.col<-rep(levels(as.factor(data$Arm)),nrow(Closed)/length(unique(Arm)))

res<-data.frame(cbind(col.names,Arm.col,Closed))

names(res)<-c("CTC Categoty","Short Name","Arm",paste("G",levels(factor(data$Grade)),sep=""))

res

}

