### Function for Kaplan Meier plots

## input:	time
#			cen
#			fac



KMplot <- function(time,cen,fac,xlim="auto",Cen.no=FALSE,ylim=c(0,1),xlab="Time"
,ylab="Probability",sep=12,infl=1,LRtest=TRUE,LR.y="none",LR.x="none",back.grid=TRUE,summStat=TRUE,ss.x="none",ss.y="none",
leg.x="none",leg.y="none",X2="none",P="none",...){

library(survival)

### Sorting out factor

if(class(fac)!="factor"){
fac<-as.factor(as.character(fac))}


lev<-levels(fac)
n.lev<-length(lev)
ab.lev<-abbreviate(lev,minlength=35)
time<-as.numeric(time)

######## Graphical Settings -

#### vertical position of numbers at risk
ypos<-seq(0.15,0.85,length=(n.lev))
if(length(lev)==2) ypos<-c(0.25,0.7)
#ypos<-ypos[-c(1,length(ypos))]

#### setting xlim of graph
if(xlim[1]=="auto") xlim<-c(0,max(time,na.rm=TRUE))

#### Tidying xlim
xlim <- sep*round(xlim/sep)
#xlim[1] <- xlim[1]-(max(xlim)*0.01)
xlim[2] <- xlim[2]+(max(xlim)*0.01)

#if(lty[1]=="lty") lty<-rep(1,n.lev)
#ylim[1]<-ylim[1]-0.02
ylim[2]<-ylim[2]+0.03

#### Setting Legend Posistion
if(leg.x=="none") leg.x<-max(xlim*0.65)
if(leg.y=="none") leg.y<-0.85

#########
#### Calculating numbers at risk
#########

### Required Fucntion for Calculating Numbers at risk
Nrisk<-function(time){
NR<-NULL
for(i in 1:length(time.point)){
NR<-c(NR,length(which(time>=time.point[i])))
}
NR
}

Nob <- function(time){
NR<-NULL
for(i in 2:length(time.point)){
NR<-c(NR,length(which(time>=time.point[(i-1)] &time<time.point[i])))
}
NR
}

time.point<-seq(0,max(xlim,na.rm=TRUE),by=sep)
Risk <- tapply(time,fac,Nrisk)
Cen <- tapply(time[cen==0],fac[cen==0],Nob)

#### Estimating survival function
s.ob<-Surv(time,cen)
KM<-survfit(s.ob~fac)

#########
#### Plot 1 Set-up Figure
#########
#### Setting up Plotting Structures
mat<-matrix(c(1,2,3,5,2,4),3,2)
he<-0.03+0.04*n.lev

#################################  
quartz("KMplot",17,10.53)
layout(mat,height=c((1-he-0.015),0.075,he-0.015),width=c(max(0.13,(max(nchar(ab.lev)/250)+0.05)),0.9))
layout.show(5)

par(mar=c(4,0,2,4),bty="l",yaxs="i",xaxs="i")
plot(1,1,col=0,ylim=c(0,1),bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
mtext(ylab,4,adj=0.5,at=0.5,cex=1.4*infl,padj=-0.75,font=2)

### Error Trap
id<-which(time<=0)
if(length(id)>0) warning("Some Survival time =<0 observed")
################################

#########
#### Plot 2 (No. at Risk)
#########
ypos<-rev(ypos)
par(mar=c(0,0,0,0))
plot(1,1,col=0,xlim=c(0,1),ylim=c(0,1),bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
text(0.05,0.25,"Numbers at risk",pos=4,cex=1.5*infl,font=3)
#################################

#################################
par(mar=c(0,0,0,0),xaxs="i")
plot(KM,col=0,yaxt="n",xaxt="n",bty="n",xlim=c(0,1),ylim=ylim)
text(1,ypos,ab.lev,pos=2,cex=1.4*infl)
#################################

#################################
par(mar=c(0,0,0,1),xaxs="i")
plot(KM,col=0,yaxt="n",xaxt="n",bty="n",xlim=xlim,ylim=ylim)
TP <- time.point
TPcen <- TP[-1] - diff(TP)/2
#TP[1]<-TP[1]+0.005*max(xlim,na.rm=TRUE)

#plot(1,1,col=0,xlim=c(-1,max(xlim,na.rm=TRUE)),ylim=c(0,1),bty="n",xaxt="n",yaxt="n")
for(i in 1:n.lev){
text(TP,ypos[i],labels=Risk[[i]],cex=1.2*infl,font=2)
if(Cen.no) text(TPcen,ypos[i],labels=paste("(",Cen[[i]],")"),cex=1.2*infl,font=2)
}
#################################


#################################  
#### Survival Plot
par(mar=c(4,0,3,1))
plot(KM,yaxt="n",xaxt="n",xlim=xlim,ylim=ylim,bty="l",cex.lab=1.5*infl,xlab="",ylab="",bty="n",...)
mtext(xlab,1,padj=4,at=(max(xlim)/2),cex=1.3*infl,font=2)

#### Axis
segments(min(xlim),-1,min(xlim),1.02,lty=1,lwd=3)
segments(0,0,time.point,0,lty=1,lwd=3)
#segments(0,0,(max(xlim,na.rm=TRUE)+1),0,lty=1)
axis(seq(0,max(ylim-0.03,na.rm=TRUE),length=6),paste(seq(0,(max(ylim-0.03,na.rm=TRUE)*100),length=6),"%",sep=""),side=2,lwd=0,hadj=0.1,padj=0,cex.axis=1.2*infl,font=2,las=2)
axis(time.point,time.point,side=1,lwd=2,cex.axis=1.2*infl,font=2,padj=1,tick=TRUE)

if(back.grid){
abline(v=time.point[-1],lty=2,col="gray")
segments(c(0),seq(0,max(ylim-0.03,na.rm=TRUE),length=6),max(time.point),seq(0,max(ylim-0.03,na.rm=TRUE),length=6),lty=2,col="gray")
}


### Adding in Legend
if(n.lev>1) legend(leg.x,leg.y,lev,bty="n",cex=1.6*infl,...)


if(summStat){
ss.tb <- round(summary(KM)$table,1)
ss.txt <- paste(ss.tb[,7]," (", ss.tb[,8],", ", ss.tb[,9],")",sep="")

ss.lab <- paste(lev,": median (95% CI) - ",sep="")
ss.txt <- paste(ss.lab,ss.txt)

if(ss.x=="none") ss.x<-96
if(ss.y=="none") ss.y<-0.4

for(i in 1:length(lev)){
text(ss.x,ss.y-(i*0.05),ss.txt[i],cex=1.4*infl,font=3,pos=4)
}
}

#### Log Rank Test
if(LRtest){

if(n.lev <2) warning("Factor levels less than 2 - no LR provided")
if(n.lev>1){

### Log Rank Test
cm<-coxph(s.ob~fac)
LR<-summary(cm)$sctest

if(X2=="none") X2<-round(LR[1],2)
df<-round(LR[2],0)
if(P=="none") P<-round(LR[3],3)
if(P<0.001) P<-"<0.001"

if(LR.x=="none") LR.x<-96
if(LR.y=="none") LR.y<-0.5

Xl<-paste("Log Rank -",df,"df")
text(LR.x,LR.y, labels=bquote(Chi[.(Xl)]^{2} ~ "=" ~ .(X2) * ", p value =" ~ .(P)), cex=1.4*infl,font=3,pos=4)
}
}


}






