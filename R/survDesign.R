
survDesign <- function(alpha,power,tails,HR,r=1,intFrac,intPow,nSite,rpm,openRate,maxTime,penal,folUp,S_t,plotDes=T,results=T){

  #############
  ### Sample Size estimate - Somewhere just over 500 events
  events <- gsdesign.survival(ifrac=intFrac,haz.ratio = HR,r=r,sig.level=alpha,power=power,delta.eb=intPow)
  if(tails==1) events <- gsdesign.survival(ifrac=intFrac,haz.ratio = HR,r=r,sig.level=alpha,
                                           power=power,delta.eb=intPow,alternative="one.sided")

  #############
  #############
  ### recruitment rate
  rec <- rec.forcast(nSite,rpm,openRate,maxTime,penal,plot=F);rec
  #############

  #############
  ### Fitted survival distribution to data

  ### Survival function
  S_new <- exp(log(S_t)%*%t(HR))
  #############

  #############
  ### Accumulating events
  surv <-rowSums(S_new+S_t)/(length(HR)+1)

  #(Monthly survival function)
  Mrec <- rec$Monthly.Rec	 #(Monthly recruitment)

  ### Function for estimating accumulating death rate based on recruitment and survival rates
  design <- accEvent(Mrec,surv, length(surv),drop=0.05);design
  #############


  #### Plotting survival functions and recruitment info
  if(plotDes){
    quartz("Time To Event Design",height=5,width=10)
    par(mfrow=c(1,2),mar=c(4.5,4.5,2,1.5))


    plot(0,0,col=0,xlim=c(0,max(maxTime,folUp)),ylim=c(0,1),main="Survival Function",xlab="Time (Months)",ylab="Overall Survival (%)",bty="l",cex.lab=0.8,cex.main=0.8,cex.axis=0.7,cex.lab=0.7,cex=0.7)
    lines(t,S_t,col=c("hotpink"),lwd=4)
    lines(t,S_new,col=c("lightblue"),lwd=4)
    abline(h=c(0.25,0.5,0.75,1),v=seq(12,240,by=12),col="gray",lty=2)

    ### Plotting recruitment
    trialLength <- maxTime + folUp
    plot(design$Month[1:nrow(rec)],design$CumulativeRec[1:nrow(rec)],lwd=3,typ="l",xlim=c(0,nrow(design)),main="Cumulative Recruitment/Deaths",xlab="Time (Months)",ylab="Cumulative recruitment/Deaths",cex.lab=0.8,cex.main=0.8,cex.axis=0.7,cex.lab=0.7,cex=0.7)
    lines(design$Month,design$accEvent,col=6,lwd=3)
    abline(h=seq(100,900,by=100),v=seq(0,108,by=12),col="gray",lty=2)
    abline(h= events$num.events,lty=3,col=2,lwd=3)
    abline(v=trialLength*intFrac,lty=3,col="forestgreen",lwd=3)


  }
  ######

  ### design summary
  sumDes <- data.frame("SS"=events$num.events,"Recruitment"=max(design$CumulativeRec),"Deaths"=max(design$accEvent))

  list(sumDes,design)


}

