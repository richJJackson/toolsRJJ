percTable<-function(r,c,total=T,row=T){

	r <- as.factor(r);r.lev <- levels(r)
	c <- as.factor(c);c.lev <- levels(c)

	### creating basic table
	tab <- table(r,c)

	### Adding in Totals (if required)
	if(total){
		r.lev <- c(r.lev,"Total")
		c.lev <- c(c.lev,"Total")
		tab <- rbind(tab,colSums(tab))
		tab <- cbind(tab,rowSums(tab))
	}

	### Adding in percentages (if required)
	p.tab <- tab[,-ncol(tab)]
	perc <- round(100*p.tab/rowSums(p.tab),0)
	#perc <- cbind(perc,"")

	if(!row){
		p.tab <- tab[-nrow(tab),]
		perc <- round(100*t(t(p.tab)/colSums(p.tab)),0)
		#perc <- rbind(perc,"")
	}

	### replacing "NaN"
	miss.id <- which(is.na(perc)|perc=="NaN"|perc=="NA",arr.ind=T)
	if(length(miss.id)!=0){
		perc[miss.id] <- "0"
	}


	### pasting

	tab2 <- paste(p.tab," (",perc,"%)",sep="")
	dim(tab2) <- dim(p.tab)

	if(row)	tab2 <- cbind(tab2,tab[,ncol(tab)])
	if(!row) tab2 <- rbind(tab2,tab[nrow(tab),])


	### Adding in row/column names
	rownames(tab2) <- r.lev
	colnames(tab2) <- c.lev

	### writing out results
	tab2
}
