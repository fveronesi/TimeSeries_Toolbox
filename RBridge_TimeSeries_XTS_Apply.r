### KMeans Clustering Toolbox
##Author: Fabio Veronesi
tool_exec <- function(in_params, out_params)
{
	if (!requireNamespace("ggplot2", quietly = TRUE))
	install.packages("ggplot2")
	require(ggplot2)
	
	if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
	require(sp)
	
	if (!requireNamespace("reshape2", quietly = TRUE))
    install.packages("reshape2")
	require(reshape2)
	
	if (!requireNamespace("xts", quietly = TRUE))
    install.packages("xts")
	require(xts)
  
	print("Time Average Plots")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	variable = in_params[[2]]
	timeCL = in_params[[3]]
	formatTM = in_params[[4]]
	CAT.VAR = in_params[[5]]
	TM = in_params[[6]]
	position = in_params[[7]]
	
	out_folder = in_params[[8]]
   
	### Read Data
	d <- arc.open(source_shp)
	
	### Create a Data.Frame with all the variables
	data <- arc.select(d, variable)
	TIME <- arc.select(d, timeCL)
	
	data_plot <- data.frame(VAR=data[,variable], TIME=TIME[,timeCL])
	
	if(!is.null(CAT.VAR)){
		CAT = strsplit(CAT.VAR,"\"")[[1]][2]
		
		subVA = strsplit(CAT.VAR,"'")[[1]][2]
		
		CATEGORY <- arc.select(d, CAT)
		data_sub <- cbind(data_plot, CAT=CATEGORY[,CAT])
		data_plot <- data_sub[paste(data_sub$CAT)==subVA,]
	}
	
	data_plot$DATETIME <- as.POSIXct(data_plot$TIME, format=formatTM)
	
	
	if(TM == "Day"){
		time.series <- xts(data_plot$VAR, data_plot$DATETIME)
			
		dev.new()
		plot(time.series, main=paste0("Time Series ",variable),ylab=paste(variable))
		lines(apply.daily(time.series,FUN=mean),col="red")
		lines(apply.daily(time.series,FUN=quantile, probs=0.75),col="blue",lty=5)
		lines(apply.daily(time.series,FUN=quantile, probs=0.25),col="blue",lty=2)
		legend(position, title="Daily Average",c("Mean","q75","q25"),col=c("red","blue","blue"),lty=c(1,5,2))
	
		results <- data.frame(DateTime=index(apply.daily(time.series,FUN=mean)),
                      Mean=coredata(apply.daily(time.series,FUN=mean)),
                      q25=coredata(apply.daily(time.series,FUN=quantile, probs=0.25)),
                      q75=coredata(apply.daily(time.series,FUN=quantile, probs=0.75)))
					  
		write.table(results, paste0(out_folder,"/Daily_Average.csv"), sep=",", row.names=F)
	
	} else if(TM == "Week"){
		time.series <- xts(data_plot$VAR, data_plot$DATETIME)
			
		dev.new()
		plot(time.series, main=paste0("Time Series ",variable),ylab=paste(variable))
		lines(apply.weekly(time.series,FUN=mean),col="red")
		lines(apply.weekly(time.series,FUN=quantile, probs=0.75),col="blue",lty=5)
		lines(apply.weekly(time.series,FUN=quantile, probs=0.25),col="blue",lty=2)
		legend(position, title="Weekly Average",c("Mean","q75","q25"),col=c("red","blue","blue"),lty=c(1,5,2))
	
		results <- data.frame(DateTime=index(apply.weekly(time.series,FUN=mean)),
                      Mean=coredata(apply.weekly(time.series,FUN=mean)),
                      q25=coredata(apply.weekly(time.series,FUN=quantile, probs=0.25)),
                      q75=coredata(apply.weekly(time.series,FUN=quantile, probs=0.75)))
					  
		write.table(results, paste0(out_folder,"/Weekly_Average.csv"), sep=",", row.names=F)
	
	} else if(TM == "Month"){
		time.series <- xts(data_plot$VAR, data_plot$DATETIME)
			
		dev.new()
		plot(time.series, main=paste0("Time Series ",variable),ylab=paste(variable))
		lines(apply.monthly(time.series,FUN=mean),col="red")
		lines(apply.monthly(time.series,FUN=quantile, probs=0.75),col="blue",lty=5)
		lines(apply.monthly(time.series,FUN=quantile, probs=0.25),col="blue",lty=2)
		legend(position, title="Monthly Average",c("Mean","q75","q25"),col=c("red","blue","blue"),lty=c(1,5,2))
	
		results <- data.frame(DateTime=index(apply.monthly(time.series,FUN=mean)),
                      Mean=coredata(apply.monthly(time.series,FUN=mean)),
                      q25=coredata(apply.monthly(time.series,FUN=quantile, probs=0.25)),
                      q75=coredata(apply.monthly(time.series,FUN=quantile, probs=0.75)))
					  
		write.table(results, paste0(out_folder,"/Monthly_Average.csv"), sep=",", row.names=F)
	
	} else if (TM == "Year"){
		time.series <- xts(data_plot$VAR, data_plot$DATETIME)
			
		dev.new()
		plot(time.series, main=paste0("Time Series ",variable),ylab=paste(variable))
		lines(apply.yearly(time.series,FUN=mean),col="red")
		lines(apply.yearly(time.series,FUN=quantile, probs=0.75),col="blue",lty=5)
		lines(apply.yearly(time.series,FUN=quantile, probs=0.25),col="blue",lty=2)
		legend(position, title="Yearly Average",c("Mean","q75","q25"),col=c("red","blue","blue"),lty=c(1,5,2))
	
		results <- data.frame(DateTime=index(apply.yearly(time.series,FUN=mean)),
                      Mean=coredata(apply.yearly(time.series,FUN=mean)),
                      q25=coredata(apply.yearly(time.series,FUN=quantile, probs=0.25)),
                      q75=coredata(apply.yearly(time.series,FUN=quantile, probs=0.75)))
					  
		write.table(results, paste0(out_folder,"/Yearly_Average.csv"), sep=",", row.names=F)
	
	} 
	
	
}
