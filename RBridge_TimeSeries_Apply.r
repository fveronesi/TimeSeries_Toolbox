### KMeans Clustering Toolbox
##Author: Fabio Veronesi
tool_exec <- function(in_params, out_params)
{
	if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
	require(sp)
	
	if (!requireNamespace("reshape2", quietly = TRUE))
    install.packages("reshape2")
	require(reshape2)
	
	if (!requireNamespace("xts", quietly = TRUE))
    install.packages("xts")
	require(xts)
  
	print("Time Functions")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	variable = in_params[[2]]
	timeCL = in_params[[3]]
	formatTM = in_params[[4]]
	FUN_str = in_params[[5]]
	CAT.VAR = in_params[[6]]
	TM = in_params[[7]]
	
	
	out_folder = in_params[[8]]
   
	### Read Data
	d <- arc.open(source_shp)
	
	### Create a Data.Frame with all the variables
	data <- arc.select(d, variable)
	TIME <- arc.select(d, timeCL)
	
	data_plot <- data.frame(VAR=data[,variable], TIME=TIME[,timeCL])
	
	FUN_TM = eval(bquote(function(x) .(parse(text = FUN_str)[[1]]))) 
	
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
			
		res <- apply.daily(time.series,FUN=FUN_TM)
	
		results <- data.frame(DateTime=index(res),
                      Data=coredata(res))
		print(results)	  
		write.table(results, paste0(out_folder,"/Daily_Results.csv"), sep=",", row.names=F)
	
	} else if(TM == "Week"){
		time.series <- xts(data_plot$VAR, data_plot$DATETIME)
		
		res <- apply.weekly(time.series,FUN=FUN_TM)
	
		results <- data.frame(DateTime=index(res),
                      Data=coredata(res))
			
		print(results)	  
		write.table(results, paste0(out_folder,"/Weekly_Results.csv"), sep=",", row.names=F)
	
	} else if(TM == "Month"){
		time.series <- xts(data_plot$VAR, data_plot$DATETIME)
		
		res <- apply.monthly(time.series,FUN=FUN_TM)
	
		results <- data.frame(DateTime=index(res),
                      Data=coredata(res))
		print(results)		  
		write.table(results, paste0(out_folder,"/Monthly_Results.csv"), sep=",", row.names=F)
	
	} else if (TM == "Year"){
		time.series <- xts(data_plot$VAR, data_plot$DATETIME)
		
		res <- apply.yearly(time.series,FUN=FUN_TM)
	
		results <- data.frame(DateTime=index(res),
                      Data=coredata(res))
		print(results)
		write.table(results, paste0(out_folder,"/Yearly_Results.csv"), sep=",", row.names=F)
	
	} 
	
}
