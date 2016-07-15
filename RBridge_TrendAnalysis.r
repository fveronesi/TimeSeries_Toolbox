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
  
	print("Trend Analysis")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	variable = in_params[[2]]
	timeCL = in_params[[3]]
	formatTM = in_params[[4]]
	CAT.VAR = in_params[[5]]
	FREQ = in_params[[6]]
		
   
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
		data_plot <- cbind(data_plot, CAT=CATEGORY[,CAT])
		data_sub <- data_plot[paste(data_plot$CAT)==subVA,]
		
		data_sub$DATETIME <- as.POSIXct(data_sub$TIME, format=formatTM)
		
		time.series <- xts(data_sub$VAR, as.Date(data_sub$DATETIME))
		attr(time.series, 'frequency') <- as.numeric(FREQ)
		
		dev.new()
		plot(decompose(as.ts(time.series)))
	
	} else {
		data_plot$DATETIME <- as.POSIXct(data_plot$TIME, format=formatTM)

		time.series <- ts(data_sub$VAR, as.Date(data_sub$DATETIME))
		attr(time.series, 'frequency') <- as.numeric(FREQ)	
		
		dev.new()
		plot(decompose(as.ts(time.series)))
	
	}
	
	

	
	
}
