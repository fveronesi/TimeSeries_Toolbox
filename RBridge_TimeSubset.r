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
  
	print("Time Subsetting")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	timeCL = in_params[[2]]
	formatTM = in_params[[3]]
	subTM = in_params[[4]]

	out_shape = out_params[[1]]
   
	### Read Data
	d <- arc.open(source_shp)
	
	### Create a Data.Frame with all the variables
	data_SP <- arc.data2sp(arc.select(d, fields = "*"))
	
	
	DATETIME <- as.POSIXct(as.data.frame(data_SP)[,timeCL], format=formatTM)
		
	time.series <- xts(as.data.frame(data_SP), DATETIME)
	
	data_sub <- data_SP[which(DATETIME %in% index(time.series[subTM])),]
	
	arc.write(out_shape, data_sub)

}
	
#In ArcGIS with the Selection by Attribute it is possible to subset by time periods, like >= '2013-01-01' and <= '2013-02-01'.
#However, subsetting a single months is not possible. But subsetting with time IS possible: >= '2013-01-01 10:00:00' and <= '2013-02-01 10:00:00'.