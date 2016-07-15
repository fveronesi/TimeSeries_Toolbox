### KMeans Clustering Toolbox
##Author: Fabio Veronesi
tool_exec <- function(in_params, out_params)
{
	if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
	require(sp)
	
	if (!requireNamespace("ggplot2", quietly = TRUE))
	install.packages("ggplot2")
	require(ggplot2)
	
	if (!requireNamespace("reshape2", quietly = TRUE))
    install.packages("reshape2")
	require(reshape2)
	
	if (!requireNamespace("xts", quietly = TRUE))
    install.packages("xts")
	require(xts)
  
	print("Time Histograms")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	variable = in_params[[2]]
	timeCL = in_params[[3]]
	formatTM = in_params[[4]]
	CAT.VAR = in_params[[5]]
	TM = in_params[[6]]
	
	out_folder = in_params[[7]]
   
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
		TIME_SUB <- unique(format(data_plot$DATETIME, format="%d"))
		
		results <- matrix(0, length(TIME_SUB), 2, dimnames=list(c(),c("Month","Frequency")))
		for(i in TIME_SUB){
			sub <- data_plot[format(data_plot$DATETIME, "%d")==i,]
			results[which(i==TIME_SUB),] <- matrix(c(as.numeric(paste(i)), nrow(sub)),ncol=2)
		}
		RES <- as.data.frame(results)

		dev.new()
		bc <- ggplot(data=RES, aes(x=as.factor(RES[,1]), y=RES[,2])) + 
			geom_bar(stat="identity") +
			xlab("Day") +
			ylab("Frequency") +
			theme_classic() 
		print(bc)
	
		ggsave(filename=paste0(out_folder,"/TimeHistogram_",variable,".jpg"),plot=bc, dpi=300)
	
	} else if(TM == "Month"){
		TIME_SUB <- unique(format(data_plot$DATETIME, format="%m"))
		
		results <- matrix(0, length(TIME_SUB), 2, dimnames=list(c(),c("Month","Frequency")))
		for(i in TIME_SUB){
			sub <- data_plot[format(data_plot$DATETIME, "%m")==i,]
			results[which(i==TIME_SUB),] <- matrix(c(as.numeric(paste(i)), nrow(sub)),ncol=2)
		}
		RES <- as.data.frame(results)

		dev.new()
		bc <- ggplot(data=RES, aes(x=as.factor(RES[,1]), y=RES[,2])) + 
			geom_bar(stat="identity") +
			xlab("Month") +
			ylab("Frequency") +
			theme_classic() 
		print(bc)
	
		ggsave(filename=paste0(out_folder,"/TimeHistogram_",variable,".jpg"),plot=bc, dpi=300)
	
	} else if (TM == "Year"){
		TIME_SUB <- unique(format(data_plot$DATETIME, format="%Y"))
		
		results <- matrix(0, length(TIME_SUB), 2, dimnames=list(c(),c("Month","Frequency")))
		for(i in TIME_SUB){
			sub <- data_plot[format(data_plot$DATETIME, "%Y")==i,]
			results[which(i==TIME_SUB),] <- matrix(c(as.numeric(paste(i)), nrow(sub)),ncol=2)
		}
		RES <- as.data.frame(results)

		dev.new()
		bc <- ggplot(data=RES, aes(x=as.factor(RES[,1]), y=RES[,2])) + 
			geom_bar(stat="identity") +
			xlab("Year") +
			ylab("Frequency") +
			theme_classic() 
		print(bc)
	
		ggsave(filename=paste0(out_folder,"/TimeHistogram_",variable,".jpg"),plot=bc, dpi=300)
	} 
	
	
}
