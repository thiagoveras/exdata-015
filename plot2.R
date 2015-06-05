plot2 <- function() 
{  
    ## Getting the dataset (household_power_consuption.txt)
    data <- read.csv("./data/household_power_consumption.txt", header=T, sep=';', na.strings="?", 
                          nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
    data$Date <- as.Date(data$Date, format="%d/%m/%Y")
    
    ## Subsetting the data: Only be using data from the dates 2007-02-01 and 2007-02-02
    dataFiltered <- subset(data, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
    rm(data)
    
    ## Converting the date and time variables to datetime variable
    datetime <- paste(as.Date(dataFiltered$Date), dataFiltered$Time)
    dataFiltered$Datetime <- as.POSIXct(datetime)
    
    ## Ploting the graph
    message("Ploting the graph")
    plot(dataFiltered$Global_active_power~dataFiltered$Datetime, type="l",
         ylab="Global Active Power (kilowatts)", xlab="")
    
    ## Saving it to a PNG file (plot2.png) with a width of 480 pixels and a height of 480 pixels
    message("Saving the PNG file (plot2.png)")
    dev.copy(png, file="plot2.png", height=480, width=480)
    dev.off()
}