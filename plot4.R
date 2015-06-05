plot4 <- function() 
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
    par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
    with(dataFiltered, {
        plot(Global_active_power~Datetime, type="l", 
             ylab="Global Active Power (kilowatts)", xlab="")
        plot(Voltage~Datetime, type="l", 
             ylab="Voltage (volt)", xlab="")
        plot(Sub_metering_1~Datetime, type="l", 
             ylab="Global Active Power (kilowatts)", xlab="")
        lines(Sub_metering_2~Datetime,col='Red')
        lines(Sub_metering_3~Datetime,col='Blue')
        legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex=0.30)
        plot(Global_reactive_power~Datetime, type="l", 
             ylab="Global Rective Power (kilowatts)",xlab="")
    })
    
    ## Saving it to a PNG file (plot4.png) with a width of 480 pixels and a height of 480 pixels
    message("Saving the PNG file (plot4.png)")
    dev.copy(png, file="plot4.png", height=480, width=480)
    dev.off()
}