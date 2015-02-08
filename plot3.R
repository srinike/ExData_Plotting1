createplot3 <- function() {
  
  # Read the data file
  data = read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
  
  # Remove the NA data
  cleandata = na.omit(data)
  
  # Create a new column with date and time
  cleandata$datetime = strptime(paste(cleandata$Date, cleandata$Time), "%d/%m/%Y %H:%M:%S")
  
  # Find the data for the two dates in February
  febdata = subset(cleandata, cleandata$datetime >= '2007-02-01 00:00:00' & cleandata$datetime <= '2007-02-02 23:59:59')
  
  # Get the data of the global active power
  globalactivepower = as.numeric(febdata$Global_active_power)
  
  datetime = febdata$datetime
  
  submeter1 = febdata$Sub_metering_1
  submeter2 = febdata$Sub_metering_2
  submeter3 = febdata$Sub_metering_3
  
  # Create 'plot3Q' PNG file and plot the graph
  png(filename = "plot3.png", width = 480, height = 480, units = "px")
  plot(datetime, submeter1, type="l", ylim=c(0,40), axes = FALSE, ylab = "Energy sub metering", xlab = "")
  par(new=T)
  plot(datetime, submeter2, type="l", col="red", ylim=c(0,40), ylab = "", xlab = "")
  par(new=T)
  plot(datetime, submeter3, type="l", col="blue", ylim=c(0,40), ylab = "", xlab = "")
  par(new=T)
  legend('topright', c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3") , lty=1, col=c('black', 'red', 'blue'), cex=.75)
  dev.off()
    
} 