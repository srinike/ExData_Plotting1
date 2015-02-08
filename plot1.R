createplot1 <- function() {
  
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
  
  # Create 'plot1' PNG file and plot the graph
  png(filename = "plot1.png", width = 480, height = 480, units = "px")
  hist(globalactivepower, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
  dev.off()
}