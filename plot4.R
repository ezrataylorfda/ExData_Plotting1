#script to generate plot 3

#reset device
dev.off()

#script uses the data.table and plyr libraries
library(data.table)
library(plyr)

#read the data file
data <- fread("./Electric_power_consumption/household_power_consumption.txt")

#filter data for 2/1/2007 to 2/2/2007
data_subset <- rbind(data[data$Date=="2/2/2007",],data[data$Date=="2/1/2007",])

#convert and sort dates and times to date data, including a date/time column
data_subset <- mutate(data_subset, Date = as.Date(Date,"%m/%d/%Y"))
data_subset <- mutate(data_subset,Date_Time = paste(Date,Time))
data_subset <- mutate(data_subset,Date_Time = strptime(Date_Time,"%Y-%m-%d %H:%M:%S"))
data_subset <- arrange(data_subset,Date_Time)

#add a column with the weekday
data_subset <- mutate(data_subset,Weekday = weekdays(Date_Time))

#convert the data columns to numeric data
for (i in 3:9) {
      data_subset[[i]] <- as.numeric(data_subset[[i]])
}


#PNG device with dimensions 480x480
png("plot4.png",width=480,height=480)
par(mfrow=c(2,2))

y_max = max(c(max(data_subset$Sub_metering_1),max(data_subset$Sub_metering_2),max(data_subset$Sub_metering_3)))+5
y_min = min(c(min(data_subset$Sub_metering_1),min(data_subset$Sub_metering_2),min(data_subset$Sub_metering_3)))


#line graph of global active power by date
plot(data_subset$Date_Time, data_subset$Global_active_power, type = "l", lty = 1,ylab="Global Active Power (kilowatts)",xlab="Weekday",main="Global Active Power")

#line graph of voltage by date
plot(data_subset$Date_Time, data_subset$Voltage, type = "l", lty = 1,ylab="Voltage",xlab="Weekday",main="Voltage")

#line graph of sub metering by date
plot(data_subset$Date_Time, data_subset$Sub_metering_1, type = "n",ylab="Energy Sub Metering",xlab="Weekday",main="Sub Metering",ylim = c(y_min,y_max))
lines(data_subset$Date_Time, data_subset$Sub_metering_1, type = "l", lty = 1,col="black")
lines(data_subset$Date_Time, data_subset$Sub_metering_2, type = "l", lty = 1,col="red")
lines(data_subset$Date_Time, data_subset$Sub_metering_3, type = "l", lty = 1,col="blue")
legend("topright",legend=c("Sub Metering 1","Sub Metering 2","Sub Metering 3"),lty =1,col = c("black","red","blue"))

#line graph of global reactive power by date
plot(data_subset$Date_Time, data_subset$Global_reactive_power, type = "l", lty = 1,ylab="Global Reactive Power (kilowatts)",xlab="Weekday",main="Global Reactive Power")

#close device
dev.off()