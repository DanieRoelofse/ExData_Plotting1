##Exploratory data analysis 

setwd("C:\\Coursera\\Exploratory Data Analysis\\Week 1\\Assignment")


#Importing#

fh <- file("household_power_consumption.txt")
ba <- read.table(text = grep("^[1,2]/2/2007", readLines(fh), value = TRUE), col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), sep = ";", header = TRUE)

## Getting full dataset
data_full <- read.csv("household_power_consumption.txt", header = T, sep = ';', 
                      na.strings = "?", nrows = 2075259, check.names = F, 
                      stringsAsFactors = F, comment.char = "", quote = '\"')
data_full$Date <- as.Date(data_full$Date, format = "%d/%m/%Y")

## Subsetting the data
data <- subset(data_full, subset = (Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(data_full)#Removing the full data set

## Converting dates
datetime <- paste(as.Date(data$Date), data$Time)
data$Datetime <- as.POSIXct(datetime)


#Plot3
plotFile=file.path(getwd(),"Plot3.png")
png(plotFile,width=480,height = 480,units='px')

with(data,{plot(Sub_metering_1~Datetime,
                col="black"
                ,type="l"
                ,xlab=""
                ,ylab = "Energy Sub Metering")
  ,lines(Sub_metering_2~Datetime,type="l",col="red",xlab="",ylab="Energy Sub Metering")
  ,lines(Sub_metering_3~Datetime,type="l",col="blue",xlab="",ylab="Energy Sub Metering")})

plot(data$Sub_metering_1~data$Datetime,type="l",col="black",xlab="",ylab="Energy Sub Metering")
lines(data$Sub_metering_2~data$Datetime,type="l",col="red")
lines(data$Sub_metering_3~data$Datetime,type="l",col="blue")

legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 1,cex=0.6 ,
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.off()