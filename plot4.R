#Set wd
setwd("D:/Cousera/Modulo 4/Atividade 1")
#Import the data - renamed the original file
dados <- read.table("file.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

## Format date to Type Date
dados$Date <- as.Date(dados$Date, "%d/%m/%Y")

## Filter the data period
dados <- subset(dados,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Remove incomplete observation
dados <- dados[complete.cases(dados),]

## Combine Date and Time column
dateTime <- paste(dados$Date, dados$Time)

## Name the vector
dateTime <- setNames(dateTime, "DateTime")

## Remove Date and Time column
dados <- dados[ ,!(names(dados) %in% c("Date","Time"))]

## Add DateTime column
dados <- cbind(dateTime, dados)

## Format dateTime Column
dados$dateTime <- as.POSIXct(dateTime)

#plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(dados, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2,
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)")
})
dev.copy(png,file="plot4.png", height=480, width=480)
dev.off()
