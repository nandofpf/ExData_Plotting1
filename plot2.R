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

#plot 2
plot(dados$Global_active_power~dados$dateTime, type="l", ylab="Global Active Power (kilowatts)")
dev.copy(png,file="plot2.png", height=480, width=480)
dev.off()
