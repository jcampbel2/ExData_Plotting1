
plot4 <-function(d="png", workdata=TRUE, fname="plot4.png") {
      
      toDate <- as.POSIXct("2007-02-02 23:59:59")
      fromDate <- as.POSIXct("2007-02-01 00:00:00")
      
      if (is.logical(workdata) && workdata) {
            workdata <- LoadData(fDate=fromDate, 
                                 tDate=toDate,
                                 wdir="C:/Users/james_000/DataScPrj/ExDA/Assign1/ExData_Plotting1",
                                 fname="household_power_consumption.txt")            
      }
      
      ##switch to set device
      if (d=="screen") {
            
            ##windows()
      } else if  (d=="png") {
            png(filename=fname,width=480, height=480,units="px")
      } else {
            stop("invalid output device")
      }
      
      
      par(mfrow=c(2,2))
      ##top-left
      with(workdata, plot(Time,Global_active_power, type="l", 
                          xlab="", ylab="Global Active Power"))
      ##top-right
      with(workdata, plot(Time,Voltage, type="l", 
                          xlab="datetime", ylab="Voltage"))
      
      ##bottom left
      with(workdata, {
            plot(Time,Sub_metering_1, type="l", xlab="", ylab="Energy sub metering", 
                 col="black")
            lines(Time,Sub_metering_2,type="l", col="red")
            lines(Time,Sub_metering_3,type="l", col="blue")
            legend ("topright", 
                    legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
                    col=c("black","red","blue"), pch="_", bty="n")
      } )
      ##bottom right
      with(workdata, plot(Time,Global_reactive_power, type="l", 
                          xlab="datetime", ylab="Global_reactive_power"))
      
      ##close device
      if  (d=="png") dev.off()
}


## utility function to load data from text file for assignment
## option parameters to subset return data.frame by date
## mandatory para for filename and workign directory
## function doe NOT do parameter validiation
## alternative parameter to only load partial file (-1 = all)

LoadData <- function(loadrow = -1, fDate=as.POSIXct("1900-01-01 00:00:00") , 
                     tDate=as.POSIXct("2999-12-31 23:59:59"),
                     wdir, fname) {
      setwd(wdir)
      PowerData <- read.table(fname,
                header=TRUE, sep=";", na.strings="?", 
                colClasses=c("character","character",
                             "numeric","numeric","numeric",
                             "numeric","numeric","numeric",
                             "numeric"),nrows=loadrow)

      PowerData$Time <- as.POSIXct(paste(PowerData$Date,PowerData$Time),format="%d/%m/%Y %H:%M:%S")
      PowerData$Date <- as.Date(PowerData$Date,format="%d/%m/%Y")

      RetData <- PowerData[PowerData$Time >= fDate & PowerData$Time <= tDate,]
      
      RetData
}

