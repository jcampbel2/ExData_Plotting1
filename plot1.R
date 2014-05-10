

##parameter d is switch to either screen or png (default) file called plot1.png
##data.frame can either be passed as paramete (to speed up testing or will be loaded if workdata passed as TRUE) 
plot1 <- function(d="png", workdata=TRUE, fname="plot1.png") {
      
      toDate <- as.POSIXct("2007-02-02 23:59:59")
      fromDate <- as.POSIXct("2007-02-01Load 00:00:00")
      
      if (is.logical(workdata) && workdata) {
            workdata <- LoadData(fDate=fromDate, 
                                 tDate=toDate,
                                 wdir="C:/Users/james_000/DataScPrj/ExDA/Assign1/ExData_Plotting1",
                                 fname="household_power_consumption.txt")            
      }
      
      ##switch to set device
      if (d=="screen") {
            windows()
      } else if  (d=="png") {
            png(filename=fname,width=480, height=480,units="px")
      } else {
            stop("invalid output device")
      }
      
      hist(workdata$Global_active_power, col="red", main="Global Active Power", 
           xlab="Global Active Power (kilowatts)", ylab="Frequency")
      
      ##only close device if not screen
      if  (!d=="screen") dev.off()
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
