# Add the experiments numbers that you wish to load.
experimentNumber <- c("7001")
# Select the cold loads.
loads <- c("0000")
# TC Transient Experiment
curves <- c("CC")
# frequency of the experiment
freq <- c("01")




# Set Directory of file
mainDir <- "C:/Users/Theo Christiaanse/Dropbox/Current Team Members/Theo Christiaanse/Phd/Other PhD Files/2017 - 2014 - PM1 Experiments/Experiment"
setwd(mainDir)
# Set directory of raw data
dataDir <- "Unprocessed"
# Set directory of processed data
tableDir <- "Processed"


#Set device\bed parameters
Rcond <- 0.7392
Rleak <- 0.3024
Tamb  <- 19.5


# We need this library for exporting tabular data.
library(foreign)
library(kimisc)
library(dplyr)
library(stringr)


# process data
dat       <- list()
small_dat <- list()
full_dat <- list()

for (i in 1:length(experimentNumber)) {
  filePath=file.path(dataDir,experimentNumber[i])
  if (length(list.files(filePath,include.dirs=TRUE))!=0) {
    for(j in 1:length(loads)){
      for(k in 1:length(curves)) {
        for(m in 1:length(freq)) {
          filePathChillerTemps=file.path(dataDir,experimentNumber[i],loads[j],curves[k],freq[m])
          chillerTemps=list.dirs(filePathChillerTemps)
          ChillerTemp=list.files(filePathChillerTemps)
          for(l in 1:(length(chillerTemps)-1)){
            if(length(chillerTemps)!=0){
              filePathCurves=chillerTemps[l+1]
              theList=list.files(filePathCurves,include.dirs = FALSE)
              if(length(theList)!=0){
                for(p in 1:length(theList)){
                  
                  #print(theList[p])      
                  dataFile <- read.table(file.path(filePathCurves,theList[p]), sep="\t",header=TRUE,fill=TRUE)
                  modDate <- format(as.Date(file.info(file.path(filePathCurves,theList[p]))$mtime,format="%Y-%m-%d "), "%Y-%m-%d")
                  
                  if(length(dataFile$Qes) == 0){
                    # Some files do not have Qes built in 
                    # a list of other numbers is assigned to prevent errors
                    dataFile$Qes <- dataFile$TH1..K.
                  }
                  if(length(dataFile$Tamb..K.) == 0) {
                    # Some files do not have Tamb built in 
                    # a list of constant number is assigned to prevent errors
                    dataFile$Tamb..K. <- Tamb
				          }
				          if(length(dataFile$Tin..K) == 0)  {
                    # Also the chiller temperatures are not known when this
                    # is not available. Set some values for those. 
                    dataFile$Tin..K.  <- as.numeric(ChillerTemp[l])
                    dataFile$Tout..K. <- as.numeric(ChillerTemp[l])
                  }
				          if(length(dataFile$Tflan..K.) == 0)  {
				            # Also the chiller temperatures are not known when this
				            # is not available. Set some values for those. 
				            dataFile$Tflan..K.  <- as.numeric(ChillerTemp[l])
				            dataFile$Tmag..K. <- as.numeric(ChillerTemp[l])
				          }

                  
                  # Take the time at half way point of the samples as the time point taken
                  halfTime <- round(length(levels(dataFile$Absolute.Time))/2)
                  AbbTimeMe <- levels(dataFile$Absolute.Time)[halfTime]
                  
                  # Calculate the temperature properties of the measured SS points.
                  TH1 <- mean(dataFile$TH1..K.,na.rm=TRUE) 
                  TC1 <- mean(dataFile$TC1..K.,na.rm=TRUE)
                  TH2 <- mean(dataFile$TH2..K.,na.rm=TRUE)
                  TC2 <- mean(dataFile$TC2..K.,na.rm=TRUE)
                
                  # Chiller in&out temperature
                  TCHILLERIN  <- mean(dataFile$Tin..K.,na.rm=TRUE)
                  TCHILLEROUT <- mean(dataFile$Tout..K.,na.rm=TRUE)
                  
                  # ambient temperature
                  TAMB        <- mean(dataFile$Tamb..K.,na.rm=TRUE)
				          # ambient temperature
				          TMAG        <- mean(dataFile$Tmag..K.,na.rm=TRUE)
				          TFLAN        <- mean(dataFile$Tflan..K.,na.rm=TRUE)
                  
                  THmean <- (TH1+TH2)/2
                  TCmean <- (TC1+TC2)/2
                  TSPAN <- THmean-TCmean
                  TAVE <- (THmean+TCmean)/2
                  
                  # Calculate the variance
                  varTH1 <- var(dataFile$TH1..K.,na.rm=TRUE) 
                  varTC1 <- var(dataFile$TC1..K.,na.rm=TRUE)
                  varTH2 <- var(dataFile$TH2..K.,na.rm=TRUE)
                  varTC2 <- var(dataFile$TC2..K.,na.rm=TRUE)                  
                            
                  # Calculate the Standard Diviation of Hot and Span
                  sdTSPAN1  <- sqrt(1/4*(varTH1+varTC1))
                  sdTSPAN2  <- sqrt(1/4*(varTH2+varTC2))
                  sdTHOT    <- sqrt(1/4*(varTH1+varTH2))

                  
                  # Report on pressuredrop
                  Pmean  <- mean(dataFile$P..psi.,na.rm=TRUE)
                  Pdrop  <- mean(dataFile$Pdrop..psi.,na.rm=TRUE)
                  
                  # Had a version where Pmean was equal to Pdrop so this just fixes that.
                  if(Pmean == Pdrop){
                    Pdrop <- max(dataFile$P..psi.,na.rm=TRUE)-min(dataFile$P..psi.,na.rm=TRUE)
                  }
                  
                  
                  
                  # Take measured frequency
                  FreqHz <- mean(dataFile$F..Hz.,na.rm=TRUE)
                  
                  
                   
                  # Qestimate 
                  QE <- mean(dataFile$Qes,na.rm=TRUE) 
                  QBEDS <- Rleak*(TCmean-TAMB)-Rcond*(THmean-TCmean)-as.numeric(loads[j])/10
                  
                  dat <- rbind(dat,list(AbbTime=AbbTimeMe,Ex=experimentNumber[i],load=loads[j],curve=curves[k],f=freq[m],chillerTemp=ChillerTemp[l],Thot1=TH1,Thot2=TH2,Thmean=THmean,sdTH=sdTHOT,Tcold1=TC1,Tcold2=TC2,Tcmean=TCmean,Tspan=TSPAN,sdTspan1=sdTSPAN1,sdTspan2=sdTSPAN2,pmean=Pmean,pdrop=Pdrop,freqHz=FreqHz,Qes=QE,Qbeds=QBEDS,TchilIn=TCHILLERIN,TchilOut=TCHILLEROUT,Tamb=TAMB,Taveg=TAVE,Tmag=TMAG,Tflan=TFLAN))
                  if(length(theList)==p){
                    small_dat <- rbind(small_dat,list(AbbTime=AbbTimeMe,Ex=experimentNumber[i],load=loads[j],curve=curves[k],f=freq[m],chillerTemp=ChillerTemp[l],Thot1=TH1,Thot2=TH2,Thmean=THmean,ThmeanK=THmean+273.15,Tcold1=TC1,Tcold2=TC2,Tcmean=TCmean,Tspan=TSPAN,pmean=Pmean,pdrop=Pdrop,freqHz=FreqHz,Qes=QE,Taveg=TAVE,Qbeds=QBEDS,Tamb=TAMB,Tmag=TMAG,Tflan=TFLAN))
                    full_dat <- rbind(full_dat,list(ModDate=modDate,AbbTime=AbbTimeMe,Ex=experimentNumber[i],load=loads[j],curve=curves[k],f=freq[m],chillerTemp=ChillerTemp[l],Thot1=TH1,Thot2=TH2,Thmean=THmean,ThmeanK=THmean+273.15,Tcold1=TC1,Tcold2=TC2,Tcmean=TCmean,Tspan=TSPAN,pmean=Pmean,pdrop=Pdrop,freqHz=FreqHz,Qes=QE,Taveg=TAVE,Qbeds=QBEDS,Tamb=TAMB,Tmag=TMAG,Tflan=TFLAN))
                    
                  }
                   
                  
                }
                
              }
            }
          }
        } 
      }
    }
    dat2         <- as.data.frame(dat)
    dat2$AbbTime <- as.character(dat2$AbbTime)
    dat2$Thot1   <- as.numeric(dat2$Thot1)
    dat2$Thot2   <- as.numeric(dat2$Thot2)
    dat2$Thmean  <- as.numeric(dat2$Thmean)
    
    dat2$Tmag    <- as.numeric(dat2$Tmag)
    dat2$Tflan   <- as.numeric(dat2$Tflan)
  
    dat2$Tcold1  <- as.numeric(dat2$Tcold1)
    dat2$Tcold2  <- as.numeric(dat2$Tcold2)
    dat2$Tcmean  <- as.numeric(dat2$Tcmean)
    dat2$Tspan   <- as.numeric(dat2$Tspan)
    dat2$pdrop   <- as.numeric(dat2$pdrop)
    dat2$pmean   <- as.numeric(dat2$pmean)
    dat2$Ex      <- as.numeric(dat2$Ex)
    dat2$freqHz  <- as.numeric(dat2$freqHz)
    dat2$Qes     <- as.numeric(dat2$Qes)
    dat2$f       <- as.numeric(dat2$f)
    dat2$sdTH    <- as.numeric(dat2$sdTH)
    dat2$sdTspan1<- as.numeric(dat2$sdTspan1)
    dat2$sdTspan2<- as.numeric(dat2$sdTspan2)
    dat2$Qbeds   <- as.numeric(dat2$Qbeds)
    
    dat2$curve  <- as.character(dat2$curve)
    dat2$load   <- as.character(dat2$load)
    dat2$chillerTemp <- as.character(dat2$chillerTemp)
    dat2$TchilIn <- as.character(dat2$TchilIn)
    dat2$TchilOut <- as.character(dat2$TchilOut)
    dat2$Tamb <- as.character(dat2$Tamb)
    dat2$Taveg  <- as.numeric(dat2$Taveg)
    
    
    
    small_dat2        <- as.data.frame(small_dat)
    small_dat2$Tmag    <- as.numeric(small_dat2$Tmag)
    small_dat2$Tflan   <- as.numeric(small_dat2$Tflan)
    
    small_dat2$AbbTime<- as.character(small_dat2$AbbTime)
    small_dat2$Thot1  <- as.numeric(small_dat2$Thot1)
    small_dat2$Thot2  <- as.numeric(small_dat2$Thot2)
    small_dat2$Thmean <- as.numeric(small_dat2$Thmean)
    small_dat2$ThmeanK <- as.numeric(small_dat2$ThmeanK)
    small_dat2$Tcold1 <- as.numeric(small_dat2$Tcold1)
    small_dat2$Tcold2 <- as.numeric(small_dat2$Tcold2)
    small_dat2$Tcmean <- as.numeric(small_dat2$Tcmean)
    small_dat2$Tspan  <- as.numeric(small_dat2$Tspan)
    small_dat2$pdrop  <- as.numeric(small_dat2$pdrop)
    small_dat2$pmean  <- as.numeric(small_dat2$pmean)
    small_dat2$Ex     <- as.numeric(small_dat2$Ex)
    small_dat2$freqHz <- as.numeric(small_dat2$freqHz)
    small_dat2$Qes    <- as.numeric(small_dat2$Qes)
    small_dat2$f    <- as.numeric(small_dat2$f)
    small_dat2$Qbeds    <- as.numeric(small_dat2$Qbeds)
    small_dat2$Taveg  <- as.numeric(small_dat2$Taveg)
    small_dat2$Tamb <- as.numeric(small_dat2$Tamb)
    
    small_dat2$curve  <- as.character(small_dat2$curve)
    small_dat2$load   <- as.character(small_dat2$load)
    small_dat2$chillerTemp <- as.character(small_dat2$chillerTemp)
    
    full_dat2        <- as.data.frame(full_dat)
    full_dat2$Tmag    <- as.numeric(full_dat2$Tmag)
    full_dat2$Tflan   <- as.numeric(full_dat2$Tflan)
    
    full_dat2$AbbTime<- as.character(full_dat2$AbbTime)
    full_dat2$Thot1  <- as.numeric(full_dat2$Thot1)
    full_dat2$Thot2  <- as.numeric(full_dat2$Thot2)
    full_dat2$Thmean <- as.numeric(full_dat2$Thmean)
    full_dat2$ThmeanK <- as.numeric(full_dat2$ThmeanK)
    full_dat2$Tcold1 <- as.numeric(full_dat2$Tcold1)
    full_dat2$Tcold2 <- as.numeric(full_dat2$Tcold2)
    full_dat2$Tcmean <- as.numeric(full_dat2$Tcmean)
    full_dat2$Tspan  <- as.numeric(full_dat2$Tspan)
    full_dat2$pdrop  <- as.numeric(full_dat2$pdrop)
    full_dat2$pmean  <- as.numeric(full_dat2$pmean)
    full_dat2$Ex     <- as.numeric(full_dat2$Ex)
    full_dat2$freqHz <- as.numeric(full_dat2$freqHz)
    full_dat2$Qes    <- as.numeric(full_dat2$Qes)
    full_dat2$f    <- as.numeric(full_dat2$f)
    full_dat2$Qbeds    <- as.numeric(full_dat2$Qbeds)
    full_dat2$Taveg  <- as.numeric(full_dat2$Taveg)
    full_dat2$Tamb <- as.numeric(full_dat2$Tamb)
    full_dat2$curve  <- as.character(full_dat2$curve)
    full_dat2$ModDate  <- as.character(full_dat2$ModDate)
    full_dat2$load   <- as.character(full_dat2$load)
    full_dat2$chillerTemp <- as.character(full_dat2$chillerTemp)
    
    
     
  } else {
    print("Can't find the experiment folder")
  }
  
}



# Directory + filename
exportFilePath=file.path(tableDir,"export_v1.txt")
# Sor
dat2$AbbTime <- strptime(as.character(dat2$AbbTime),'%H:%M:%S')
dat2 <- dat2[order(dat2$AbbTime),]
# Calculate ResTime
dat2$AbbTime <- as.POSIXct(dat2$AbbTime, format='%F %T')
dat2$ResTime <- seconds.to.hms(dat2$AbbTime-dat2$AbbTime[1])
# Reorder Columns
dat2 <- dat2[c("AbbTime", "ResTime", "Ex", "load", "curve", "f", "chillerTemp", "Thot1", "Thot2", "Thmean", "sdTH", "Tcold1", "Tcold2", "Tcmean", "Tspan", "sdTspan1", "sdTspan2","pmean","pdrop","freqHz","Qes","Qbeds","TchilIn","TchilOut","Tamb","Taveg","Tmag","Tflan")]
#Export data
write.table(dat2, exportFilePath, sep="\t",row.names = FALSE)


# Directory + filename
exportFilePath=file.path(tableDir,"export_points_v1.txt")
#Export data
write.table(small_dat2, exportFilePath, sep="\t",row.names = FALSE)

# Directory + filename
exportFilePath=file.path(tableDir,"export_tab_v1.txt")
#Sort data based on time

#Export data
write.table(full_dat2, exportFilePath, sep="\t",row.names = FALSE)

#options(OutDec = ".")
#cb <- function(df, sep="\t", dec=".", max.size=(20*1000)){
#  # Copy a data.frame to clipboard
#  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=6,decimal.mark = getOption("OutDec"))), sep=sep, row.names=FALSE,col.names=FALSE, dec=dec)
#}
#df <- cbind(full_dat2['Tamb'],full_dat2['Tmag'], full_dat2['Tflan'])
#cb(df)
#cb(full_dat2)
