setwd("C:/Users/Anais/OneDrive/Documents/UAF/Research2/Growth Rate Data")
library(readxl)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(ggforce)

Raw_Sheet_List <- excel_sheets("Total Raw Data,2.xlsx")
Blank_Data<- read_excel("Total Raw Data,2.xlsx", sheet = "Blanks")
Blank_Data$`Start Time` <- rep(Blank_Data[[1,2]],nrow(Blank_Data))
Blank_Data$`TD` <- as.numeric(difftime(Blank_Data$`Time Taken`,Blank_Data$`Start Time`,units = "hours"))
Blank_Data$`Time Taken` <- NULL
Blank_Data$`Start Time` <- NULL

GR_Data <- read_excel("Total Raw Data,2.xlsx", sheet = "Data")
GR_Data$`Start Time` <- rep(GR_Data[[1,3]],nrow(GR_Data))
GR_Data$`TD` <- as.numeric(difftime(GR_Data$`Time Taken`,GR_Data$`Start Time`,units = "hours"))
GR_Data$`Time Taken` <- NULL
GR_Data$`Start Time` <- NULL


# View the blanks to see if there is any growth 
B_Data.2 <- melt(Blank_Data,id = c("TD","Temperature","Plate Number","Time Point"))
B_Data.2$`Time Point` <- NULL
B_Data.2$ID <- paste(B_Data.2$Temperature,",", B_Data.2$`Plate Number`)
ggplot(B_Data.2,aes(TD,value, colour = factor(ID))) + 
  geom_point() +
  facet_wrap(~ `Plate Number`) +
  xlab("Time (hours)") + 
  ylab("OD600") 

ggplot(B_Data.2,aes(TD,value, colour = factor(ID))) + 
    geom_point() +
    facet_wrap(~ Temperature) + 
    xlab("Time (hours)") + 
    ylab("OD600") 
  

# View the actually data without subtraction of blanks 
GR_Data.2 <- GR_Data
GR_Data.2$`Plate Number` <- NULL 
GR_Data.2 <- melt(GR_Data,id = c("TD","Temperature","Time Point","Strain"))


 
for (i in 1:5) {
  nam <- paste(i)
  plot <- ggplot(GR_Data.2,aes(TD,value, colour = factor(Temperature))) + 
      geom_point() +
      facet_wrap_paginate(~ Strain, nrow = 3,ncol = 3,page = nam) + 
      xlab("Time (hours)") + 
      ylab("OD600")
  print(plot)
}

ggplot(subset(GR_Data.2,Temperature == "17"))+
  geom_point(aes(TD,value))+
  facet_wrap_paginate(~ Strain, nrow = 3,ncol = 3,page = 1) + 
  xlab("Time (hours)") + 
  ylab("OD600")

#-------------------------------------------------------------------------------------------------------------------------------------------
#Use all of the data from -1 degree for all of the strains
#Use all of the data from 4 degree for all of the strain except for the ones on the 3rd plate where you only use the data up to 40 hours 
#Use all of the data from 11 degree for all of the strain except for the  ones on the 8th plate where you only use the data up to 40 hours 
#Use all of the data frin 17 degree for all of the strain except cut all of the strains at 40 hours and only use that data. 
# If there is no data at all( growth) for any temperature it is one of the ones to possibly redo
GR_Data.3 <- GR_Data[!(GR_Data$Temperature == 4 & GR_Data$`Plate Number`== 3 & GR_Data$TD > 40),]
GR_Data.4 <- GR_Data.3[!(GR_Data.3$Temperature == 11 & GR_Data.3$`Plate Number` == 8 & GR_Data.3$TD > 40),]
GR_Data.5 <- GR_Data.4[!(GR_Data.4$Temperature == 17 & GR_Data.4$TD > 40),]
GR_Data.5$`Plate Number` <- NULL
GR_Data.6 <- melt(GR_Data.5,id = c("TD","Temperature","Time Point","Strain"))
for (i in 1:5) {
  nam <- paste(i)
  plot <- ggplot(GR_Data.6,aes(TD,value, colour = factor(Temperature))) + 
    geom_point() +
    geom_smooth() +
    facet_wrap_paginate(~ Strain, nrow = 3,ncol = 3,page = nam) + 
    xlab("Time (hours)") + 
    ylab("OD600")
  print(plot)
}



