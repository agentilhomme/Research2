library(readxl)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(ggforce)
library(growthrates)
library(data.table)

#-----------------------NEW DATA ---------------------------------------------------
setwd("C:/Users/Anais/OneDrive/Documents/UAF/Research2/Growth Rate Data")
Raw_Sheet_List <- excel_sheets("Total Raw Data,2.xlsx")
Blank_Data<- read_excel("Total Raw Data,2.xlsx", sheet = "Blanks")
GR_Data <- read_excel("Total Raw Data,2.xlsx", sheet = "Data")

#-------------------OLD DATA---------------------------------------------------------
# setwd("C:/Users/Anais/OneDrive/Documents/UAF/Research")
# Raw_Sheet_List <- excel_sheets("Total Raw Data.xlsx")
# Blank_Data<- read_excel("Total Raw Data.xlsx", sheet = "Blanks")
# Blank_Data$Date <- NULL
# Blank_Data$Time <- NULL
# GR_Data <- read_excel("Total Raw Data.xlsx", sheet = "Data")
# GR_Data$Time <- NULL
# GR_Data$Date <- NULL 


Blank_Data$`Start Time` <- rep(Blank_Data[[1,2]],nrow(Blank_Data))
Blank_Data$`TD` <- as.numeric(difftime(Blank_Data$`Time Taken`,Blank_Data$`Start Time`,units = "hours"))
Blank_Data$`Time Taken` <- NULL
Blank_Data$`Start Time` <- NULL


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
GR_Data.2 <- melt(GR_Data.2,id = c("TD","Temperature","Time Point","Strain"))


 
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
#------------------------------------------------------------------------------------------------------------------------------------------
# formatting data into usable frame to calculate the growth rate along each replicate 
Rep_Data.1 <- split.data.frame(GR_Data.6,with(GR_Data.6,interaction(variable,Temperature,Strain)),drop = TRUE)
# Creates an empty matrix with strain, Rep #, Temperature, growth rate and fill it 
GR_Data_All = as.data.frame(matrix(nrow = length(Rep_Data.1),ncol= 5))
colnames(GR_Data_All) <- c("Strain","Rep.","Temp.","GR","GR Test")
for (i in 1:length(Rep_Data.1)) {
  GR_Data_All[[i,1]] <- as.character(Rep_Data.1[[i]][1,4])
  GR_Data_All[[i,2]] <- as.character(Rep_Data.1[[i]][1,5])
  GR_Data_All[[i,3]] <- as.character(Rep_Data.1[[i]][1,2])
  # the try function is to bypass the errors if the growth rate is not valid 
  try({line1 <- fit_easylinear(Rep_Data.1[[i]]$TD,Rep_Data.1[[i]]$value)
  fit1 <- as.numeric(rsquared(line1))
  GR1 <- coef(line1)[3]
  GR1 <- as.numeric(GR1)
  
  line2 <- fit_spline(Rep_Data.1[[i]]$TD,Rep_Data.1[[i]]$value)
  fit2 <- rsquared(line2)
  GR2 <- coef(line2)[2]
  GR2 <- as.numeric(GR2)
  
  if(fit1 > fit2){
    GR_Data_All[[i,4]] <- GR1
    GR_Data_All[[i,5]] <- "fit_easylinear"
  } else{
    GR_Data_All[[i,4]] <- GR2
    GR_Data_All[[i,5]] <- "fit_spline"
  }
  },silent = TRUE)
}

# get a subset of the dataframe where the GR is negative or not valid( NA)
Invalid_Strains <- subset.data.frame(GR_Data_All,GR_Data_All$GR < 0 | is.na(GR_Data_All$GR))
Valid_GR_Data <- GR_Data_All[!(GR_Data_All$GR < 0 | is.na(GR_Data_All$GR)),]
table(Valid_GR_Data$`GR Test`)# this counts the the best test that was done was the fit_spline
Final_GR_Data = as.data.frame(matrix(nrow = length(Rep_Data.1),ncol= 4))
# remake the dataframe with only the valid strains using only the test that was the best 
for (i in 1:length(Rep_Data.1)) {
  Final_GR_Data[[i,1]] <- as.character(Rep_Data.1[[i]][1,4])
  Final_GR_Data[[i,2]] <- as.character(Rep_Data.1[[i]][1,5])
  Final_GR_Data[[i,3]] <- as.character(Rep_Data.1[[i]][1,2])
  # the try function is to bypass the errors if the growth rate is not valid 
  #try({
  line2 <- fit_spline(Rep_Data.1[[i]]$TD,Rep_Data.1[[i]]$value)
  GR2 <- as.numeric(coef(line2)[2])
  Final_GR_Data[[i,4]] <- GR2
  #},silent = TRUE)
}
Invalid_Rows <- c(698,699,700,701,702,703,731,732,922,923,924,1084,1242)
Final_GR_Data <- Final_GR_Data[!row.names(Final_GR_Data) %in% Invalid_Rows,]
colnames(Final_GR_Data) <- c("Strain","Rep.","Temp.","GR")
#--------------------------------------------------------------------------------------------------------------------
# Start analysis of growth rates by doing a boxplots and an anova to see if the growth rates are statistically significantly different
#between temperatures 


pdf("All Strains Boxplots.pdf")
for (i in 1:5) {
  print(ggplot(Final_GR_Data,aes(x = Temp., y = GR, fill = Strain)) + 
  geom_boxplot(show.legend = FALSE)+ 
  facet_wrap_paginate(~ Strain, nrow = 3,ncol = 3,page = paste(i)) + 
  geom_point()+
  theme(legend.position = "none"))
}
dev.off()

Final_GR_Data_spl <- split(Final_GR_Data,Final_GR_Data$Strain)
result.list <- list()
Temp.Statistic.Data <- NULL
for (i in 1:length(Final_GR_Data_spl)){
  Dataframe <- Final_GR_Data_spl[[i]]
  result.list[[1]] <- paste(Dataframe[1,1]) # strain 
  p.value <- summary(aov(GR~Temp.,data = Dataframe))[[1]][["Pr(>F)"]][[1]]
  result.list[[2]] <- p.value
  if(p.value < 0.05){
    result.list[[3]] <- paste("Sig.Temp.Diff")
  }else {
    result.list[[3]] <- paste("No.Sig.Temp.Diff")
  }
  Temp.Statistic.Data <- rbind.data.frame(Temp.Statistic.Data,result.list,stringsAsFactors = FALSE)
}
colnames(Temp.Statistic.Data) <- c("Strain","p-value","Significant Difference")
Sig.Diff.Data <- split(Temp.Statistic.Data,Temp.Statistic.Data$`Significant Difference`)
Sig.Diff.Strains <- Sig.Diff.Data[[2]]
Rel.Strain <- as.list(unique(Sig.Diff.Strains$Strain))
Rel.GR.Data <- Final_GR_Data[Final_GR_Data$Strain %in% Rel.Strain,]
Rel.GR.Data <- reshape(Rel.GR.Data,timevar = "Rep.",idvar = c("Strain","Temp."),direction = "wide")

Rel.GR.Data$Average <- rowMeans(Rel.GR.Data[,3:10],na.rm = TRUE) # going to average out each temperature at each strain and use temperature where the growth rate is max. 
Rel.GR.Data <- data.table(Rel.GR.Data, key = "Strain")
Rel.GR.Average <- Rel.GR.Data[, .SD[Average %in% max(Average)], by= Strain] # use the temperature associated with the maximum average
Rel.GR.OPT.New <- Rel.GR.Average[,-c(3:10)] # remove replicate columns
write.csv(Rel.GR.OPT.New,"New Growth Rates and OGT.csv")

Sig.Strain.Data <- subset(Final_GR_Data,Strain %in% Sig.Diff.Strains$Strain)
pdf("Significant Strains Boxplots.pdf")
for (i in 1:4) {
  print(ggplot(Sig.Strain.Data,aes(x = Temp., y = GR, fill = Strain)) + 
          geom_boxplot(show.legend = FALSE)+ 
          facet_wrap_paginate(~ Strain, nrow = 2,ncol = 4,page = paste(i)) + 
          geom_point()+
          theme(legend.position = "none"))
}
dev.off()







