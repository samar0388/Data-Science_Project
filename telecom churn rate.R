
# ### Data Import ## ------------------------------------------------------

# working directory
getwd()

#Read given csv file
telecom_data<- read.csv("F:\\Jigsaw assignment\\capstone project\\project\\case study\\sampletelecomfinal.csv")

#Install some commonly required package and import the librarys

library(dplyr) #Work with Data Frame and carry out some common manipulation;#linear Model or simple regression model function useage
library(gains) # Gains Chart, Gains tables and lift chart for prediction algorithms
library(irr) #Kappa matrix; Model Accuracy algorithm 
library(ROCR) #ROCR Curve; Cutoff Parameterized Performance Curve design
library(caret) #Confusion Matrix: Classification and Regression Training 
install.packages("lm.beta")
library(lm.beta) #linear model object in MASS package
library(car) #multicollinearity Type I , Type III calcualtions  OR
install.packages("VIF")
library(VIF)
library(ggplot2)
library(caTools)
library(dummies)
library(MASS)
library(car)

#Disable Scientific Notation (to enable use options(scipen = 0) )
options(scipen = 999)


# ## Data Quality Report## ------------------------------------------------

#Basic sanity check of dataset
dim(telecom_data)            #row-13259,    col-79
colnames(telecom_data)       # data_documentation.csv has some meaning of each variable  OR names(telecomChurn_Study)
str(telecom_data)            # Data Frame with Differnt Data Type Columns
class(telecom_data)          # Class of data
head(telecom_data)           # top 6 rows of dataset 
tail(telecom_data)           # Top and Bottom values in data Set Check
View(telecom_data)           # View data's in Grid Format
summary(telecom_data)        # Summary of each column data's


#Extracting all columns of dataset
colnames_telecomdata<- as.data.frame(variable.names(telecom_data))
nrow(colnames_telecomdata)

#Creating data quality report(DQR)
#Data type or class for each variable
DQR_telecom_data<- colnames_telecomdata
DQR_telecom_data$datatype<- sapply(telecom_data, class)

#Check Sum of Records available for all variable  
DQR_telecom_data$NoOfRecords<-nrow(telecom_data)

#Check Sum of unique Records available for all variable  
for(i in 1:ncol(telecom_data))
{
  DQR_telecom_data$UniqueRecords[i] <- length(unique(telecom_data[,i]))
} 

#Check Sum of Observations available for all variable and percentage of Observations
DQR_telecom_data$DataAvailable <- colSums(!is.na(telecom_data))
DQR_telecom_data$AvailablePercent <- round(colMeans(!is.na(telecom_data)),3)

#Check Sum of Missed Values / anamolys for all variable and percentage of Missing Values
DQR_telecom_data$Missing <- colSums(is.na(telecom_data))
DQR_telecom_data$MissingPercent <- round(colMeans(is.na(telecom_data)),3)

#Find Numerical Measures result (Check Minimum, Maximum, Mean, Quantile values for all variables)
for(i in 1:ncol(telecom_data))
{
  DQR_telecom_data$Minimum[i]                <- round(ifelse(class(telecom_data[,i]) == "integer" | class(telecom_data[,i]) == "numeric",min(telecom_data[,i],na.rm=T),0))
  DQR_telecom_data$Maximum[i]                <- round(ifelse(class(telecom_data[,i]) == "integer" | class(telecom_data[,i]) == "numeric",max(telecom_data[,i],na.rm=T),0))
  DQR_telecom_data$Mean[i]                   <- round(ifelse(class(telecom_data[,i]) == "integer" | class(telecom_data[,i]) == "numeric",mean(telecom_data[,i],na.rm=T),0))
  DQR_telecom_data$FifthPercentile[i]        <- round(ifelse(class(telecom_data[,i]) == "integer" | class(telecom_data[,i]) == "numeric",quantile(telecom_data[,i],p=0.05,na.rm=T),0))
  DQR_telecom_data$TenthPercentile[i]        <- round(ifelse(class(telecom_data[,i]) == "integer" | class(telecom_data[,i]) == "numeric",quantile(telecom_data[,i],p=0.10,na.rm=T),0))
  DQR_telecom_data$TwentyFifthPercentile[i]  <- round(ifelse(class(telecom_data[,i]) == "integer" | class(telecom_data[,i]) == "numeric",quantile(telecom_data[,i],p=0.25,na.rm=T),0))
  DQR_telecom_data$FiftythPercentile[i]      <- round(ifelse(class(telecom_data[,i]) == "integer" | class(telecom_data[,i]) == "numeric",quantile(telecom_data[,i],p=0.50,na.rm=T),0))
  DQR_telecom_data$SeventyFifthPercentile[i] <- round(ifelse(class(telecom_data[,i]) == "integer" | class(telecom_data[,i]) == "numeric",quantile(telecom_data[,i],p=0.75,na.rm=T),0))
  DQR_telecom_data$NinetythPercentile[i]     <- round(ifelse(class(telecom_data[,i]) == "integer" | class(telecom_data[,i]) == "numeric",quantile(telecom_data[,i],p=0.90,na.rm=T),0))
  DQR_telecom_data$NinetyfifthPercentile[i]  <- round(ifelse(class(telecom_data[,i]) == "integer" | class(telecom_data[,i]) == "numeric",quantile(telecom_data[,i],p=0.95,na.rm=T),0))
}

#Export in .csv Data Quality Report 
write.csv(DQR_telecom_data, "Data Quality Report for Telecom data.csv", row.names = T)



# ## Dataset Variable Profiling ## ----------------------------------------

summary(telecom_data$retdays)
head(telecom_data$retdays, 100)  ## This is not missing as here states that how many 
                                 ## coustmer retained after number of days.
sort(unique(telecom_data$retdays), na.last = F)
telecom_data$retdays_1<-ifelse(is.na(telecom_data$retdays)==TRUE, 0, 1)
summary(telecom_data$retdays_1)
head(telecom_data$retdays_1,100)

#missing value teatment by rejecting the column
#Reject variables with more than 10% anamolys and Assign New dataset for further analysys Process
Cleaned_telecom_data <- telecom_data[, colMeans(is.na(telecom_data)) <= 0.10]
summary(Cleaned_telecom_data)#'data.frame':	13259 obs. of  65 variables: (Rejected 14 variable)
write.csv(Cleaned_telecom_data, file = "cleaned telecom data.csv")

#Variable drop_blk_Means is one such: it shows its appearence in combination of 
# blck_dat_Mean, drop_dat_Mean
#Reject / omit variable blck_dat_Mean
Cleaned_telecom_data <- Cleaned_telecom_data[,-6]
#Check its position
names(Cleaned_telecom_data) #Remove Position 6 (66 become 65 in total)

#Variable Profiling: Continuous Variables, Categorical Variables
#Deciling Continuous Variables; finding out event rate (churn rate in this case)

#variable :  [1] "mou_Mean" 

summary(Cleaned_telecom_data$mou_Mean)
dat_mou_mean    <- Cleaned_telecom_data%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_mou_mean$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_mou_mean$churn_pred <- round(dat_mou_mean$n / dat_mou_mean$N , 2)
dat_mou_mean$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mou_Mean,n = 10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat_mou_mean$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mou_Mean,n = 10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat_mou_mean$varname    <- rep("mou_mean", nrow(dat_mou_mean))
View(dat_mou_mean)

##variable :  [2] "totmrc_mean"
summary(Cleaned_telecom_data$totmrc_Mean)
dat_totmrc_mean    <- Cleaned_telecom_data%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_totmrc_mean$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_totmrc_mean$churn_pred <- round(dat_totmrc_mean$n / dat_totmrc_mean$N , 2)
dat_totmrc_mean$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(totmrc_Mean,n = 10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat_totmrc_mean$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(totmrc_Mean,n = 10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat_totmrc_mean$varname    <- rep("totmrc_Mean", nrow(dat_totmrc_mean))
View(dat_totmrc_mean)

  ##variable :  [3] "rev_range"
  summary(Cleaned_telecom_data$rev_Range)
  dat_rev_range    <- Cleaned_telecom_data%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
  dat_rev_range$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
  dat_rev_range$churn_pred <- round(dat_rev_range$n / dat_rev_range$N , 2)
  dat_rev_range$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(rev_Range,n = 10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
  dat_rev_range$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(rev_Range,n = 10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
  dat_rev_range$varname    <- rep("rev_range", nrow(dat_rev_range))
  View(dat_rev_range)
  
  ##variable :  [4] "mou_range"
  summary(Cleaned_telecom_data$mou_Range)
  dat_mou_range    <- Cleaned_telecom_data%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
  dat_mou_range$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
  dat_mou_range$churn_pred <- round(dat_mou_range$n / dat_mou_range$N , 2)
  dat_mou_range$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mou_Range,n = 10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
  dat_mou_range$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mou_Range,n = 10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
  dat_mou_range$varname    <- rep("mou_Range", nrow(dat_mou_range))
  View(dat_mou_range)
  
  ##variable :  [5] "change_mou"
  summary(Cleaned_telecom_data$change_mou)
  dat_change_mou    <- Cleaned_telecom_data%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
  dat_change_mou$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
  dat_change_mou$churn_pred <- round(dat_change_mou$n / dat_change_mou$N , 2)
  dat_change_mou$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(change_mou,n = 10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
  dat_change_mou$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(change_mou,n = 10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
  dat_change_mou$varname    <- rep("change_mou", nrow(dat_change_mou))
  View(dat_change_mou)
  
  ##variable : [6] "drop_vce_Range"
  summary(Cleaned_telecom_data$drop_vce_Range)
  dat_drop_vce_Range    <- Cleaned_telecom_data%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
  dat_drop_vce_Range$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
  dat_drop_vce_Range$churn_pred <- round(dat_drop_vce_Range$n / dat_drop_vce_Range$N , 2)
  dat_drop_vce_Range$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(drop_vce_Range,n = 10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
  dat_drop_vce_Range$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(drop_vce_Range,n = 10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
  dat_drop_vce_Range$varname    <- rep("drop_vce_Range", nrow(dat_drop_vce_Range))
  View(dat_drop_vce_Range)
  
  ##variable : [7] "owylis_vce_Range"
  summary(Cleaned_telecom_data$owylis_vce_Range)
  dat_owylis_vce_Range    <- Cleaned_telecom_data%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
  dat_owylis_vce_Range$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
  dat_owylis_vce_Range$churn_pred <- round(dat_owylis_vce_Range$n / dat_owylis_vce_Range$N , 2)
  dat_owylis_vce_Range$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(owylis_vce_Range,n = 10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
  dat_owylis_vce_Range$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(owylis_vce_Range,n = 10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
  dat_owylis_vce_Range$varname    <- rep("owylis_vce_Range", nrow(dat_owylis_vce_Range))
  View(dat_owylis_vce_Range)
  
  ##variable : [8] "mou_opkv_Range"
  summary(Cleaned_telecom_data$mou_opkv_Range)
  dat_mou_opkv_Range    <- Cleaned_telecom_data%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
  dat_mou_opkv_Range$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
  dat_mou_opkv_Range$churn_pred <- round(dat_mou_opkv_Range$n / dat_mou_opkv_Range$N , 2)
  dat_mou_opkv_Range$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mou_opkv_Range,n = 10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
  dat_mou_opkv_Range$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mou_opkv_Range,n = 10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
  dat_mou_opkv_Range$varname    <- rep("mou_opkv_Range", nrow(dat_mou_opkv_Range))
  View(dat_mou_opkv_Range)
  
  ##variable : [9] "months"
  summary(Cleaned_telecom_data$months)
  dat_months    <- Cleaned_telecom_data%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)
  dat_months$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
  dat_months$churn_pred <- round(dat_months$n / dat_months$N , 2)
  dat_months$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(months,n = 10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
  dat_months$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(months,n = 10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
  dat_months$varname    <- rep("months", nrow(dat_months))
  View(dat_months)
  
  ##variable : [10] "totcalls"
  summary(Cleaned_telecom_data$totcalls)
  dat_totcalls    <- Cleaned_telecom_data%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)
  dat_totcalls$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
  dat_totcalls$churn_pred <- round(dat_totcalls$n / dat_totcalls$N , 2)
  dat_totcalls$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(totcalls,n = 10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
  dat_totcalls$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(totcalls,n = 10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
  dat_totcalls$varname    <- rep("totcalls", nrow(dat_totcalls))
  View(dat_totcalls)
  
  ##variable : [11] "eqpdays"
  summary(Cleaned_telecom_data$eqpdays)
  dat_eqpdays    <- Cleaned_telecom_data%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)
  dat_eqpdays$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
  dat_eqpdays$churn_pred <- round(dat_eqpdays$n / dat_eqpdays$N , 2)
  dat_eqpdays$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(eqpdays,n = 10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
  dat_eqpdays$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(eqpdays,n = 10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
  dat_eqpdays$varname    <- rep("eqpdays", nrow(dat_eqpdays))
  View(dat_eqpdays)


##variable : [17] "adjqty"
summary(Cleaned_telecom_data$adjqty)
dat_adjqty    <- Cleaned_telecom_data%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_adjqty$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat_adjqty$churn_pred <- round(dat_adjqty$n / dat_adjqty$N , 2)
dat_adjqty$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(adjqty,n = 10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat_adjqty$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(adjqty,n = 10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat_adjqty$varname    <- rep("adjqty", nrow(dat_adjqty))
View(dat_adjqty)

##variable : [18] "ovrrev_Mean"
summary(Cleaned_telecom_data$ovrrev_Mean)
dat_ovrrev_Mean    <- Cleaned_telecom_data%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_ovrrev_Mean$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_ovrrev_Mean$churn_pred <- round(dat_ovrrev_Mean$n / dat_ovrrev_Mean$N , 2)
dat_ovrrev_Mean$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(ovrrev_Mean,n = 10))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat_ovrrev_Mean$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(ovrrev_Mean,n = 10))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat_ovrrev_Mean$varname    <- rep("ovrrev_Mean", nrow(dat_ovrrev_Mean))
View(dat_ovrrev_Mean)

##variable : [19] "rev_Mean"
summary(Cleaned_telecom_data$rev_Mean)
dat_rev_Mean    <- Cleaned_telecom_data%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_rev_Mean$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_rev_Mean$churn_pred <- round(dat_ovrrev_Mean$n / dat_rev_Mean$N , 2)
dat_rev_Mean$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(rev_Mean,n = 10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat_rev_Mean$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(rev_Mean,n = 10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat_rev_Mean$varname    <- rep("rev_Mean", nrow(dat_rev_Mean))
View(dat_rev_Mean)


##variable : [21] "comp_vce_Mean"    #omit this variable as decile having less data
summary(Cleaned_telecom_data$comp_vce_Mean)
dat_comp_vce_Mean    <- Cleaned_telecom_data%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_comp_vce_Mean$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_comp_vce_Mean$churn_pred <- round(dat_comp_vce_Mean$n / dat_comp_vce_Mean$N , 2)
dat_comp_vce_Mean$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(comp_vce_Mean,n = 10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat_comp_vce_Mean$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(comp_vce_Mean,n = 10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat_comp_vce_Mean$varname    <- rep("comp_vce_Mean", nrow(dat_comp_vce_Mean))
View(dat_comp_vce_Mean)

##variable : [22] "plcd_vce_Mean"
summary(Cleaned_telecom_data$plcd_vce_Mean)
dat_plcd_vce_Mean    <- Cleaned_telecom_data%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_plcd_vce_Mean$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_plcd_vce_Mean$churn_pred <- round(dat_plcd_vce_Mean$n / dat_plcd_vce_Mean$N , 2)
dat_plcd_vce_Mean$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(plcd_vce_Mean,n = 10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat_plcd_vce_Mean$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(plcd_vce_Mean,n = 10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat_plcd_vce_Mean$varname    <- rep("plcd_vce_Mean", nrow(dat_plcd_vce_Mean))
View(dat_plcd_vce_Mean)

##variable : [23] "avg3mou"
summary(Cleaned_telecom_data$avg3mou)
dat_avg3mou   <- Cleaned_telecom_data%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avg3mou$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat_avg3mou$churn_pred <- round(dat_avg3mou$n / dat_avg3mou$N , 2)
dat_avg3mou$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg3mou,n = 10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat_avg3mou$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg3mou,n = 10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat_avg3mou$varname    <- rep("avg3mou", nrow(dat_avg3mou))
View(dat_avg3mou)

##variable : [24] "avgmou"
summary(Cleaned_telecom_data$avgmou)
dat_avgmou   <- Cleaned_telecom_data%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avgmou$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat_avgmou$churn_pred <- round(dat_avgmou$n / dat_avgmou$N , 2)
dat_avgmou$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avgmou,n = 10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat_avgmou$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avgmou,n = 10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat_avgmou$varname    <- rep("avgmou", nrow(dat_avgmou))
View(dat_avgmou)

#variable : [25] "avg3qty"
summary(Cleaned_telecom_data$avg3qty)
dat_avg3qty  <- Cleaned_telecom_data%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avg3qty$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat_avg3qty$churn_pred <- round(dat_avg3qty$n / dat_avg3qty$N , 2)
dat_avg3qty$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg3qty,n = 10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat_avg3qty$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg3qty,n = 10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat_avg3qty$varname    <- rep("avg3qty", nrow(dat_avg3qty))
View(dat_avg3qty)

#variable : [26] "avgqty"
summary(Cleaned_telecom_data$avgqty)
dat_avgqty  <- Cleaned_telecom_data%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avgqty$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat_avgqty$churn_pred <- round(dat_avgqty$n / dat_avgqty$N , 2)
dat_avgqty$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avgqty,n = 10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat_avgqty$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avgqty,n = 10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat_avgqty$varname    <- rep("avgqty", nrow(dat_avgqty))
View(dat_avgqty)

#variable : [27] "avg6mou"
summary(Cleaned_telecom_data$avg6mou)
dat_avg6mou  <- Cleaned_telecom_data%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avg6mou$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat_avg6mou$churn_pred <- round(dat_avg6mou$n / dat_avg6mou$N , 2)
dat_avg6mou$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg6mou,n = 10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat_avg6mou$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg6mou,n = 10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat_avg6mou$varname    <- rep("avg6mou", nrow(dat_avg6mou))
View(dat_avg6mou)

#variable : [28] "avg6qty"
summary(Cleaned_telecom_data$avg6qty)
dat_avg6qty  <- Cleaned_telecom_data%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avg6qty$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat_avg6qty$churn_pred <- round(dat_avg6qty$n / dat_avg6qty$N , 2)
dat_avg6qty$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg6qty,n = 10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat_avg6qty$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avg6qty,n = 10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat_avg6qty$varname    <- rep("avg6qty", nrow(dat_avg6qty))
View(dat_avg6qty)

#variable : [37] "ag1"   ## variable factor conversion 
summary(Cleaned_telecom_data$age1)
dat_age1  <- Cleaned_telecom_data%>%mutate(dec=ntile(age1,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_age1$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(age1,n=10))%>%count(dec)%>%unname())[[2]]
dat_age1$churn_pred <- round(dat_age1$n / dat_age1$N , 2)
dat_age1$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(age1,n = 10))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat_age1$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(age1,n = 10))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat_age1$varname    <- rep("age1", nrow(dat_age1))
View(dat_age1)

#variable : [38] "ag2"   ## variable factor conversion
summary(Cleaned_telecom_data$age2)
dat_age2  <- Cleaned_telecom_data%>%mutate(dec=ntile(age2,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_age2$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(age2,n=10))%>%count(dec)%>%unname())[[2]]
dat_age2$churn_pred <- round(dat_age2$n / dat_age2$N , 2)
dat_age2$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(age2,n = 10))%>%group_by(dec)%>%summarise(min(age2)))[[2]]
dat_age2$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(age2,n = 10))%>%group_by(dec)%>%summarise(max(age2)))[[2]]
dat_age2$varname    <- rep("age2", nrow(dat_age2))
View(dat_age2)

#variable : [39] "models"   ## variable factor conversion
summary(Cleaned_telecom_data$models)
dat_models  <- Cleaned_telecom_data%>%mutate(dec=ntile(models,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_models$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(models,n=10))%>%count(dec)%>%unname())[[2]]
dat_models$churn_pred <- round(dat_models$n / dat_models$N , 2)
dat_models$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(models,n = 10))%>%group_by(dec)%>%summarise(min(models)))[[2]]
dat_models$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(models,n = 10))%>%group_by(dec)%>%summarise(max(models)))[[2]]
dat_models$varname    <- rep("models", nrow(dat_models))
View(dat_models)

#variable : [40] "hnd_price"   ## variable factor conversion
summary(Cleaned_telecom_data$hnd_price)
dat_hnd_price  <- Cleaned_telecom_data%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_hnd_price$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
dat_hnd_price$churn_pred <- round(dat_hnd_price$n / dat_hnd_price$N , 2)
dat_hnd_price$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(hnd_price,n = 10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat_hnd_price$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(hnd_price,n = 10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat_hnd_price$varname    <- rep("hnd_price", nrow(dat_hnd_price))
View(dat_hnd_price)


#variable : [41] "actvsubs"    ## variable factor conversion
summary(Cleaned_telecom_data$actvsubs)
dat_actvsubs  <- Cleaned_telecom_data%>%mutate(dec=ntile(actvsubs,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_actvsubs$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(actvsubs,n=10))%>%count(dec)%>%unname())[[2]]
dat_actvsubs$churn_pred <- round(dat_actvsubs$n / dat_actvsubs$N , 2)
dat_actvsubs$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(actvsubs,n = 10))%>%group_by(dec)%>%summarise(min(actvsubs)))[[2]]
dat_actvsubs$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(actvsubs,n = 10))%>%group_by(dec)%>%summarise(max(actvsubs)))[[2]]
dat_actvsubs$varname    <- rep("actvsubs", nrow(dat_actvsubs))
View(dat_actvsubs)


#variable : [42] "uniqsubs"     ## variable factor conversion
summary(Cleaned_telecom_data$uniqsubs)
dat_uniqsubs  <- Cleaned_telecom_data%>%mutate(dec=ntile(uniqsubs,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_uniqsubs$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(uniqsubs,n=10))%>%count(dec)%>%unname())[[2]]
dat_uniqsubs$churn_pred <- round(dat_uniqsubs$n / dat_uniqsubs$N , 2)
dat_uniqsubs$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(uniqsubs,n = 10))%>%group_by(dec)%>%summarise(min(uniqsubs)))[[2]]
dat_uniqsubs$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(uniqsubs,n = 10))%>%group_by(dec)%>%summarise(max(uniqsubs)))[[2]]
dat_uniqsubs$varname    <- rep("uniqsubs", nrow(dat_uniqsubs))
View(dat_uniqsubs)

#variable : [43] "forgntvl"     ## variable factor conversion
summary(Cleaned_telecom_data$forgntvl)
dat_forgntvl  <- Cleaned_telecom_data%>%mutate(dec=ntile(forgntvl,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_forgntvl$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(forgntvl,n=10))%>%count(dec)%>%unname())[[2]]
dat_forgntvl$churn_pred <- round(dat_forgntvl$n / dat_forgntvl$N , 2)
dat_forgntvl$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(forgntvl,n = 10))%>%group_by(dec)%>%summarise(min(forgntvl)))[[2]]
dat_forgntvl$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(forgntvl,n = 10))%>%group_by(dec)%>%summarise(max(forgntvl)))[[2]]
dat_forgntvl$varname    <- rep("forgntvl", nrow(dat_forgntvl))
View(dat_forgntvl)


#variable : [45] "mtrcycle"     ## variable factor conversion
summary(Cleaned_telecom_data$mtrcycle)    #omit this variable as decile having less data
dat_mtrcycle <- Cleaned_telecom_data%>%mutate(dec=ntile(mtrcycle,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_mtrcycle$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mtrcycle,n=10))%>%count(dec)%>%unname())[[2]]
dat_mtrcycle$churn_pred <- round(dat_mtrcycle$n / dat_mtrcycle$N , 2)
dat_mtrcycle$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mtrcycle,n = 10))%>%group_by(dec)%>%summarise(min(mtrcycle)))[[2]]
dat_mtrcycle$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(mtrcycle,n = 10))%>%group_by(dec)%>%summarise(max(mtrcycle)))[[2]]
dat_mtrcycle$varname    <- rep("mtrcycle", nrow(dat_mtrcycle))
View(dat_mtrcycle)

#variable : [46] "truck"      ## variable factor conversion
summary(Cleaned_telecom_data$truck)    #omit this variable as decile having less data
dat_truck <- Cleaned_telecom_data%>%mutate(dec=ntile(truck,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_truck$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(truck,n=10))%>%count(dec)%>%unname())[[2]]
dat_truck$churn_pred <- round(dat_mtrcycle$n / dat_mtrcycle$N , 2)
dat_truck$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(truck,n = 10))%>%group_by(dec)%>%summarise(min(truck)))[[2]]
dat_truck$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(truck,n = 10))%>%group_by(dec)%>%summarise(max(truck)))[[2]]
dat_truck$varname    <- rep("truck", nrow(dat_truck))
View(dat_truck)


#variable : [49] "blck_dat_Mean"    
summary(Cleaned_telecom_data$blck_dat_Mean)
dat_blck_dat_Mean <- Cleaned_telecom_data%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_blck_dat_Mean$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_blck_dat_Mean$churn_pred <- round(dat_blck_dat_Mean$n / dat_blck_dat_Mean$N , 2)
dat_blck_dat_Mean$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(blck_dat_Mean,n = 10))%>%group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]
dat_blck_dat_Mean$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(blck_dat_Mean,n = 10))%>%group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]
dat_blck_dat_Mean$varname    <- rep("blck_dat_Mean", nrow(dat_blck_dat_Mean))
View(dat_blck_dat_Mean)


#variable : [59] "drop_vce_Mean"   
summary(Cleaned_telecom_data$drop_vce_Mean)
dat_drop_vce_Mean  <- Cleaned_telecom_data%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_drop_vce_Mean$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_drop_vce_Mean$churn_pred <- round(dat_drop_vce_Mean$n / dat_drop_vce_Mean$N , 2)
dat_drop_vce_Mean$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(drop_vce_Mean,n = 10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat_drop_vce_Mean$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(drop_vce_Mean,n = 10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat_drop_vce_Mean$varname    <- rep("drop_vce_Mean", nrow(dat_drop_vce_Mean))
View(dat_drop_vce_Mean)

#variable : [60] "adjmou"
summary(Cleaned_telecom_data$adjmou)
dat_adjmou  <- Cleaned_telecom_data%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_adjmou$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat_adjmou$churn_pred <- round(dat_adjmou$n / dat_adjmou$N , 2)
dat_adjmou$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(adjmou,n = 10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat_adjmou$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(adjmou,n = 10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat_adjmou$varname    <- rep("adjmou", nrow(dat_adjmou))
View(dat_adjmou)

#variable : [61] "totrev"
summary(Cleaned_telecom_data$totrev)
dat_totrev <- Cleaned_telecom_data%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_totrev$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat_totrev$churn_pred <- round(dat_totrev$n / dat_totrev$N , 2)
dat_totrev$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(totrev,n = 10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat_totrev$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(totrev,n = 10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat_totrev$varname    <- rep("totrev", nrow(dat_totrev))
View(dat_totrev)

#variable : [62] "adjrev"
summary(Cleaned_telecom_data$adjrev)
dat_adjrev <- Cleaned_telecom_data%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_adjrev$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat_adjrev$churn_pred <- round(dat_adjrev$n / dat_adjrev$N , 2)
dat_adjrev$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(adjrev,n = 10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat_adjrev$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(adjrev,n = 10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat_adjrev$varname    <- rep("adjrev", nrow(dat_adjrev))
View(dat_adjrev)

#variable : [63] "avgrev"
summary(Cleaned_telecom_data$avgrev)
dat_avgrev <- Cleaned_telecom_data%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat_avgrev$N  <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat_avgrev$churn_pred <- round(dat_avgrev$n / dat_avgrev$N , 2)
dat_avgrev$greater    <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avgrev,n = 10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat_avgrev$lesser     <- unclass(Cleaned_telecom_data%>%mutate(dec=ntile(avgrev,n = 10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat_avgrev$varname    <- rep("avgrev", nrow(dat_avgrev))
View(dat_avgrev)



##Below variables Getting Less Deciles; can Omit this
#variable :              [12] "custcare_Mean", [13] "callwait_Mean", [14] "iwylis_vce_Mean", [15] "callwait_Range" 
#,[16] "ccrndmou_Range", [20] "ovrmou_Mean",   [44] "opk_dat_Mean",       
#,[47] "roam_Mean",      [48] "recv_sms_Mean", [50] "mou_pead_Mean", [54] "da_Mean"           
#,[55] "da_Range",       [56] "datovr_Mean",   [57] "datovr_Range",  [58] "drop_dat_Mean",   [65] "retdays_1" 

#Later this can be factor converted
#[37] "age1",           [38] "age2" ,    [39] "models", [40] "hnd_price"
#[41] "actvsubs",       [42] "uniqsubs", [43] "forgntvl", [45] "mtrcycle", [46] "truck"  


#Add above created objects to create data Object for telecom Study
data_continous_variable<- rbind(dat_mou_mean, dat_totmrc_mean, dat_rev_range, dat_mou_range,
                                            dat_change_mou, dat_drop_vce_Range, dat_owylis_vce_Range,
                                            dat_mou_opkv_Range, dat_months, dat_totcalls, dat_eqpdays,
                                           dat_adjqty, dat_ovrrev_Mean, dat_rev_Mean, dat_comp_vce_Mean,
                                            dat_plcd_vce_Mean, dat_avg3mou, dat_avgmou, dat_avg3qty, dat_avgqty,
                                            dat_avg6mou, dat_avg6qty, dat_blck_dat_Mean, dat_drop_vce_Mean,
                                            dat_adjmou, dat_totrev, dat_adjrev, dat_avgrev)
View(data_continous_variable)

#If needed we can Omit columns based on dicile output to avoid insignificant info in model;
names(Cleaned_telecom_data) #Total 65
str(Cleaned_telecom_data) 
telecom_Cleaned_deciled <- Cleaned_telecom_data[, -c(12:16,20,44,47:48,50,54:58)] 
names(telecom_Cleaned_deciled) #Total 50 (Dropped 15)
View(telecom_Cleaned_deciled)

##Deciling Categorical Variables; finding out event rate (churn rate in this case)

#variable :  [29] "crclscod" 
summary(Cleaned_telecom_data$crclscod)
dat_crclscod             <- Cleaned_telecom_data%>%count(churn,levels=crclscod)%>%filter(churn==1)
dat_crclscod$N           <- unclass(Cleaned_telecom_data%>%filter(crclscod%in%dat_crclscod$levels)%>%count(crclscod))[[2]]
dat_crclscod$ChurnPerc   <- dat_crclscod$n/dat_crclscod$N
dat_crclscod$Var.Name    <- rep("crclscod",nrow(dat_crclscod))
View(dat_crclscod)

#variable :  [30] "asl_flag" 
summary(Cleaned_telecom_data$asl_flag)
Cleaned_telecom_data$asl_flag<- ifelse(Cleaned_telecom_data$asl_flag== "Y", 1, 0)
dat_asl_flag            <- Cleaned_telecom_data%>%count(churn,levels=asl_flag)%>%filter(churn==1)
dat_asl_flag$N          <- unclass(Cleaned_telecom_data%>%filter(asl_flag%in%dat_asl_flag$levels)%>%count(asl_flag))[[2]]
dat_asl_flag$ChurnPerc  <- dat_asl_flag$n/dat_asl_flag$N
dat_asl_flag$Var.Name   <- rep("asl_flag",nrow(dat_asl_flag))
View(dat_asl_flag)



#variable :  [31] "prizm_social_one" 
summary(Cleaned_telecom_data$prizm_social_one)
dat_prizm_social_one <- Cleaned_telecom_data%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)
dat_prizm_social_one$N<-unclass(Cleaned_telecom_data%>%filter(prizm_social_one%in%dat_prizm_social_one$levels)%>%count(prizm_social_one))[[2]]
dat_prizm_social_one$ChurnPerc<-dat_prizm_social_one$n/dat_prizm_social_one$N
dat_prizm_social_one$Var.Name<-rep("prizm_social_one",nrow(dat_prizm_social_one))
View(dat_prizm_social_one)

#variable :  [32] "area"  
summary(Cleaned_telecom_data$area)
dat_area <- Cleaned_telecom_data%>%count(churn,levels=area)%>%filter(churn==1)
dat_area$N<-unclass(Cleaned_telecom_data%>%filter(area%in%dat_area$levels)%>%count(area))[[2]]
dat_area$ChurnPerc<-dat_area$n/dat_area$N
dat_area$Var.Name<-rep("area",nrow(dat_area))
View(dat_area)

#variable :  [33] "refurb_new" 
summary(Cleaned_telecom_data$refurb_new)
Cleaned_telecom_data$refurb_new <- ifelse(Cleaned_telecom_data$refurb_new == "R",1,0)
dat_refurb_new <- Cleaned_telecom_data%>%count(churn,levels=refurb_new)%>%filter(churn==1)
dat_refurb_new$N<-unclass(Cleaned_telecom_data%>%filter(refurb_new%in%dat_refurb_new$levels)%>%count(refurb_new))[[2]]
dat_refurb_new$ChurnPerc<-dat_refurb_new$n/dat_refurb_new$N
dat_refurb_new$Var.Name<-rep("refurb_new",nrow(dat_refurb_new))
View(dat_refurb_new)

#variable :  [34] "hnd_webcap"
summary(Cleaned_telecom_data$hnd_webcap)
dat_hnd_webcap <- Cleaned_telecom_data%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)
dat_hnd_webcap$N<-unclass(Cleaned_telecom_data%>%filter(hnd_webcap%in%dat_hnd_webcap$levels)%>%count(hnd_webcap))[[2]]
dat_hnd_webcap$ChurnPerc<-dat_hnd_webcap$n/dat_hnd_webcap$N
dat_hnd_webcap$Var.Name<-rep("hnd_webcap",nrow(dat_hnd_webcap))
View(dat_hnd_webcap)

#variable :  [35] "marital"    
summary(Cleaned_telecom_data$marital)
dat_marital <- Cleaned_telecom_data%>%count(churn,levels=marital)%>%filter(churn==1)
dat_marital$N<-unclass(Cleaned_telecom_data%>%filter(marital%in%dat_marital$levels)%>%count(marital))[[2]]
dat_marital$ChurnPerc<-dat_marital$n/dat_marital$N
dat_marital$Var.Name<-rep("marital",nrow(dat_marital))
View(dat_marital)

#variable :  [36] "ethnic" 
summary(Cleaned_telecom_data$ethnic)
dat_ethnic <- Cleaned_telecom_data%>%count(churn,levels=ethnic)%>%filter(churn==1)
dat_ethnic$N<-unclass(Cleaned_telecom_data%>%filter(ethnic%in%dat_ethnic$levels)%>%count(ethnic))[[2]]
dat_ethnic$ChurnPerc<-dat_ethnic$n/dat_ethnic$N
dat_ethnic$Var.Name<-rep("ethnic",nrow(dat_ethnic))
View(dat_ethnic)

#variable :  [52] "car_buy" 
summary(Cleaned_telecom_data$car_buy)
dat_car_buy <- Cleaned_telecom_data%>%count(churn,levels=car_buy)%>%filter(churn==1)
dat_car_buy$N<-unclass(Cleaned_telecom_data%>%filter(car_buy%in%dat_car_buy$levels)%>%count(car_buy))[[2]]
dat_car_buy$ChurnPerc<-dat_car_buy$n/dat_car_buy$N
dat_car_buy$Var.Name<-rep("car_buy",nrow(dat_car_buy))
View(dat_car_buy)

#variable :  [53] "csa" 
summary(Cleaned_telecom_data$csa)
dat_csa <- Cleaned_telecom_data%>%count(churn,levels=csa)%>%filter(churn==1)
dat_csa$N<-unclass(Cleaned_telecom_data%>%filter(csa%in%dat_csa$levels)%>%count(csa))[[2]]
dat_csa$ChurnPerc<-dat_csa$n/dat_csa$N
dat_csa$Var.Name<-rep("csa",nrow(dat_csa))
View(dat_csa)

#variable : [65] "retdays_1"
summary(Cleaned_telecom_data$retdays_1)
dat_retdays_1 <- Cleaned_telecom_data%>%count(churn,levels=retdays_1)%>%filter(churn==1)
dat_retdays_1$N<-unclass(Cleaned_telecom_data%>%filter(retdays_1%in%dat_retdays_1$levels)%>%count(retdays_1))[[2]]
dat_retdays_1$ChurnPerc<-dat_retdays_1$n/dat_retdays_1$N
dat_retdays_1$Var.Name<-rep("retdays_1",nrow(dat_retdays_1))
View(dat_retdays_1)

#dataset of categorical variable from telecom dataset for further analysis
data_categorical_variable<- rbind.data.frame(dat_crclscod, dat_asl_flag, dat_prizm_social_one,
                                              dat_area, dat_refurb_new, dat_hnd_webcap, dat_marital,
                                              dat_ethnic, dat_car_buy, dat_csa, dat_retdays_1)



# #vaiables for data Preparations step ------------------------------------

telecom_Cleaned_deciled_Data_Preparation<- telecom_Cleaned_deciled

#Outlier Identifying 
#outliers from continuous variable of deciled cleaned telecom dataset
variable_index<-names(telecom_Cleaned_deciled_Data_Preparation)
 
continuous_variable_outliers<- telecom_Cleaned_deciled_Data_Preparation[,c(1:22,31:41,44:49)]
categorical_variable_outliers<-telecom_Cleaned_deciled_Data_Preparation[,c(23:30,42:43,50)]

par(mfrow=c(2,4)) 
for(i in 1:length(continuous_variable_outliers))
{
  boxplot(continuous_variable_outliers[,i], main = names(continuous_variable_outliers[i]), type="l")
}


#Outlier Check
for(i in 1:length(continuous_variable_outliers))
{
  plot(continuous_variable_outliers[,i], main = names(continuous_variable_outliers[i]), type = "l")
}


#Imputation of Outlier.

#library(outliers)
for (i in 1:ncol(continuous_variable_outliers)) {
  continuous_variable_outliers[,i] = ifelse(outlier(continuous_variable_outliers[,i], logical = TRUE),
                                            median(continuous_variable_outliers[,i], FUN = function(x) median(x, na.rm = TRUE)),
                                            continuous_variable_outliers[,i])
}

#Check Outlier after imputation
par(mfrow=c(2,4)) 
for(i in 1:length(continuous_variable_outliers))
{
  boxplot(continuous_variable_outliers[,i], main = names(continuous_variable_outliers[i]), type="l")
}

missing<-colSums(is.na(telecom_Cleaned_deciled_Data_Preparation))
View(missing)      #15 continous variable has Missing values
                  # 5 categorical variable has missing value

#Remove first 5 variables
Missing_value <- which(is.na(telecom_Cleaned_deciled_Data_Preparation[,c(1:5)]))
telecom_Cleaned_deciled_Data_Preparation <- telecom_Cleaned_deciled_Data_Preparation[-Missing_value,]
summary(telecom_Cleaned_deciled_Data_Preparation)

#remove from variable change_mou
Missing_value1 <- which(is.na(telecom_Cleaned_deciled_Data_Preparation$change_mou))
telecom_Cleaned_deciled_Data_Preparation <- telecom_Cleaned_deciled_Data_Preparation[-Missing_value1,]

#remove from variable area
Missing_value2 <- which(is.na(telecom_Cleaned_deciled_Data_Preparation$area))
telecom_Cleaned_deciled_Data_Preparation <- telecom_Cleaned_deciled_Data_Preparation[-Missing_value2,]

#remove from variable change_mou
Missing_value1 <- which(is.na(telecom_Cleaned_deciled_Data_Preparation$change_mou))
telecom_Cleaned_deciled_Data_Preparation <- telecom_Cleaned_deciled_Data_Preparation[-Missing_value1,]

##impute mean of column in variable avg6mou
telecom_Cleaned_deciled_Data_Preparation$avg6mou[is.na(telecom_Cleaned_deciled_Data_Preparation$avg6mou)]     <- mean(telecom_Cleaned_deciled_Data_Preparation$avg6mou,na.rm=T)

#impute mean of column in variable avg6qty
telecom_Cleaned_deciled_Data_Preparation$avg6qty[is.na(telecom_Cleaned_deciled_Data_Preparation$avg6qty)]     <- mean(telecom_Cleaned_deciled_Data_Preparation$avg6qty,na.rm=T)

#impute mean of column in variable hnd_price
telecom_Cleaned_deciled_Data_Preparation$hnd_price[is.na(telecom_Cleaned_deciled_Data_Preparation$hnd_price)] <- mean(telecom_Cleaned_deciled_Data_Preparation$hnd_price,na.rm=T)

#remove from variable marital
Missing_value3 <- which(is.na(telecom_Cleaned_deciled_Data_Preparation$marital))
telecom_Cleaned_deciled_Data_Preparation <- telecom_Cleaned_deciled_Data_Preparation[-Missing_value3,]

#Pending variables : prizm_social_one, hnd_webcap (Create category "Missing")

telecom_Cleaned_deciled_Data_Preparation$prizm_social_one_Dummy <- ifelse(is.na(telecom_Cleaned_deciled_Data_Preparation$prizm_social_one), "Missing", as.factor(telecom_Cleaned_deciled_Data_Preparation$prizm_social_one))
telecom_Cleaned_deciled_Data_Preparation$prizm_social_one_Dummy <- as.factor(telecom_Cleaned_deciled_Data_Preparation$prizm_social_one_Dummy)
telecom_Cleaned_deciled_Data_Preparation$prizm_social_one_Dummy <- factor(telecom_Cleaned_deciled_Data_Preparation$prizm_social_one_Dummy, labels= c("C","R","S","T","U","Missing"))
#summary(telecom_Cleaned_deciled_Data_Preparation$prizm_social_one)
#summary(telecom_Cleaned_deciled_Data_Preparation$prizm_social_one_Dummy)

telecom_Cleaned_deciled_Data_Preparation$hnd_webcap_Dummy <- ifelse(is.na(telecom_Cleaned_deciled_Data_Preparation$hnd_webcap), "Missing", as.factor(telecom_Cleaned_deciled_Data_Preparation$hnd_webcap))
telecom_Cleaned_deciled_Data_Preparation$hnd_webcap_Dummy <- as.factor(telecom_Cleaned_deciled_Data_Preparation$hnd_webcap_Dummy)
telecom_Cleaned_deciled_Data_Preparation$hnd_webcap_Dummy <- factor(telecom_Cleaned_deciled_Data_Preparation$hnd_webcap_Dummy, labels= c("UNKW","WC","WCMB","Missing"))
#summary(telecom_Cleaned_deciled_Data_Preparation$hnd_webcap)
#summary(telecom_Cleaned_deciled_Data_Preparation$hnd_webcap_Dummy)

#Remove Duplicate with Dummy's
names(telecom_Cleaned_deciled_Data_Preparation)
telecom_Cleaned_deciled_Data_Preparation <- telecom_Cleaned_deciled_Data_Preparation[, -c(25,28)]
names(telecom_Cleaned_deciled_Data_Preparation)

# check any other missing value
missing<-colSums(is.na(telecom_Cleaned_deciled_Data_Preparation))
View(missing) 


#Create Few more Dummy Variable and convert some variable to Vector for further process
#convert age1 variable into category of young, mid age, senior
telecom_Cleaned_deciled_Data_Preparation$age1_Dummy <- ifelse(telecom_Cleaned_deciled_Data_Preparation$age1==0,"None",
                                                        ifelse(telecom_Cleaned_deciled_Data_Preparation$age1<=30,"Young",
                                                               ifelse(telecom_Cleaned_deciled_Data_Preparation$age1>30 & telecom_Cleaned_deciled_Data_Preparation$age1<=55,"Mid Age","Senior")))
telecom_Cleaned_deciled_Data_Preparation$age1_Dummy <- as.factor(telecom_Cleaned_deciled_Data_Preparation$age1_Dummy)

#convert age2 variable into category of young, mid age, senior
telecom_Cleaned_deciled_Data_Preparation$age2_Dummy <- ifelse(telecom_Cleaned_deciled_Data_Preparation$age2==0,"None",
                                                        ifelse(telecom_Cleaned_deciled_Data_Preparation$age2<=30,"Young",
                                                               ifelse(telecom_Cleaned_deciled_Data_Preparation$age2>30 & telecom_Cleaned_deciled_Data_Preparation$age2<=55,"Mid Age","Senior")))
telecom_Cleaned_deciled_Data_Preparation$age2_Dummy <- as.factor(telecom_Cleaned_deciled_Data_Preparation$age2_Dummy)

names(telecom_Cleaned_deciled_Data_Preparation)
#Remove Duplicate Variable with Dummy Creation
telecom_Cleaned_deciled_Data_Preparation <- telecom_Cleaned_deciled_Data_Preparation[, -c(29,30)]

#Vector Convertion in data type for further calculation
telecom_Cleaned_deciled_Data_Preparation$models    <- as.factor(telecom_Cleaned_deciled_Data_Preparation$models)
telecom_Cleaned_deciled_Data_Preparation$hnd_price <- as.factor(telecom_Cleaned_deciled_Data_Preparation$hnd_price)
telecom_Cleaned_deciled_Data_Preparation$actvsubs  <- as.factor(telecom_Cleaned_deciled_Data_Preparation$actvsubs)
telecom_Cleaned_deciled_Data_Preparation$uniqsubs  <- as.factor(telecom_Cleaned_deciled_Data_Preparation$uniqsubs)
telecom_Cleaned_deciled_Data_Preparation$forgntvl  <- as.factor(telecom_Cleaned_deciled_Data_Preparation$forgntvl)
telecom_Cleaned_deciled_Data_Preparation$mtrcycle  <- as.factor(telecom_Cleaned_deciled_Data_Preparation$mtrcycle)
telecom_Cleaned_deciled_Data_Preparation$truck     <- as.factor(telecom_Cleaned_deciled_Data_Preparation$truck)

str(telecom_Cleaned_deciled_Data_Preparation)



# Built Logistic model churn rate -----------------------------------------
telecom_churn_data_model<- telecom_Cleaned_deciled_Data_Preparation
summary(telecom_churn_data_model)
str(telecom_churn_data_model)
names(telecom_churn_data_model)

telecom_churn_data_mode2<- telecom_churn_data_model[,-39]   # remove variable csa which has large factor and not converging while building model

#Splitting dataset sample into Traning and Testing Samples
set.seed(300)
index <- sample(nrow(telecom_churn_data_mode2), 0.70*nrow(telecom_churn_data_mode2),replace=F)
training_data <- telecom_churn_data_mode2[index,]
testing_data  <- telecom_churn_data_mode2[-index,]

#Quick Check with Churn Rate
table(training_data$churn) / nrow(training_data)
table(testing_data$churn) / nrow(testing_data)


#Build Logistic Regression Model
ModelStage1 <- glm(formula = churn~., family="binomial", data=training_data)

summary(ModelStage1)

#using stepwise function to identify sequence and significant variable.
stepAIC(ModelStage1)

#built model as stepwise function 
ModelStage2<- glm(formula = churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + 
                    owylis_vce_Range + months + totcalls + eqpdays + ovrrev_Mean + 
                    rev_Mean + comp_vce_Mean + avg3mou + avgmou + asl_flag + 
                    ethnic + hnd_price + uniqsubs + blck_dat_Mean + drop_vce_Mean + 
                    adjmou + totrev + adjrev + retdays_1 + age1_Dummy, family = "binomial", 
                  data = training_data)
summary(ModelStage2)

#Some More Dummy Variable preparation Here to put in section for significant level
View(telecom_churn_data_mode2)


training_data$ethnic_O  <- ifelse(training_data$ethnic == "O",1,0)
testing_data$ethnic_O  <- ifelse(testing_data$ethnic == "O",1,0)
table(training_data$ethnic_O)

training_data$hnd_price_199.98  <- ifelse(training_data$hnd_price == "199.9899902",1,0)
testing_data$hnd_price_199.98   <- ifelse(testing_data$hnd_price == "199.9899902",1,0)

training_data$hnd_price_249.98  <- ifelse(training_data$hnd_price == "249.9899902",1,0)
testing_data$hnd_price_249.98   <- ifelse(testing_data$hnd_price == "249.9899902",1,0)

training_data$uniqsubs_2  <- ifelse(training_data$uniqsubs == "2",1,0)
testing_data$uniqsubs_2   <- ifelse(testing_data$uniqsubs == "2",1,0)

training_data$uniqsubs_3  <- ifelse(training_data$uniqsubs == "3",1,0)
testing_data$uniqsubs_3   <- ifelse(testing_data$uniqsubs == "3",1,0)

training_data$uniqsubs_4  <- ifelse(training_data$uniqsubs == "4",1,0)
testing_data$uniqsubs_4   <- ifelse(testing_data$uniqsubs == "4",1,0)

summary(training_data$age1_Dummy)
training_data$age1_Dummy_Senior  <- ifelse(training_data$age1_Dummy == "Senior",1,0)
testing_data$age1_Dummy_Senior   <- ifelse(testing_data$age1_Dummy == "Senior",1,0)

training_data$age1_Dummy_none  <- ifelse(training_data$age1_Dummy == "None",1,0)
testing_data$age1_Dummy_none   <- ifelse(testing_data$age1_Dummy == "None",1,0)


#rebuilt the model with the factor variable
attach(training_data)
ModelStage3<- glm(formula = churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + 
                    owylis_vce_Range + months + totcalls + eqpdays + ovrrev_Mean + 
                    rev_Mean + comp_vce_Mean + avg3mou + avgmou + asl_flag + 
                    ethnic_O + hnd_price_199.98 + hnd_price_249.98 + uniqsubs_2 + 
                    uniqsubs_3 + uniqsubs_4  + drop_vce_Mean + 
                    adjmou + totrev + adjrev + retdays_1 + age1_Dummy_none + age1_Dummy_Senior, family = "binomial", 
                  data = training_data)
summary(ModelStage3)

#All the variables looks Significant
#Model Diagnostics
#Multicollinearity Check
vif(ModelStage3)

##Confidence interval
confint(ModelStage3)

##Model testing
##Predict Probability of customer Churning

predict_CustomerChurn <- predict(ModelStage3, type="response", newdata=testing_data)
head(predict_CustomerChurn)

#Cut off probability as per Churn Rate
table(telecom_churn_data_mode2$churn) / nrow(telecom_churn_data_mode2)

predictionChurn <- ifelse(predict_CustomerChurn >= 0.234428,1,0)
table(predictionChurn)

#Check Prediction Quality

kappa2(data.frame(testing_data$churn, predictionChurn))
table(testing_data$churn)

#Confusion Matrix

table(predictionChurn, testing_data$churn)
table(testing_data$churn)
#predictionChurn    0    1
#               0 1777  418
#               1 1172  525

#ROCR Curve
predictionRocr <- prediction(predictionChurn, testing_data$churn)
performanceRocr <- performance(predictionRocr, "tpr", "fpr")
plot(performanceRocr, col="red")
abline(0,1,lty=8,col="grey")
rocrAccuracy <- performance(predictionRocr,"auc")
rocrAccuracy <- unlist(slot(rocrAccuracy,"y.values"))
rocrAccuracy  #0.58

#The Auc is 0.58 and which is More than 0.50, Also The curve is above the gray Line. 
#Model Looks like Fine and Acceptable

#Gains Chart
gains(testing_data$churn, predict(ModelStage3, type="response", newdata=testing_data), groups = 10)

#Chart table shows that top 40% probability contains more than 50% (52.1) Likely to Churn.


testing_data$predictedvaluesCutoff <- predict(ModelStage3, type="response", newdata = testing_data)

quantile(testing_data$predictedvaluesCutoff, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#Top 50% of the probability is between 0.2050408 - 0.2236484 #By using this data we can predict who highly likely with Churn.



# Question capstone project -----------------------------------------------



#1. What are the top five factors driving likelihood of churn at Telecom Company?

head(sort(abs(ModelStage3$coefficients),decreasing = T),500)

#ANSWER:-

#<Top 5 Factors>    <Beta Coefficient>
#hnd_price_249.98    1.94791677312
#retdays_1           0.91358996774
#ethnic_O            0.46596786873
#hnd_price_199.98    0.33808226166
#uniqsubs_4          0.30683087763


#2. Validation of survey findings. a) Whether "cost and billing" and "network and service quality" are
#important factors influencing churn behaviour. b) Are data usage connectivity issues turning out to be
#costly? In other words, is it leading to churn? 

#ANSWER:-
#"cost and billing" Factors
#totmrc_Mean          -0.00560419
#ovrrev_Mean           0.00356835
#totrev                0.00129417
#rev_Range            -0.00112214 


#"network and service quality" factors
                   
#change_mou            -0.00025968
#owylis_vce_Range       0.00292270
#drop_vce_Mean          0.00646137  
#retdays_Dummy1         0.91358997

#A) Yes, the beta coefficient of variables is showing  factor influencing churn increase/decrease.
#that is considering with customers join date from analyse day how he/she makes a call to retent. Customer chance of churn is high.
#the factor value helped customer need to be focused to give offers to retain.

#No data usage factor

#hence

#B) No, No variables is showing a very important factor for churn behaviour influence. 
#If we notice above mentioned are beta values and a unit increase is them is not impacting any on churn.

#USeage for data is not affecting and looks anaysys have less data; 
#Not Leading very much with abve data % interms to Churn


#3. Would you recommend rate plan migration as a proactive retention strategy?

#Yes, But for few not all
#Reason : Variable Ovrrev Mean is with 0.004 coEff; Its a Mean Overage Revenue in business
# CoEff not strong to show churn activitys


#4. What would be your recommendation on how to use this churn model for prioritisation of customers for a
#proactive retention campaigns in the future?


#Gains Chart
gains(testing_data$churn, predict(ModelStage3, type="response", newdata=testing_data), groups = 10)

#Chart table shows that top 40% probability contains more than 50% (52.1) Likely to Churn.


testing_data$predictedvaluesCutoff <- predict(ModelStage3, type="response", newdata = testing_data)

quantile(testing_data$predictedvaluesCutoff, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
#10%       20%       30%       40%       50%       60%       70%       80%       90% 
#0.1392156 0.1660142 0.1864208 0.2050408 0.2236484 0.2409383 0.2604427 0.2847985 0.3245924 
#100% 
#0.8812331 

# 20% of the probability is between 0.2847985 - 0.8812331 #By using this data we can predict who highly likely with Churn.

##Predict a Customer Who will Churn
## Apply Cut-off

customerPrediction <- predict(ModelStage3, type="response", newdata=testing_data)
customerPrediction <- ifelse(customerPrediction >= 0.3010562 , 1, 0)
table(customerPrediction, testing$churn)
#customerPrediction    0    1
#                  0 2567  764
#                  1  425  136

#take upto <from 80 to 100(20)> as per quantile result 
#and consider churn value(0 or 1 : here 1) then to predict customer use unique vaiable to identify via Customer_ID
expectedPrediction <- testing_data[testing_data$highChurnProbability > 0.2847985 & 
                                testing_data$highChurnProbability <= 0.8812331 & 
                                testing_data$churn == "0","Customer_ID"]
expectedPrediction <- as.data.frame(expectedPrediction)
nrow(expectedPrediction) #1351 Customers
table(testing_data$churn)


### Model ModelStage3 is used to predict list of Telecom Company customers who highly likely churn.
# Unique list can be prepared using Customer_ID



# 5. What would be the target segments for proactive retention campaigns? Falling ARPU forecast is also a
# concern and therefore, Telecom Company would like to save their high revenue customers besides managing
# churn. Given a budget constraint of a contact list of 20% of the subscriber pool, which subscribers should
# prioritized if "revenue saves" is also a priority besides controlling churn. In other words, controlling churn
# is the primary objective and revenue saves is the secondary objective.


## Get the Probability Score and Revenue value rate
saveRevenueCustomers <- predict(ModelStage3, type="response",newdata=testing_data)
testing_data$highRevenueCustomers <- predict(ModelStage3,type="response",newdata=testing_data)
quantile(testing_data$highRevenueCustomers, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

savehighRevenueCustomers <- ifelse(saveRevenueCustomers < 0.20, "Low Probability", 
                                   ifelse(saveRevenueCustomers >= 0.20 & saveRevenueCustomers < 0.30, "Medium Probability", "High Probability"))

table(savehighRevenueCustomers, testing_data$churn)

#savehighRevenueCustomers    0    1
#High Probability           365  210
#Low Probability           1211  237
#Medium Probability        1373  496

str(testing_data$totrev)
quantile(testing_data$totrev, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
revenueValueRate <-  ifelse(testing_data$totrev < 677.796, "Low Revenue", 
                            ifelse(testing_data$totrev >= 677.796 & testing_data$totrev < 1148.073, "Medium Revenue", "High Revenue"))

table(savehighRevenueCustomers,revenueValueRate)

#                        revenueValueRate
#savehighRevenueCustomers High Revenue Low Revenue Medium Revenue
#High Probability            231         137            207
#Low Probability             493         585            370
#Medium Probability          444         835            590

## This Result can help to select the level of customer need to be targeted 

## Extract List

summary(savehighRevenueCustomers)
summary(revenueValueRate)

#put it in testing Data Set
testing_data$ProbabilityRange <- ifelse(saveRevenueCustomers < 0.20, "Low Probability", 
                                   ifelse(saveRevenueCustomers >= 0.20 & saveRevenueCustomers < 0.30, "Medium Probability", "High Probability"))
testing_data$RevenueRange <-  ifelse(testing$totrev < 677.796, "Low Revenue", 
                                ifelse(testing$totrev >= 677.796 & testing$totrev < 1148.073, "Medium Revenue", "High Revenue"))


TargetedCustomersforHighRevenue <- testing_data[testing_data$ProbabilityRange == "High Probability" & testing$RevenueRange == "High Revenue","Customer_ID"]
TargetedCustomersforHighRevenue<- as.data.frame(TargetedCustomersforHighRevenue)
nrow(TargetedCustomersforHighRevenue) #1385 Customers

#Predicted Customers who likely to be targetted with offers and etc etc to retain for High Revenue 
write.csv(TargetedCustomersforHighRevenue, "Predicted Customers who likely to retain for High Revenue.csv", row.names = T)
