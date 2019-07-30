#Load data

#Require packages

require(dlm)
require(ggplot2)
require(plyr)
require(tidyr)
require(yarr)

#set working directory

#setwd("G:/Mijn Drive/Projecten/u-can-act/Data/Data 30-4-2019/")
setwd("G:/My Drive/Projecten/u-can-act/Data/Data 30-4-2019/")


#read data from csv file into data.frame (can be necessary to change the decimal setting of Dutch excel from "," to "." !!!)
diary_control_student<-read.csv2("responses_dagboek_studenten_controle_2019-01-16.csv", sep=";", na.strings = "NA")
diary_risk_student<-read.csv2("responses_dagboek_studenten_2019-01-16.csv", sep=";", na.strings = "NA")

#1 creating dataframe
#1.1 #add column to deviate the risk group from the control group
diary_risk_student$group <- "risk_student"
diary_control_student$group <-"control_student"

#bind risk and control student dataframe
diary_all<-rbind.fill(diary_risk_student, diary_control_student)

#change class of column to numeric
diary_all$v3<-as.numeric(diary_all$v3)
diary_all$v4<-as.numeric(diary_all$v4)
diary_all$v5<-as.numeric(diary_all$v5)
diary_all$v6<-as.numeric(diary_all$v6)
diary_all$v14<-as.numeric(diary_all$v14)
diary_all$v15<-as.numeric(diary_all$v15)
diary_all$v16<-as.numeric(diary_all$v16)
diary_all$v17<-as.numeric(diary_all$v17)
diary_all$v25<-as.numeric(diary_all$v25)

#data frame with only variables of interest
#A C Rfriends Rteachers school = v3,v4,v5,v6;
#A C Rfriends Rparents non school = v14,v15,v16,v17;

var_of_interest<- data.frame(
  "filled_out_by_id" =diary_all$filled_out_by_id,
  "open_from" =diary_all$open_from,
  "completed_at" =diary_all$completed_at,
  "group" =diary_all$group,
  "A_school"= diary_all$v3,
  "C_school"= diary_all$v4,
  "Rfriends_school"= diary_all$v5,
  "Rteachers_school"= diary_all$v6,
  "A_nonschool"= diary_all$v14,
  "C_nonschool"= diary_all$v15,
  "Rfriends_nonschool"= diary_all$v16,
  "Rparents_nonschool"= diary_all$v17)

#create function which correlates all varibles of interest whith each other
cor_func <- function(var_of_interest)
{ 
  df<-data.frame(NA)
  for (j in 5:12) {
    x <- var_of_interest[ ,j]
    
    for(i in 5:12) {
      y <- var_of_interest[ ,i]
      m<-cor(x, y, use = "pairwise.complete.obs")
      
      #creating data frame wich extend itself when extra correlations are calculated
      df<-cbind(df, m)
      #give a name to the extended column 
      names(df)[names(df) == 'm'] <- paste(colnames(var_of_interest[j]), colnames(var_of_interest[i]), sep = "&")
    }
  }
  return(df)
  }

#ddply formule
cor_var_of_interest<-ddply(var_of_interest, "filled_out_by_id", cor_func)


## create dataframe with the means of the correlations
#create empty dataframe
means_cor_var_of_interest <- data.frame (NA, NA)

#Calculating mean for each correlation combination using forloop, and add each of these means in dataframe.
for(i in 3:66) {
  
  
  n<-mean(cor_var_of_interest[[i]], na.rm = TRUE)
  
  means_cor_var_of_interest<-cbind(means_cor_var_of_interest, n)
}    

#insert correct column names
colnames(means_cor_var_of_interest) <- colnames(cor_var_of_interest)

write.csv2(means_cor_var_of_interest,'means_cor_var_of_interest.csv')


