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
diary_risk_student<-read.csv2("responses_dagboek_studenten_controle_2019-01-16.csv", sep=";", na.strings = "NA")
diary_control_student<-read.csv2("responses_dagboek_studenten_2019-01-16.csv", sep=";", na.strings = "NA")
