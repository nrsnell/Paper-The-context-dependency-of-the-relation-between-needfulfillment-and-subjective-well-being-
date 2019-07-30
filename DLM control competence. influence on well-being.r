#########################
# Important information #
#########################

#data frame with only variables of interest
#A C Rfriends Rteachers school = v3,v4,v5,v6;
#A C Rfriends Rparents non school = v14,v15,v16,v17;
#well-being = v25

#######################################
# Setting up data frames and packages #
#######################################

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
diary_control_student_all<-read.csv2("responses_dagboek_studenten_controle_2019-01-16.csv", sep=";", na.strings = "NA")
diary_risk_student_all<-read.csv2("responses_dagboek_studenten_2019-01-16.csv", sep=";", na.strings = "NA")

#################################################
# exclude persons with <15 filled in datapoints #
#################################################

### Risk group ###
#Omit NA van completed-at - gebruiken geen rijen die toch niet zijn ingevuld-
diary_risk_student<-diary_risk_student_all[-which(is.na(diary_risk_student_all$completed_at)),]

#kijk hoeveel metingen per case minimaal 15
countsrisico  <- data.frame(table(diary_risk_student$filled_out_by_id))

colnames(countsrisico)[1]<-"filled_out_by_id"

#dataframe counts combineren met dataframe
diary_risk_student <- join(diary_risk_student, countsrisico, by=('filled_out_by_id'), type='inner')
diary_risk_student<- subset(diary_risk_student, Freq  > 14)

### Control group ###
#Omit NA van completed-at - gebruiken geen rijen die toch niet zijn ingevuld-
diary_control_student<-diary_control_student_all[-which(is.na(diary_control_student_all$completed_at)),]

#kijk hoeveel metingen per case minimaal 15
countsrisico  <- data.frame(table(diary_control_student$filled_out_by_id))

colnames(countsrisico)[1]<-"filled_out_by_id"

#dataframe counts combineren met dataframe
diary_control_student <- join(diary_control_student, countsrisico, by=('filled_out_by_id'), type='inner')
diary_control_student<- subset(diary_control_student, Freq  > 14)

###################################################

#change class of column to numeric
diary_risk_student$v3<-as.numeric(diary_risk_student$v3)
diary_risk_student$v4<-as.numeric(diary_risk_student$v4)
diary_risk_student$v5<-as.numeric(diary_risk_student$v5)
diary_risk_student$v6<-as.numeric(diary_risk_student$v6)
diary_risk_student$v14<-as.numeric(diary_risk_student$v14)
diary_risk_student$v15<-as.numeric(diary_risk_student$v15)
diary_risk_student$v16<-as.numeric(diary_risk_student$v16)
diary_risk_student$v17<-as.numeric(diary_risk_student$v17)
diary_risk_student$v25<-as.numeric(diary_risk_student$v25)

diary_control_student$v3<-as.numeric(diary_control_student$v3)
diary_control_student$v4<-as.numeric(diary_control_student$v4)
diary_control_student$v5<-as.numeric(diary_control_student$v5)
diary_control_student$v6<-as.numeric(diary_control_student$v6)
diary_control_student$v14<-as.numeric(diary_control_student$v14)
diary_control_student$v15<-as.numeric(diary_control_student$v15)
diary_control_student$v16<-as.numeric(diary_control_student$v16)
diary_control_student$v17<-as.numeric(diary_control_student$v17)
diary_control_student$v25<-as.numeric(diary_control_student$v25)

#creating dataframes
#add column to deviate the risk group from the control group
diary_risk_student$group <- "risk_student"
diary_control_student$group <-"control_student"

#bind risk and control student dataframe
diary_all<-rbind.fill(diary_risk_student, diary_control_student)

#delete NA's out of relevent columns
diary_risk_student<-diary_risk_student[complete.cases(diary_risk_student[c('v25','v3','v4','v5','v6','v14','v15','v16','v17')]),]
diary_control_student<-diary_control_student[complete.cases(diary_control_student[c('v25','v3','v4','v5','v6','v14','v15','v16','v17')]),]


#############################
# 1. DLM's controlstudent group#
#############################

##############
# Competence #
##############



dlmfunction <- function(diary_control_student)
{
  
  ###DATAPREP###
  
  
  #make matrix of X data (independent variables):
  onaf_var<-as.matrix(diary_control_student[,c("v4","v15")])
  ## v4 = Competence school
  ## v15 = Competence niet-school
  
  #make matrix of y data (dependent variables)
  afh_var<-as.matrix(diary_control_student[,c("v25")])
  ## v25 = welzijn
  
  
  #########
  # MODEL #
  #########
  
  
  dlmmodel<-dlm(
    
    #first we define the observed model y(t)=b1*x1(t)+b2*x2(t)+u(t)+v(t)
    
    #FF defines changes over time: do the elements change over time? 1=yes 0=no
    #Elements (?): b1X1, b2X2, u, v 
    #nr means amount of rows, but it divides the numbers over these rows column-wise (first filling the first column, then the second etc)
    FF = matrix(c(1,1,1,0), nr = 1),
    #this means that b1*x1, b2*x2 and u are 'on' in the model, but the observed error is not
    
    #JFF defines whether the corresponding part of FF is time-variant, with 0 = time invariant,
    #a number indicates that it is variant, and the columnnumber of X where the corresponding part of F can be found
    #elements (?): b1X1, b2X2, u, v  
    JFF = matrix(c(1,2,0,0), nr = 1),
    ##this means b1*x1 and b2*x2 are time variant, but u(t) and v(t) are not.
    
    #Variance of the observed model (the v(t) in y(t))
    V = 15,
    ## this numer is a guesstimate of the observed variance
    
    
    #second we define the latent/system model: u=u(t-1)+g(t-1)+w(t)
    #where g =trend/autocorrelation and w = variance
    
    #GG is definition of whether the component is time dependent, 1=yes 0=no
    #Elements are a matrix of bt (on y-axis) and bt-1 (on x-axis)
    GG = matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,1), nr = 4),
    ##meaning that the current value of each element depends on the previous value (t-1) of that same element,
    ##in addition, the current value of u depends on g-1
    
    #W is the variance of the latent/system model
    #Elements are latent model variance of: b1, b2, u, g 
    W = diag(c(0,0,15,0)),
    ## this number is a guesstimate of the variance of the latent model
    
    #Expected mean for each of the elements at time 0. 
    #Elements (?): b1, b2, u, g 
    m0 = c(0,0,50,0),
    ## this means we guesstimate u to start at 50 - the center of the 100-point scale, 
    ## and expect a starting value of 0 for the other elements, but is this justified? 
    
    #Expected variance of the observed model (?) for each element at time 0. Elements are: (which elements?). 
    #matrix of elements (?): X1, X2, u, V 
    C0 = diag(c(15,15,15,15)),
    ## this means we guestimate the variance of all the elements to start at 15
    
    
    X = onaf_var
  )
  
  #calculate model outcome
  outcome<-dlmFilter(y = afh_var, mod = dlmmodel)
  
  # calculate smoothed version
  S <- dlmSmooth(outcome)
  pred.smooth <- onaf_var[,1]*S$s[-1,1]
  +onaf_var[,2]*S$s[-1,2]
  + S$s[-1,3] + S$s[-1,4]
  
  
  ########################
  # results in dataframe #
  ########################
  
  data.frame(com_school = S$s[1,1],
             com_niet = S$s[1,2])
}


################################
# Apply DLM to each individual #
################################

regressionweights_competence <- ddply(diary_control_student, "filled_out_by_id", dlmfunction)
regressionweights_competence

############
#pirateplot#
############

#restructuring of regressionweight dataframe so pirateplot can read it:
betas_competence<-gather(regressionweights_competence, type, regrvalue, com_school:com_niet)

#execute pirateplot
pirateplot(formula = regrvalue ~ type,
           data = betas_competence,
           main = "Control group: relation between different sources of competence and well-being",
           ylab = "Regression weights",
           xlab = "sources of competence",
           inf.method = "ci",
           jitter.val = .08,
           sortx = "sequential",
           ylim = c(-5.5, 5.5),
           xaxt = "n",
           pal = "xmen",
           point.o = .2,
           avg.line.o = 1,
           bar.b.o = 0,
           inf.f.o = 0.3,
           bean.b.o = 0.5,
           point.cex = 1.5,
           point.pch = 18,
           bw = 0.2,
           yaxt= "n"
)

#custom axis
axis(1, at=1:2, labels=c("School", "Non school"))
axis(2, tick=c(-1, -0.75,-0.5,-0.25,0,0.25,0.5, 0.75, 1), las=1)

#add zero line
abline(h = 0, col = "gray20", lty = 3)

######################################################
#calculate % positive and negative regression weights#
######################################################

#School
sum(regressionweights_competence$com_school > 0)/length(regressionweights_competence$com_school)*100
(1-(sum(regressionweights_competence$com_school > 0)/length(regressionweights_competence$com_school)))*100

#Non school
sum(regressionweights_competence$com_niet > 0)/length(regressionweights_competence$com_niet)*100
(1-(sum(regressionweights_competence$com_niet > 0)/length(regressionweights_competence$com_niet)))*100

########
#school#
########

#give confidence intervals for mean
t.test(regressionweights_competence$com_school,conf.int=TRUE)

#descriptives 
mean(regressionweights_competence$com_school)
sd (regressionweights_competence$com_school)
min (regressionweights_competence$com_school)
max (regressionweights_competence$com_school)

############
#non-school#
############

#give confidence intervals for mean
t.test(regressionweights_competence$com_niet,conf.int=TRUE)

#descriptives 
mean(regressionweights_competence$com_niet)
sd (regressionweights_competence$com_niet)
min (regressionweights_competence$com_niet)
max (regressionweights_competence$com_niet)



