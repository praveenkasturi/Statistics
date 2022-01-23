#############################################
###### IDS_570_Project_code ######          #
## FHCS_2006_dataset ###                    #
#############################################

# setting seed and Loading Libraries ##

set.seed(123)
library(psych)
library(dplyr)
library(car)
library(corrplot)


# Function to replace 'X' bt 'NA' , later we will be deleting rows with 'NA' ##

replacing_X <- function(x) {
  gsub("X",NA,x)
}

# function to check transformation graphs at the later part ##

transforms <- function(x)
{ 
  par(mfrow=c(2,2))
  plot(density(x))
  plot(density(log(x)))
  plot(density(sqrt(x)))
}


#Dataset loading and cleaning ##

base_dataset <- read.csv(file.choose(), header=T, na.strings = "") 
base_dataset <- data.frame(apply(base_dataset,2,replacing_X))
survey_complete <- na.omit(base_dataset)

survey_clean <- survey_complete[apply(survey_complete[c(2:74)],1,function(z) !any(z==0)),]
#View(survey_clean)

##Number of employees in each agency
by_dept <- aggregate(as.numeric(survey_clean$Respond),list(survey_clean$Agency),sum)
#View(by_dept)

##Subsetting for four departments with maximum employees
survey_required <- subset(survey_clean,survey_clean$Agency=="HE"|survey_clean$Agency=="IN"|
                            survey_clean$Agency=="AG"|survey_clean$Agency=="AR")



survey_required[, c(2:74)] <- apply(survey_required[, c(2:74)],2, as.numeric)



final_dataset <- survey_clean
top_agency <- survey_required


#Distribution of Agencies vs No. of employees

barplot(table(final_dataset$Agency) , col=c("orange","blue"), main="Distribution of Agencies vs No. of employees", xlab="Agency", ylab="No. of Employees")
box()

#In Each Dept
IN <- final_dataset[final_dataset$Agency=="IN", ]
HE <- final_dataset[final_dataset$Agency=="HE", ]
AG <- final_dataset[final_dataset$Agency=="AG", ]
AR <- final_dataset[final_dataset$Agency=="AR", ]

nrow(IN[IN$DSEX == "A",])
nrow(IN[IN$DSEX == "B",])
nrow(HE[HE$DSEX == "A",])
nrow(HE[HE$DSEX == "B",])
nrow(AG[AG$DSEX == "A",])
nrow(AG[AG$DSEX == "B",])
nrow(AR[AR$DSEX == "A",])
nrow(AR[AR$DSEX == "B",])

#Males and Females in Each Dept
dev.off()
par(mfrow=c(2,2))

barplot(table(IN$DSEX), col=c("maroon","pink"), main="No. of Male and Females in the department IN", ylim=c(0,3000), xlab="Gender", ylab="No. of Employees")
legend("topright", legend = c("Males", "Females"), fill = c("maroon", "pink"),pt.cex=1, cex=0.4)
box()


barplot(table(HE$DSEX), col=c("darkgreen","lightgreen"), main="No. of Male and Females in the department HE", ylim=c(0,3000),xlab="Gender", ylab="No. of Employees")
legend("topleft", legend = c("Males", "Females"), fill = c("darkgreen", "lightgreen"),pt.cex=1, cex=0.4)
box()


barplot(table(AG$DSEX), col=c("sienna1","sienna4"), main="No. of Male and Females in the department AG", ylim=c(0,3000),xlab="Gender", ylab="No. of Employees")
legend("topright", legend = c("Males", "Females"), fill = c("sienna1","sienna4"),pt.cex=1, cex=0.4)
box()

barplot(table(AR$DSEX), col=c("turquoise4","turquoise2"), main="No. of Male and Females in the department AR", ylim=c(0,3000),xlab="Gender", ylab="No. of Employees")
legend("topright", legend = c("Males", "Females"), fill = c("turquoise4","turquoise2"), pt.cex = 1, cex=0.4)
box()




#Distribution of employees across age groups


nrow(IN[IN$DAGEGRP == "B",])  
nrow(IN[IN$DAGEGRP == "C",])
nrow(IN[IN$DAGEGRP == "D",])
nrow(IN[IN$DAGEGRP == "E",])
nrow(IN[IN$DAGEGRP == "F",])

nrow(HE[HE$DAGEGRP == "B",])
nrow(HE[HE$DAGEGRP == "C",])
nrow(HE[HE$DAGEGRP == "D",])
nrow(HE[HE$DAGEGRP == "E",])
nrow(HE[HE$DAGEGRP == "F",])

nrow(AG[AG$DAGEGRP == "B",])
nrow(AG[AG$DAGEGRP == "C",])
nrow(AG[AG$DAGEGRP == "D",])
nrow(AG[AG$DAGEGRP == "E",])
nrow(AG[AG$DAGEGRP == "F",])

nrow(AR[AR$DAGEGRP == "B",])
nrow(AR[AR$DAGEGRP == "C",])
nrow(AR[AR$DAGEGRP == "D",])
nrow(AR[AR$DAGEGRP == "E",])
nrow(AR[AR$DAGEGRP == "F",])

dev.off()
par(mfrow=c(2,2))

barplot(table(IN$DAGEGRP), col=c("pink","violetred4","salmon","orangered4","tan1"), main="Distribution of Employees in IN across Age groups", ylim=c(0,4000), xlab="Age Groups", ylab="No. of Employees")
legend("topleft", legend = c("29 and Under", "30-39", "40-49", "50-59" , "60 or older"), fill = c("pink","violetred4","salmon","orangered4","tan1"), ncol=2, pt.cex=1, cex=0.3)
box()

barplot(table(HE$DAGEGRP), col=c("orange","springgreen","forestgreen","darkolivegreen1","orange4"), main="Distribution of Employees in HE across Age groups", ylim=c(0,4000),xlab="Age Groups", ylab="No. of Employees")
legend("topleft", legend = c("29 and Under", "30-39", "40-49", "50-59" , "60 or older"), fill = c("orange","springgreen","forestgreen","darkolivegreen1","orange4"), cex=0.3)
box()

barplot(table(AG$DAGEGRP), col=c("darkorange","yellow","lightsalmon","firebrick","red"), main="Distribution of Employees in AG across Age groups", ylim=c(0,4000),xlab="Age Groups", ylab="No. of Employees")
legend("topleft", legend = c("29 and Under", "30-39", "40-49", "50-59" , "60 or older"), fill = c("darkorange","yellow","lightsalmon","firebrick","red"), cex=0.3)
box()


barplot(table(AR$DAGEGRP), col=c("cyan","cyan4","navy","purple","darkmagenta"), main="Distribution of Employees in AR across Age groups", ylim=c(0,4000),xlab="Age Groups", ylab="No. of Employees")
legend("topleft", legend = c("29 and Under", "30-39", "40-49", "50-59" , "60 or older"), fill = c("cyan","cyan4","navy","purple","darkmagenta"), cex=0.3)
box()


dev.off()
par(mfrow=c(1,2))
#employees in headquarters
HQ<-top_agency[top_agency$DLOC=="A",]

nrow(HQ[HQ$DSEX == "A",])
nrow(HQ[HQ$DSEX == "B",])

barplot(table(HQ$DSEX), col=c("blue", "red"),main="No. of males and females in Head Quarter", ylab="No. of employees", xlab="Gender" ,ylim=c(0,5000)) 
legend("topleft",legend = c("Male", "Female"), fill = c("blue", "red"), pt.cex=1,cex=0.7)
box()


#Employees in Field
Field<-top_agency[top_agency$DLOC=="B",]

nrow(Field[Field$DSEX == "A",])
nrow(Field[Field$DSEX == "B",])

barplot(table(Field$DSEX), col=c("green", "yellow"),main="No. of males and females in Field", ylim =c(0,5000), xlab = "Gender", ylab = "No. of Employees")
legend("topright",legend = c("Male", "Female"), fill = c("green", "yellow"), pt.cex=1, cex=0.7)
box()


dev.off()
#Employees Tenure in Agency

barplot(table(top_agency$DAGYTEN), col=c("coral", "blue","yellow", "green", "Orange", "red"),main="Distribution wrt to experience", ylab = "No. of employees", xlab = "Tenure in Agency", ylim = c(0,10000))
legend("topleft",legend = c("Less than 1 year", "1 to 3 years", "4 to 5 years","6 to 10 years","11 to 20 years","more than 20 years"), fill = c("coral", "blue","yellow", "green", "Orange", "red"), pt.cex = 1, cex = 0.7)
box()


#Employees Leaving the Department

top_agency$LEAVING[top_agency$DLEAVING=="A"] <- "NO"
top_agency$LEAVING[top_agency$DLEAVING!="A"] <- "YES"



barplot(table(top_agency$LEAVING), col=c("coral", "blue"),main="Distribution wrt to leaving the department next year ", ylab = "No. of employees", xlab = "Employees decisions", ylim = c(0,10000))
box()


dev.off()
par(mfrow=c(1,2))
#Distribution of Job Satisfaction wrt to Location
tab <- table(top_agency$DLOC,top_agency$Q60)
barplot(tab, main="Distribution of Job Satifaction wrt Location", xlab="Job Satisfaction", ylab = "No. Of Employees", ylim=c(0,10000), col=c("red4","red"))
legend("topleft",legend = c("Headquarters","Field"), fill = c("red4","red"), pt.cex = 1, cex = 0.7)
box()

#Distribution of Job Satisfaction wrt to Organization
tab1 <- table(top_agency$DLOC,top_agency$Q62)

barplot(tab1, main="Distribution of Job Satifaction wrt Organization", xlab="Job Satisfaction", ylab = "No. Of Employees", ylim=c(0,10000), col=c("sienna4","sienna1"))
legend("topleft",legend = c("Headquarters","Field"), fill = c("sienna4","sienna1"), pt.cex = 1, cex = 0.7)
box()


dev.off()
#Distribution of age groups wrt Job satisfaction
tabAge <- table(top_agency$DAGEGRP, top_agency$Q60)
barplot(tabAge, main="Distribution of Job Satifaction wrt Age Groups",beside=TRUE, xlab="Job Satisfaction", ylab = "No. Of Employees", ylim=c(0,5000), col=c("red","yellow","orange","tan4","tomato"))
legend("topleft",legend = c("29 and Under", "30-39", "40-49", "50-59" , "60 or older"), fill = c("red","yellow","orange","tan4","tomato"), pt.cex = 1, cex = 0.7)
box()

dev.off()
#Distribution of Tenure wrt Job Satisfaction
tabTenure <- table(top_agency$DAGYTEN , top_agency$Q60)

barplot(tabTenure, main="Distribution of Job Satifaction wrt Tenure",beside=TRUE, xlab="Job Satisfaction", ylab = "No. Of Employees", ylim=c(0,4000), col=c("red","yellow","orange","tan4","peachpuff","tomato"))
legend("topleft",legend = c("Less than 1 year", "1 to 3 years", "4 to 5 years","6 to 10 years","11 to 20 years","more than 20 years"), fill = c("red","yellow","orange","tan4","peachpuff","tomato"), pt.cex = 1, cex = 0.7)
box()


dev.off()

tabLeaving <- table(top_agency$LEAVING, top_agency$Q60)
barplot(tabLeaving, main="Distribution of Job Satifaction wrt Leaving",beside=TRUE, xlab="Job Satisfaction", ylab = "No. Of Employees", ylim=c(0,6000), col=c("red","yellow"))
legend("topleft",legend = c("NO","YES"), fill = c("red","yellow"), pt.cex = 1, cex = 0.7)
box()


#Creating dependent variable Overall_Job_Satisfaction

cormat_Overall_Job_Satisfaction <- cor(survey_required[,c(57,58,59,61,62,63)])
round(cormat_Overall_Job_Satisfaction, 2)
corrplot(cormat_Overall_Job_Satisfaction, method="circle", addCoef.col="green") 
dev.off()

# Eliminating Q61 as it low corelation value.

cormat_Overall_Job_Satisfaction <- cor(survey_required[,c(57,58,59,61,63)])
round(cormat_Overall_Job_Satisfaction, 2)
corrplot(cormat_Overall_Job_Satisfaction, method="circle", addCoef.col="green") 
dev.off()


survey_required$Overall_Job_Satisfaction <- (survey_required$Q56 + survey_required$Q57 +
                                               survey_required$Q58 +survey_required$Q60 + survey_required$Q62)

hist(survey_required$Overall_Job_Satisfaction, prob=TRUE, main="Distribution of dependent variable")
curve(dnorm(x, mean(survey_required$Overall_Job_Satisfaction), sd(survey_required$Overall_Job_Satisfaction)), add=TRUE, col="red", lwd=2)
plot(density(survey_required$Overall_Job_Satisfaction))
transforms(survey_required$Overall_Job_Satisfaction) #default is betetr than the transformed ones
dev.off()

#Creating Independent Variables
#Relationship with Supervisor/Leadership

cormat_leadership <- cor(survey_required[,c(8,37,38,39,40,41,48,56,58)])
round(cormat_leadership, 2) # Rounded to 2 decimals
corrplot(cormat_leadership, method="circle", addCoef.col="green") 
dev.off()

#Removed Q7 and Q47 as it shows lower correlation against many questions

cormat_leadership <- cor(survey_required[,c(37,38,39,40,41,56,58)])
round(cormat_leadership, 2) # Rounded to 2 decimals
corrplot(cormat_leadership, method="circle", addCoef.col="green") 
dev.off()
survey_required$Leadership <- (survey_required$Q36+survey_required$Q37+survey_required$Q38+survey_required$Q39+survey_required$Q40+survey_required$Q55+ survey_required$Q57)


#Organization Fairness
cormat_orgfairness <- cor(survey_required[,c(44,45,46,47)])
round(cormat_orgfairness, 2) # Rounded to 2 decimals
corrplot(cormat_orgfairness, method="circle", addCoef.col="black")   
dev.off()
survey_required$OrgFairness <-(survey_required$Q43 + survey_required$Q44 + survey_required$Q45 + survey_required$Q46)


#Training and Personal Growth
cormat_training <- cor(survey_required[,c(3,6,14,16,19,21,25,49,51,53,59,60)])
round(cormat_training, 2) # Rounded to 2 decimals 
corrplot(cormat_training, method="circle", addCoef.col="black")                                           
dev.off()

#Removing columns Q15, Q20,and Q52, which are not much correlated to the other variables 
#since they might not be relevant

cormat_training <- cor(survey_required[,c(3,6,14,19,25,49,51,59,60)])
round(cormat_training, 2) # Rounded to 2 decimals 
corrplot(cormat_training, method="circle", addCoef.col="black")                                           
dev.off()

survey_required$Training_PGrowth <- (survey_required$Q2+survey_required$Q5+survey_required$Q13+
                                     survey_required$Q18+survey_required$Q24+survey_required$Q48+
                                     survey_required$Q50+survey_required$Q58+survey_required$Q59)
 

#Performance Evaluation
cormat_PEval <- cor(survey_required[,c(23,27,28,29,30,31,56)])
round(cormat_PEval, 2) # Rounded to 2 decimals 
corrplot(cormat_PEval, method="circle", addCoef.col="black")  
dev.off()
survey_required$PEval <- (survey_required$Q22+survey_required$Q26+survey_required$Q27+
                          survey_required$Q28+survey_required$Q29+survey_required$Q30+
                          survey_required$Q56)


#Diversity Management
cormat_Diversity <- cor(survey_required[,c(34,35,36)])
round(cormat_Diversity, 2) # Rounded to 2 decimals 
corrplot(cormat_Diversity, method="circle", addCoef.col="black") 
dev.off()
survey_required$Diversity <- (survey_required$Q33+survey_required$Q34+survey_required$Q35)


#Compensation
cormat_Compensation <- cor(survey_required[,c(62,64,65,66,67,69,70)])
round(cormat_Compensation, 2) # Rounded to 2 decimals 
corrplot(cormat_Compensation, method="circle", addCoef.col="black") 
dev.off()

# Removing Q61, Q64

cormat_Compensation <- cor(survey_required[,c(64,65,66,67,69,70)])
round(cormat_Compensation, 2) # Rounded to 2 decimals 
corrplot(cormat_Compensation, method="circle", addCoef.col="black") 
dev.off()

survey_required$Compensation <- (survey_required$Q63+survey_required$Q64+
                                 survey_required$Q65+survey_required$Q66+survey_required$Q68
                                 +survey_required$Q69)

final_data <- survey_required

##==================Hypothesis Testing using Correlation======================##

#Relationship with Leadership vs Job Satisfaction
jsat_leadership <- cor.test(survey_required$Overall_Job_Satisfaction,survey_required$Leadership) 
jsat_leadership



#Performance Evaluation vs Job Satisfaction
jsat_peval <- cor.test(survey_required$Overall_Job_Satisfaction,survey_required$PEval)
jsat_peval

#Training and Personal Growth vs Job Satisfaction
jsat_training <- cor.test(survey_required$Overall_Job_Satisfaction,survey_required$Training_PGrowth)
jsat_training


#Organization Fairness vs Job Satisfaction

jsat_orgfair <- cor.test(survey_required$Overall_Job_Satisfaction,survey_required$OrgFairness)
jsat_orgfair


#Diversity Management vs Job Satisfaction
jsat_diversity<- cor.test(survey_required$Overall_Job_Satisfaction,survey_required$Diversity)
jsat_diversity

#Compensation and Benefits vs Job Satisfaction
jsat_compensation <- cor.test(survey_required$Overall_Job_Satisfaction,survey_required$Compensation)
jsat_compensation

##Additional test conducted
tab<- table(survey_required$DSEX, survey_required$DLOC)
addmargins(tab,c(1,2))
ptab<-prop.table(tab)
round(ptab,2)
addmargins(round(ptab,2),c(1,2))
mosaicplot(ptab)
#chi-square test 
chisq.test(tab)

##Analysis: We reject the Null Hypothesis since the p-value is low. This implies that 
##there is an association between Gender and Work Location


##===================Distributions====================##

transforms(survey_required$Overall_Job_Satisfaction)
dev.off()
transforms(survey_required$Leadership)  
dev.off()
transforms(survey_required$PEval) 
dev.off()
transforms(survey_required$Training_PGrowth) 
dev.off()
transforms(survey_required$Diversity)
dev.off()
transforms(survey_required$OrgFairness)
dev.off()
transforms(survey_required$Compensation)
dev.off()

##===================Initial Linear Regression==================##


##INITIAL MODEL##
model1 <- lm(Overall_Job_Satisfaction ~ Leadership+PEval+Training_PGrowth+Diversity+OrgFairness
             +Compensation+DLOC+DSEX, data=final_data)
summary(model1)


##===================Second Linear Regression==================##


##SECOND MODEL##

model2 <- lm(Overall_Job_Satisfaction ~ Leadership+PEval+Training_PGrowth+Diversity+Compensation+DLOC, data=final_data)
summary(model2)


##==================Outlier Treatment=======================##

b1 <- boxplot(survey_required$Overall_Job_Satisfaction)
outliers <- b1$out
outliersdf <- survey_required[survey_required$Overall_Job_Satisfaction %in% outliers,] 
# Use the %in% function to match each row to these values 
# In the above, you've got an entire data frame. If you only need row names, use this function 
final_no_outliers_overall <- subset(survey_required, !(survey_required$Overall_Job_Satisfaction %in% outliers))

final_data <- final_no_outliers_overall

##============Distribution after outlier treatment===============##

transforms(final_data$Overall_Job_Satisfaction)
dev.off()
transforms(final_data$Leadership)  
dev.off()
transforms(final_data$PEval) 
dev.off()
transforms(final_data$Training_PGrowth) 
dev.off()
transforms(final_data$Diversity)
dev.off()
transforms(final_no_outliers_overall$OrgFairness)
dev.off()
transforms(final_no_outliers_overall$Compensation)
dev.off()








transforms(final_no_outliers_overall$Overall_Job_Satisfaction)
dev.off()
transforms(final_no_outliers_overall$Leadership)  
dev.off()
transforms(final_no_outliers_overall$PEval) 
dev.off()
transforms(final_no_outliers_overall$Training_PGrowth) 
dev.off()
transforms(final_no_outliers_overall$Diversity)
dev.off()
transforms(final_no_outliers_overall$OrgFairness)
dev.off()
transforms(final_no_outliers_overall$Compensation)
dev.off()

final_data <- final_no_outliers_overall

##=======================Transformations========================##

##????##There seems to be no need for Transformations

#final_data$Leadership <- log(final_data_1$Leadership)
#final_data$OrgFairness <- log(final_data_1$OrgFairness)
#final_data$PEval <- log(final_data_1$PEval)
#final_data$Training_PGrowth <- log(final_data_1$Training_PGrowth)
#final_data$Diversity <- log(final_data_1$Diversity)
#final_data$Compensation <- log(final_data_1$Compensation)




##==============================VIF=============================##

#Checking for Multicollinearity

cormat<- cor(final_data[,c(90:95)]) 
cormat # highly correlated
corrplot(cormat, method="circle")
corrplot(cormat, method="circle", addCoef.col="black") 
dev.off()
# A more precise analysis is using VIF (Variance Inflation Factor)
# It looks for multicollinearity in a model as a whole 
vif(model1) #Anything less than 4 is okay!
sqrt(vif(model1)) > 2 # if any variable is true, we would need to drop it 
#None of the variables need to be dropped based on VIF

##=====================Linear Regression========================##

model3 <- lm(Overall_Job_Satisfaction ~ Leadership+PEval+Training_PGrowth+Diversity+
          +Compensation+DLOC, data=final_data)
summary(model3)

# This is the Best model we have got among all ##

##=================-THANK-YOU-===================================##
