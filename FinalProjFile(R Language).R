#Including the libraries
library(dplyr)
library(plyr)
library(reval)
library(lattice)

#Importing the dataset

ins<-read.csv(file.choose(),header=T, na.string=c("","NA"))
View(ins)
str(ins)
summary(ins)
names(ins)

#*******************************************************************************

#Checking for the redundant data(duplicate rows)
dim(ins)
unique(ins)
dim(unique(ins))
dim(ins[!duplicated(ins),])
dim(ins[duplicated(ins),])
nrow(ins)  
ncol(ins)

#Though the data is duplicated , it can't be called redundant in this case
#*******************************************************************************


#Treating the NA values for each column
names(ins)
is.na(ins$Area_Service)
a1<-sum(is.na(ins$Area_Service))
a2<-sum(is.na(ins$Hospital.County))
a3<-sum(is.na(ins$Certificate_num))
a4<-sum(is.na(ins$Hospital.Id))
a5<-sum(is.na(ins$Hospital.Name))
a6<-sum(is.na(ins$Age))
a7<-sum(is.na(ins$zip_code_3_digits))
a8<-sum(is.na(ins$Gender))
a9<-sum(is.na(ins$Cultural_group))
a10<-sum(is.na(ins$ethnicity))
a11<-sum(is.na(ins$Days_spend_hsptl))
a12<-sum(is.na(ins$Admission_type))
a13<-sum(is.na(ins$Home.or.self.care.))
a14<-sum(is.na(ins$year_discharge))
a15<-sum(is.na(ins$ccs_diagnosis_code))
a16<-sum(is.na(ins$ccs_diagnosis_description))
a17<-sum(is.na(ins$ccs_procedure_code))
a18<-sum(is.na(ins$ccs_procedure_description))
a19<-sum(is.na(ins$apr_drg_description))
a20<-sum(is.na(ins$apr_mdc_description))
a21<-sum(is.na(ins$Code_illness))
a22<-sum(is.na(ins$Description_illness))
a23<-sum(is.na(ins$Mortality.risk))
a24<-sum(is.na(ins$Surg_Description))
a25<-sum(is.na(ins$Payment_typology_1))
a26<-sum(is.na(ins$payment_typology_2))
a27<-sum(is.na(ins$payment_typology_3))
a28<-sum(is.na(ins$Weight_baby))
a29<-sum(is.na(ins$Abortion))
a30<-sum(is.na(ins$Emergency.dept_yes.No))
a31<-sum(is.na(ins$Tot_charg))
a32<-sum(is.na(ins$Tot_cost))
a33<-sum(is.na(ins$ratio_of_total_costs_to_total_charges))
a34<-sum(is.na(ins$Result))



#Creating a dataframe for the NA values in each column
Features<-c("Area_Service","Hospital.County", "Certificate_num", "Hospital.Id" , "Hospital.Name","Age","zip_code_3_digits" , "Gender" ,"Cultural_group","ethnicity","Days_spend_hsptl", "Admission_type","Home.or.self.care." , "year_discharge" ,"ccs_diagnosis_code","ccs_diagnosis_description" ,"ccs_procedure_code", "ccs_procedure_description","apr_drg_description","apr_mdc_description","Code_illness","Description_illness","Mortality.risk","Surg_Description","Payment_typology_1","payment_typology_2","payment_typology_3"  ,"Weight_baby","Abortion"  , "Emergency.dept_yes.No","Tot_charg" , "Tot_cost","ratio_of_total_costs_to_total_charges","Result"      )
na<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34)
df<-data.frame(Features,na)
View(df)
View(ins)

#Exporting the table
install.packages("xlsx")
library(xlsx)
write.table(df,file = "df.csv")
write.csv(df, file = "df.csv")
sum(is.na(ins))
        
#*************************************************
#Converting the categorial columns to integers
ins$Area_Service<-as.integer(ins$Area_Service)
ins$Hospital.County<-as.integer(ins$Hospital.County)
#****************************************************************************
#Visualization of features
table(ins$Area_Service)
barplot(table(ins$Area_Service),
        main="Area Service",
        xlab="Count",
        ylab="Area Service",
        border="red",
        col="Green",
        density=5
)
#*******************************************************************************
#Finding the missing values for catergorial data
install.packages("mice")
install.packages("VIM")
library(mice)
library(VIM)
z<- function(x) {sum(is.na(x))/length(x)*100}
apply(ins, 2, z) 
apply(ins, 1, z)
#The dataset consists of MCAR(Missing completely at Random) NA values,
#The safe threshold for MCAR is 5%
#Considering the above function, 
# Area_Service                       Hospital.County 
#0.234222635                           0.234222635 
#Certificate_num                           Hospital.Id 
#0.234222635                           0.234222635 
#zip_code_3_digits                           
#1.405145078 
#payment_typology_2 
#33.890947238 
#payment_typology_3                           
#74.890017405

#Description_illness 
#0.004005436 
#Mortality.risk 
#0.004005436 


#The last 2 payment_typo columns have NA values above threshold level,
#Thus , those columns could be dropped.
#Dropping the required columns
#Using the subset function for dropping the columns
data<-ins
data = subset(data, select = -c(payment_typology_2,payment_typology_3) )
View(data)

ins<-data
View(ins)
install.packages("naniar")
library(naniar)
#*****************************************************
#Eradicating the redundant variable in the following column
ins$zip_code_3_digits <- replace(ins$zip_code_3_digits, ins$zip_code_3_digits =="OOS", 0)
View(ins$zip_code_3_digits)
View(insdf)

#Treating the remaining columns
nafor_ins<-ins[,1:4]
zip_code<-ins[,7]
Description_illness<-ins[,22]
View(add1)                                    #,Payment_typo2,Payment_typo3
Mortality.Risk<-ins[,23]
Payment_typo2<-ins[,26]
Payment_typo3<-ins[,27]
View(nafor_ins)
na<-cbind(nafor_ins,zip_code)
View(na)
names(na)


#Visualization using VIM package
md.pattern(na)
aggr_plot <- aggr(na, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(na), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#Boxplot visualization
marginplot(na[c(1,5)])

#Imputing the missing data
methods(mice)

na<-na[1:100000,]
View(na)
#Here,we are going to use Predictive Mean Mining(pmm) 
Temp<- mice(na,m=2,maxit=20,meth='pmm',seed=500)

summary(Temp)

ins$Mortality.risk<-as.numeric(ins$Mortality.risk)
hist(ins$Mortality.risk)
View(ins)
Temp$imp$Hospital.County
Temp$imp$Certificate_num
Temp$imp$Hospital.Id
Temp$imp$zip_code

ImputedData<-complete(Temp,2)
View(ImputedData)


#Distribution of original and imputed data
#Using scatter/xy plot
xyplot(Temp,Area_Service ~ Hospital.County+Certificate_num+Hospital.Id+zip_code_3_digits,pch=18,cex=1)


#Imputing the missing values for Area Service
install.packages("Hmisc")
library(Hmisc)
insdf$Area_Service<-ins$Area_Service
ImputedData$Area_Service[is.na(ImputedData$Area_Service)] <- mean(ImputedData$Area_Service, na.rm = T)
View(ImputedData)
sum(is.na(ImputedData))
#Cross-checking the NA vlaues in Imputed Data
sum(is.na(ImputedData$Area_Service))
sum(is.na(ImputedData$Hospital.County))
sum(is.na(ImputedData$Certificate_num))
sum(is.na(ImputedData$Hospital.Id))
sum(is.na(ImputedData$zip_code))

#Replacing the columns in actual data with the imputed columns
#Firstly dropping the required columns
#Using the subset function for dropping the columns
(insdf)
data1<-insdf
data = subset(data1, select = -c(Area_Service,Hospital.County,Certificate_num,Hospital.Id,zip_code_3_digits))
View(data)
sum(is.na(data))

insdf<-data

View(insdf)
View(ImputedData)
sum(is.na(ImputedData))
sum(is.na(insdf))


#Combining the columns
insdf<-cbind(ImputedData,insdf)
View(insdf)
sum(is.na(insdf))
#***************************************************************

#Converting the remaining categorial columns to numeric values
insdf$Hospital.Name<-as.numeric(insdf$Hospital.Name)
insdf$Gender<-as.numeric(insdf$Gender)
insdf$Cultural_group<-as.numeric(insdf$Cultural_group)
insdf$Age<-as.numeric(insdf$Age)
insdf$ethnicity<-as.numeric(insdf$ethnicity)
insdf$Admission_type<-as.numeric(insdf$Admission_type)
insdf$Home.or.self.care.<-as.numeric(insdf$Home.or.self.care.)
insdf$ccs_diagnosis_description<-as.numeric(insdf$ccs_diagnosis_description)
insdf$ccs_procedure_description<-as.numeric(insdf$ccs_procedure_description)
insdf$apr_drg_description<-as.numeric(insdf$apr_drg_description)
insdf$apr_mdc_description<-as.numeric(insdf$apr_mdc_description)
insdf$Description_illness<-as.numeric(insdf$Description_illness)
insdf$Mortality.risk<-as.numeric(insdf$Mortality.risk)
insdf$Surg_Description<-as.numeric(insdf$Surg_Description)
insdf$Payment_typology_1<-as.numeric(insdf$Payment_typology_1)
insdf$Abortion<-as.numeric(insdf$Abortion)
insdf$Emergency.dept_yes.No<-as.numeric(insdf$Emergency.dept_yes.No)

hist()
#Still there are  NA values introduced in 2 columns 

sum(is.na(insdf$Description_illness))
sum(is.na(insdf$Mortality.risk))

#To find which are those NA values
which(is.na(insdf$Description_illness))
which(is.na(insdf$Mortality.risk))

insdf$Description_illness[which(is.na(insdf$Description_illness))]<-mean(insdf$Description_illness,na.rm=TRUE)
insdf$Mortality.risk[which(is.na(insdf$Mortality.risk))]<-mean(insdf$Mortality.risk,na.rm=TRUE)
is.na(insdf)
sum(is.na(insdf))
View(insdf)


#The dataset consists of no NA values

#***************************************************************
#The zip_code column does not consist of meaningful factors
insdf$zip_code<-as.factor(insdf$zip_code)
View(insdf)

#****************************************************************************

#Univariate Analysis

#The next step is to treat the outliers




#Checking for the other columns
View(insdf)
names(insdf)

outlier_values1 <- boxplot.stats(insdf$Certificate_num)$out 
outlier_values2 <- boxplot.stats(insdf$Hospital.Id)$out 
outlier_values3 <- boxplot.stats(insdf$zip_code)$out 
outlier_values4 <- boxplot.stats(insdf$Hospital.Name)$out 
outlier_values5 <- boxplot.stats(insdf$Age)$out 
outlier_values6 <- boxplot.stats(insdf$Gender)$out 
outlier_values7 <- boxplot.stats(insdf$Cultural_group)$out 
outlier_values8 <- boxplot.stats(insdf$ethnicity)$out 
outlier_values9 <- boxplot.stats(insdf$Days_spend_hsptl)$out 
outlier_values10 <- boxplot.stats(insdf$Admission_type)$out 
outlier_values11 <- boxplot.stats(insdf$Home.or.self.care.)$out 
outlier_values12 <- boxplot.stats(insdf$year_discharge)$out 
outlier_values13 <- boxplot.stats(insdf$ccs_diagnosis_code)$out 
outlier_values14 <- boxplot.stats(insdf$ccs_diagnosis_description)$out 
outlier_values15 <- boxplot.stats(insdf$ccs_procedure_code)$out 
outlier_values16 <- boxplot.stats(insdf$ccs_procedure_description)$out 
outlier_values17 <- boxplot.stats(insdf$apr_drg_description)$out 
outlier_values18 <- boxplot.stats(insdf$apr_mdc_description)$out 
outlier_values19 <- boxplot.stats(insdf$Code_illness)$out 
outlier_values20 <- boxplot.stats(insdf$Description_illness)$out 
outlier_values21 <- boxplot.stats(insdf$Mortality.risk)$out 
outlier_values22 <- boxplot.stats(insdf$Surg_Description)$out 
outlier_values23 <- boxplot.stats(insdf$Payment_typology_1)$out 
outlier_values24 <- boxplot.stats(insdf$Weight_baby)$out 
outlier_values25 <- boxplot.stats(insdf$Abortion)$out 
outlier_values26 <- boxplot.stats(insdf$Emergency.dept_yes.No)$out 
outlier_values27 <- boxplot.stats(insdf$Tot_charg)$out 
outlier_values28 <- boxplot.stats(insdf$Tot_cost)$out 
outlier_values29 <- boxplot.stats(insdf$ratio_of_total_costs_to_total_charges)$out 
outlier_values30 <- boxplot.stats(insdf$Result)$out
outlier_values31 <- boxplot.stats(insdf$Area_Service)$out
outlier_values32 <- boxplot.stats(insdf$Hospital.County)$out
View(outlier_values25)
#************************************************************
#Creating a different variable for outliers
insout<-insdf
View(insout)
sum(is.na(insout))
require("datasets")
boxplot(insout$Hospital.County, main="Hospital_County", boxwex=0.1,horizontal = TRUE)#
hist(insdf$Hospital.County)
outliers1<-boxplot(insout$Hospital.County)$out
sum(outliers1)
insout[insout$Hospital.County %in% outliers, "Hospital.County"] = NA
sum(is.na(insout$Hospital.County))

#******************
boxplot(insout$Certificate_num, main="Certificate_num", boxwex=0.1,horizontal = TRUE)#
hist(insout$Certificate_num)
outliers2<-boxplot(insout$Certificate_num)$out
insout[insout$Certificate_num %in% outliers2, "Certificate_num"] = NA
sum(is.na(insout$Certificate_num))

#************************
boxplot(insdf$Hospital.Id, main="Hospital.Id", boxwex=0.1,horizontal = TRUE) 
insout$Hospital.Id<-insdf$Hospital.Id
outliers3<-boxplot(insout$Hospital.Id)$out
insout[insout$Hospital.Id %in% outliers3, "Hospital.Id"] = NA
sum(is.na(insout$Hospital.Id))

#************************
boxplot(insdf$zip_code, main="zip_code", boxwex=0.1,horizontal = TRUE)
outliers4<-boxplot(insout$zip_code)$out
insout[insout$zip_code %in% outliers4, "zip_code"] = NA
sum(is.na(insout$zip_code))
#****************************
boxplot(insdf$Hospital.Name, main="Hospital.Name", boxwex=0.1,horizontal = TRUE) 
outliers5<-boxplot(insout$Certificate_num)$out
#It does not consist of any outliers

#*****************************
boxplot(insdf$Age, main="Age", boxwex=0.1,horizontal=TRUE)
outliers6<-boxplot(insout$Age)$out
#It does not consist of any outliers

#*****************************
boxplot(insdf$Gender, main="Gender", boxwex=0.1,horizontal = TRUE)
hist(insout$Gender)
outliers7<-boxplot(insout$Gender)$out

#**********************************
boxplot(insdf$Cultural_group, main="Cultural_group", boxwex=0.1,horizontal = TRUE) #
hist(insout$Cultural_group)
outliers8<-boxplot(insout$Cultural_group)$out
insout[insout$Cultural_group %in% outliers8, "Cultural_group"] = NA
sum(is.na(insout$Cultural_group))

#***********************************
boxplot(insdf$ethnicity, main="ethnicity", boxwex=0.1,horizontal = TRUE) 
hist(insout$ethnicity)
outliers9<-boxplot(insout$ethnicity)$out
insout[insout$ethnicity%in% outliers9, "ethnicity"] = NA
sum(is.na(insout$ethnicity))


#**************************************************************
#The data type needs to be integer
insout$Days_spend_hsptl<-as.numeric(insout$Days_spend_hsptl)
boxplot(insdf$Days_spend_hsptl, main="Days_spend_hsptl", boxwex=0.1,horizontal = TRUE)#***

hist(insout$Days_spend_hsptl)
outliers10<-boxplot(insout$Days_spend_hsptl)$out
insout[insout$Days_spend_hsptl %in% outliers10, "Days_spend_hsptl"] = NA
sum(is.na(insout$Days_spend_hsptl))

#*****************************************************8
boxplot(insdf$Admission_type, main="Admission_type", boxwex=0.1,horizontal = TRUE) #
hist(insout$Admission_type)
outliers11<-boxplot(insout$Admission_type)$out
insout[insout$Admission_type %in% outliers11, "Admission_type"] = NA
sum(is.na(insout$Admission_type))
#******************************
boxplot(insdf$Home.or.self.care., main="Home.or.self.care.", boxwex=0.1,horizontal = TRUE) #max
hist(insout$Home.or.self.care.)
outliers12<-boxplot(insout$Home.or.self.care.)$out
insout[insout$Home.or.self.care. %in% outliers12, "Home.or.self.care."] = NA
sum(is.na(insout$Home.or.self.care.))

#**********************************************
boxplot(insdf$year_discharge, main="year_discharge", boxwex=0.1,horizontal = TRUE)#***
hist(insout$year_discharge)

#The following column has no valuable data , so it can be omitted
data = subset(data1, select = -c(Area_Service,Hospital.County,Certificate_num,Hospital.Id,zip_code_3_digits))

insout = subset(insout, select = -c(year_discharge))

#****************************
boxplot(insdf$ccs_diagnosis_code, main="ccs_diagnosis_code", boxwex=0.1,horizontal = TRUE)#
hist(insout$ccs_diagnosis_code)
quantile(insout$ccs_diagnosis_code)
outliers13<-boxplot(insout$ccs_diagnosis_code)$out
insout[insout$ccs_diagnosis_code %in% outliers13, "ccs_diagnosis_code"] = NA
sum(is.na(insout$ccs_diagnosis_code))

#****************************
boxplot(insdf$ccs_diagnosis_description, main="ccs_diagnosis_description", boxwex=0.1,horizontal = TRUE)
hist(insout$ccs_diagnosis_description)
outliers14<-boxplot(insout$ccs_diagnosis_description)$out
insout[insout$ccs_diagnosis_description %in% outliers14, "ccs_diagnosis_description"] = NA
sum(is.na(insout$ccs_diagnosis_description))
#No  outliers

#****************************
boxplot(insdf$ccs_procedure_code, main="ccs_procedure_code", boxwex=0.1,horizontal = TRUE)
hist(insout$ccs_procedure_code)
outliers15<-boxplot(insout$ccs_procedure_code)$out
insout[insout$ccs_procedure_code %in% outliers15, "ccs_procedure_code"] = NA
sum(is.na(insout$ccs_procedure_code))

#NO  outliers

#******************************
boxplot(insdf$ccs_procedure_description, main="ccs_procedure_description", boxwex=0.1,horizontal = TRUE)
hist(insout$ccs_procedure_description)
outliers16<-boxplot(insout$ccs_procedure_description)$out
insout[insout$ccs_procedure_description %in% outliers13, "ccs_procedure_description"] = NA
sum(is.na(insout$ccs_procedure_description))

#NO  outliers

#********************************
boxplot(insdf$apr_drg_description, main="apr_drg_description", boxwex=0.1,horizontal = TRUE)
hist(insout$apr_drg_description)
outliers17<-boxplot(insout$apr_drg_description)$out
insout[insout$apr_drg_description %in% outliers13, "apr_drg_description"] = NA
sum(is.na(insout$apr_drg_description))

#NO  outliers

#*******************************
boxplot(insdf$apr_mdc_description, main="apr_mdc_description", boxwex=0.1,horizontal = TRUE)
hist(insout$apr_mdc_description)
outliers18<-boxplot(insout$apr_mdc_description)$out
insout[insout$ccs_diagnosis_code %in% outliers13, "ccs_diagnosis_code"] = NA
sum(is.na(insout$ccs_diagnosis_code))

#NO  outliers

#*******************************
boxplot(insdf$Code_illness, main="Code_illness", boxwex=0.1,horizontal = TRUE)
hist(insout$Code_illness)
outliers19<-boxplot(insout$Code_illness)$out
insout[insout$Code_illness %in% outliers13, "Code_illness"] = NA
sum(is.na(insout$Code_illness))

# NO outliers

#****************************
boxplot(insdf$Description_illness, main="Description_illness", boxwex=0.1,horizontal = TRUE)
hist(insout$Description_illness)
outliers20<-boxplot(insout$Description_illness)$out
insout[insout$Description_illness %in% outliers13, "Description_illness"] = NA
sum(is.na(insout$Description_illness))

#NO outliers

#****************************
boxplot(insdf$Mortality.risk, main="Mortality.risk", boxwex=0.1,horizontal=TRUE)
hist(insout$Mortality.risk)
outliers21<-boxplot(insout$Mortality.risk)$out
insout[insout$Mortality.risk %in% outliers21, "Mortality.risk"] = NA
sum(is.na(insout$Mortality.risk))

#*********************************
boxplot(insdf$Surg_Description, main="Surg_Description", boxwex=0.1,horizontal = TRUE)
hist(insout$Surg_Description)
outliers22<-boxplot(insout$Surg_Description)$out
insout[insout$Surg_Description %in% outliers22, "Surg_Description"] = NA
sum(is.na(insout$Surg_Description))

#No outliers

#*********************************
boxplot(insdf$Payment_typology_1, main="Payment_typology_1", boxwex=0.1,horizontal = TRUE)
hist(insout$Payment_typology_1)
outliers23<-boxplot(insout$Payment_typology_1)$out
insout[insout$Payment_typology_1 %in% outliers23, "Payment_typology_1"] = NA
sum(is.na(insout$Payment_typology_1))

#********************************
boxplot(insdf$Weight_baby, main="Weight_baby", boxwex=0.1,horizontal = TRUE)


#This column has no valuable data and should be dropped
insout = subset(insout, select = -c(Weight_baby))

#***********************************
boxplot(insdf$Abortion, main="Abortion", boxwex=0.1,horizontal=TRUE)
hist(insout$Abortion)
hist(ins$Abortion)
outliers24<-boxplot(insout$Abortion)$out
insout[insout$Abortion %in% outliers24, "Abortion"] = NA
sum(is.na(insout$Abortion))

#*********************************
boxplot(insdf$Tot_charg, main="Tot_charg", boxwex=0.1,horizontal = TRUE)
hist(insout$Tot_charg)
outliers25<-boxplot(insout$Tot_charg)$out
insout[insout$Tot_charg %in% outliers25, "Tot_charg"] = NA
sum(is.na(insout$Tot_charg))

#********************************
boxplot(insdf$Tot_cost, main="Tot_cost", boxwex=0.1,horizontal = TRUE)
hist(insout$Tot_cost)
outliers26<-boxplot(insout$Tot_cost)$out
insout[insout$Tot_cost %in% outliers26, "Tot_cost"] = NA
sum(is.na(insout$Tot_cost))

#********************************
boxplot(insdf$ratio_of_total_costs_to_total_charges, main="ratio_of_total_costs_to_total_charges", boxwex=0.1,horizontal=TRUE)
hist(insout$ratio_of_total_costs_to_total_charges)
outliers27<-boxplot(insout$ratio_of_total_costs_to_total_charges)$out
insout[insout$ratio_of_total_costs_to_total_charges %in% outliers27, "ratio_of_total_costs_to_total_charges"] = NA
sum(is.na(insout$ratio_of_total_costs_to_total_charges))

#*******************************
boxplot(insdf$Emergency.dept_yes.No,horizontal = TRUE)
hist(insout$Emergency.dept_yes.No)
outliers28<-boxplot(insout$Emergency.dept_yes.No)$out
insout[insout$Emergency.dept_yes.No %in% outliers27, "Emergency.dept_yes.No"] = NA
sum(is.na(insout$Emergency.dept_yes.No))



#********************************
#After having a analysis on the above results,
 #Some of the data needs to be deleted while some need to be imputed
#Perfoming the following operations

#Deleting the required columns
insout<-subset(insout,select = -c(zip_code,Days_spend_hsptl))
insout<-subset(insout,select = -c(Certificate_num))

#Replacing the NA values from following columns with their mean
insout$Tot_charg[is.na(insout$Tot_charg)] <- mean(insout$Tot_charg, na.rm = T)
hist(insout$Tot_charg)
hist(insdf$Tot_charg)

insout$Tot_cost[is.na(insout$Tot_cost)] <- mean(insout$Tot_cost, na.rm = T)
hist(insout$Tot_cost)
hist(insdf$Tot_cost)


insout$ratio_of_total_costs_to_total_charges[is.na(insout$ratio_of_total_costs_to_total_charges)] <- mean(insout$ratio_of_total_costs_to_total_charges, na.rm = TRUE)
hist(insdf$ratio_of_total_costs_to_total_charges)
hist(insout$ratio_of_total_costs_to_total_charges)



insout$Hospital.Id[is.na(insout$Hospital.Id)] <- mean(insout$Hospital.Id, na.rm = TRUE)
hist(insdf$Hospital.Id)
hist(insout$Hospital.Id)

#*********************************************************

#For the current dataset , we can neglect (Hospital.County) column to run the model
insout<-subset(insout,select=-c(Hospital.County))

#***************************************************************
sum(is.na(insout))







#***************************************************************
#Scatter plot for the data
ins$Area_Service<-as.numeric(ins$Area_Service)
ins$Hospital.County<-as.numeric(ins$Hospital.County)




View(insdf)
library(car)
install.packages("RColorBrewer")
library(RColorBrewer)
names <-c("Area_Service","Hospital.County")
mylist<- list(ins$Area_Service,ins$Hospital.County)
makeProfilePlot(mylist,names)


#****************************************************8
makeProfilePlot <- function(mylist,names)
{
        require(RColorBrewer)
        # find out how many variables we want to include
        numvariables <- length(mylist)
        # choose 'numvariables' random colours
        colours <- brewer.pal(numvariables,"Set1")
        # find out the minimum and maximum values of the variables:
        mymin <- 1e+20
        mymax <- 1e-20
        for (i in 1:numvariables)
        {
                vectori <- mylist[[i]]
                mini <- min(vectori)
                maxi <- max(vectori)
                if (mini < mymin) { mymin <- mini }
                if (maxi > mymax) { mymax <- maxi }
        }
        # plot the variables
        for (i in 1:numvariables)
        {
                vectori <- mylist[[i]]
                namei <- names[i]
                colouri <- colours[i]
                if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
                else         { points(vectori, col=colouri,type="l")                                     }
                lastxval <- length(vectori)
                lastyval <- vectori[length(vectori)]
                text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
        }
}


#**********************************************************
#Multivariate Anayasis
View(ins)
insb<-ins[c(1:3,32)]
View(insb)
install.packages("GGally")
install.packages("ggplot2")

library(GGally)
library(ggplot2)
ggpairs(insb)+ theme_bw()

sum(is.na(ins$Certificate_num))

p <- ggpairs(insb, aes(color = Result))+ theme_bw()
# Change color manually.
# Loop through each plot changing relevant scales
for(i in 1:p$nrow) {
        for(j in 1:p$ncol){
                p[i,j] <- p[i,j] + 
                        scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) +
                        scale_color_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))  
        }
}

p


















