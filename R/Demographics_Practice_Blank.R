
# R-Ladies St. Louis Cleaning Messy Data 11/12/19 --------------------------------------------

##Install Packages
##install.packages(c("tidyverse", "lubridate","labelled", "dataMaid", "janitor")

##Library Packages
#library(tidyverse)
#library(lubridate)
#library(labelled)

#My plan
#- Read the data in
#- Select variables to keep
#- Rename variables
#- Filter out Students with NA Student IDs
#- Check for any duplicate rows
#- Transform the School District column
#- Remove the % sign from the Fall_Attendance column
#- Recode Race and Gender
#- Add variable labels


# Read in our Demographics file  -------------------------------------------------

##(1) Load our demographics using `read_csv`
Demo<-read_csv('https://raw.githubusercontent.com/Cghlewis/R-Ladies-STL-Cleaning-Data-R/master/Data/Demographics.csv', na="*")

head(as.data.frame(Demo))
glimpse(Demo)

##(2) Drop StudentName and ZipCode using `select`
names(Demo)



##(3) Rename Columns using `setNames`
names(Demo)

Demo<-Demo%>%setNames(c("School_District", "Student_ID","Race", "Gender",
                        "Fall_Attendance", "Spring_Attendance"))

##(4) Filter any NA StudentID using `filter`
summary(Demo)



summary(Demo)

##(5) Check for duplicates and remove using `distinct`
dim(Demo)

distinct(Demo)%>%count()




dim(Demo)

##(6) Transform School_District using `separate` 
head(as.data.frame(Demo), 2)




head(as.data.frame(Demo), 2)

##(7) Change Fall_Attendance class into numeric using `as.numeric` and remove the % using `str_remove`
class(Demo$Fall_Attendance)
head(as.data.frame(Demo),2)




head(as.data.frame(Demo),2)
class(Demo$Fall_Attendance)

##(8) Recode variables

###Change Race class into a factor and recode values using `recode_factor`
class(Demo$Race)
table(Demo$Race)

Demo<-Demo%>%mutate(Race=recode_factor(Race, 'W'="White", 'I'="American Indian",
                                       'A'="Asian", 'B'="Black", 'H'="Latino/Hispanic",
                                       'P'="Native Hawaiian/Pacific Islander"))

levels(Demo$Race)

##It also is good practice to recode this into a new variable such as Race.f and check that the
#recode worked correctly doing a crosstab with the new and old variable

model<-lm(Fall_Attendance~Race,data=Demo)
summary(model)

###Change Gender class into factor and recode values using `recode_factor`
class(Demo$Gender)
table(Demo$Gender)





levels(Demo$Gender)

##Other option if you are not using this data for analysis is to keep gender as numeric
  #and just add value labels, and the the researcher can recode as they see fit.
  #In this case you could leave the values as 1 and 2 and then run this code
      #Demo$Gender<-labelled (Demo$Gender, c("male"=1, "female"=2))
  #This label will appear when the data is imported into programs such as SPSS or Stata
  #If you export to excel, csv, etc. you would provide the researcher with a codebook to know 
  #what the values equal



## (9) Add variable labels using `var_label`
names(Demo)
var_label(Demo) <- list(School = "SchoolName",District="DistrictName", 
                                Student_ID="Student ID", Race="Student Race", 
                                Gender="Student Gender", Fall_Attendance="Percent Attendance Fall", 
                                Spring_Attendance="Percent Attendance Spring")



##Make codebook using `makeCodebook`-------------------------------------------------------

getwd()

dataMaid::makeCodebook(Demo, reportTitle="Codebook for R-ladies Demographics Data", replace=TRUE)


###Export data using `write_csv`-----------------------------------------------------------

write_csv(Demo,"Clean_Demographics_2018_19.csv")


# Restructure data from wide to long (to make data Tidy) ----------------------------------

#Code if you wanted to turn your wide dataframe into long format, tidy data
Long<-Demo%>%pivot_longer(cols=contains("Attend"), 
                                          names_to=c("Time",".value"), names_sep="_")
