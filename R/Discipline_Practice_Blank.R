
# R-Ladies St. Louis Cleaning Messy Data 11/12/19 ------------------------------------------

##Install Packages
##install.packages(c("tidyverse", "lubridate","labelled", "dataMaid", "janitor")

##Library Packages
#library(tidyverse)
#library(lubridate)
#library(labelled)

#My plan
#- Read in the file
#- Drop unncessary variables
#- Rename columns
#- Create a mont column
#- Create an ISS column
#- Filter to only ISS cases
#- Group by StudentID and Month and count ISS incidents for Fall and Spring
#- Merge the Fall and Spring together
#- Fill NA ISS with zeros
#- Add variable labels

# Read in our Discipline file  -------------------------------------------------------------

##(1) Read in our discipline file using `read_csv`
ISS<-read_csv('https://raw.githubusercontent.com/Cghlewis/R-Ladies-STL-Cleaning-Data-R/master/Data/Discipline.csv')

head(as.data.frame(ISS))
glimpse(ISS)

##(1) Drop Incident Description and Incident Place using `select`
names(ISS)


##(2) Rename the Student ID column using `rename`
names(ISS)



##(3) Create a month column using `mutate`, `mdy` and `month`
head(as.data.frame(ISS))
glimpse(ISS)



head(as.data.frame(ISS))

##(4) Create a column with just ISS using `str_extract`



head(as.data.frame(ISS))

##(5) Filter to keep only the rows where ISS occurred using `filter`



##(6) Count the number of ISS per Student for fall/spring using `filter`, `group_by` and `summarise`

#Make a spring ISS count dataset
ISS_S<-ISS%>%filter(Month<6)%>%group_by(Student_ID)%>%
  summarise(Spring_ISS=n())

head(as.data.frame(ISS_S))

#Make a fall ISS count dataset




head(as.data.frame(ISS_F))

## (7) Merge the two data together



## (8) Fill NA ISS with 0s using `replace_na`
ISSJoin<-ISSJoin%>%tidyr::replace_na(list(Fall_ISS=0, Spring_ISS=0))

## (9) Add variable labels using `var_label`
names(ISSJoin)
var_label(ISSJoin) <- list(Student_ID="Student ID", Fall_ISS="Fall ISS referrals",
                        Spring_ISS="Spring ISS referrals")


##Make codebook using `makeCodebook`------------------------------------------------------

getwd()

dataMaid::makeCodebook(ISSJoin, reportTitle="Codebook for R-ladies Discipline Data", replace=TRUE)


###Export data using `write_csv`----------------------------------------------------------

write_csv(ISSJoin,"Clean_Discipline_2018_19.csv")


# Restructure data from wide to long (to make data Tidy) ---------------------------------

#Code if you wanted to turn your wide dataframe into long format, tidy data
ISSLong<-ISSJoin%>%pivot_longer(cols=contains("ISS"), 
                          names_to=c("Time",".value"), names_sep="_")


