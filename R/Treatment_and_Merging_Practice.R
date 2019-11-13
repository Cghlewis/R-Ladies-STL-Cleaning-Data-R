library(tidyverse)
library(labelled)

###Make a sample research study dataframe

StudyData <- data.frame("Study_ID" = 1:3, 
                        "Student_ID"=c(71199755559,73951519331, 71397595973),"Treatment"=c(0,1,1),
                        "LikertQ1" = c(1,4,3), "LikertQ2"=c(2,3,3), "LikertQ3"=c(1,1,4))


####Recode Likert and add labels

##Q1 just needs value labels
StudyData$LikertQ1<-labelled (StudyData$LikertQ1, c("Strongly Disagree"=1, "Disagree"=2,
                                                    "Agree"=3, "Strongly Agree"=4))

##Q2 just needs value labels
StudyData$LikertQ2<-labelled (StudyData$LikertQ2, c("Strongly Disagree"=1, "Disagree"=2,
                                                    "Agree"=3, "Strongly Agree"=4))

##Q3 needs to be reverse coded and labels added
StudyData<-StudyData%>%mutate(LikertQ3.R=recode(LikertQ3, `1`=4, `2`=3, `3`=2, `4`=1))

StudyData$LikertQ3.R<-labelled (StudyData$LikertQ3.R, c("Strongly Disagree"=4, "Disagree"=3,
                                                    "Agree"=2, "Strongly Agree"=1))

StudyData<-StudyData%>%select(-LikertQ3)

####Add value labels to treatment
StudyData$Treatment<-labelled (StudyData$Treatment, c("Control"=0, "Treatment"=1))

###Add variable labels
var_label(StudyData) <- list(Study_ID = "Study ID",Student_ID="District ID", 
                        LikertQ1="I like school?", LikertQ2="I like my teacher", 
                        LikertQ3.R="I don't want to come to school", Treatment="Treatment status")

####Merging Study Data with District Data---------------------------------------------------

Demo<-read_csv("https://raw.githubusercontent.com/Cghlewis/R-Ladies-STL-Cleaning-Data-R/master/Data/Clean_Demographics_2018_19.csv")
Discipline<-read_csv("https://raw.githubusercontent.com/Cghlewis/R-Ladies-STL-Cleaning-Data-R/master/Data/Clean_Discipline_2018_19.csv")


StudyDataComplete<-StudyData%>%left_join(Demo,by="Student_ID")%>%
  left_join(Discipline, by="Student_ID")

head(as.data.frame(StudyDataComplete))

###Export data
write_csv(StudyDataComplete,"Study_Data_Complete_2018_19.csv")
