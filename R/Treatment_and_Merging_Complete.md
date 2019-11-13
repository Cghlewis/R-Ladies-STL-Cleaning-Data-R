Cleaning Study Data and Merging Practice
================

#### Crystal Lewis

#### 11/12/2019

#### Install and call packages we need

Install
packages

``` r
install.packages(c("tidyverse", "lubridate","labelled", "dataMaid", "janitor"))
```

Call the packages

``` r
library(tidyverse)
library(lubridate)
library(labelled)
```

Let’s look at our treatment data

| Study\_ID | Student\_ID | Treatment | LikertQ1 | LikertQ2 | LikertQ3 |
| --------: | ----------: | --------: | -------: | -------: | -------: |
|         1 | 71199755559 |         0 |        1 |        2 |        1 |
|         2 | 73951519331 |         1 |        4 |        3 |        1 |
|         3 | 71397595973 |         1 |        3 |        3 |        4 |

    ##     Study_ID     Student_ID          Treatment         LikertQ1    
    ##  Min.   :1.0   Min.   :7.120e+10   Min.   :0.0000   Min.   :1.000  
    ##  1st Qu.:1.5   1st Qu.:7.130e+10   1st Qu.:0.5000   1st Qu.:2.000  
    ##  Median :2.0   Median :7.140e+10   Median :1.0000   Median :3.000  
    ##  Mean   :2.0   Mean   :7.218e+10   Mean   :0.6667   Mean   :2.667  
    ##  3rd Qu.:2.5   3rd Qu.:7.267e+10   3rd Qu.:1.0000   3rd Qu.:3.500  
    ##  Max.   :3.0   Max.   :7.395e+10   Max.   :1.0000   Max.   :4.000  
    ##     LikertQ2        LikertQ3  
    ##  Min.   :2.000   Min.   :1.0  
    ##  1st Qu.:2.500   1st Qu.:1.0  
    ##  Median :3.000   Median :1.0  
    ##  Mean   :2.667   Mean   :2.0  
    ##  3rd Qu.:3.000   3rd Qu.:2.5  
    ##  Max.   :3.000   Max.   :4.0

#### My plan to clean up the treatment data

###### (1) Recode Likert questions and add labels

###### (2) Add labels to treatment vars

###### (3) Add variable labels

Q1 just needs value labels using
`labelled`

``` r
StudyData$LikertQ1<-labelled (StudyData$LikertQ1, c("Strongly Disagree"=1, "Disagree"=2,
                                                    "Agree"=3, "Strongly Agree"=4))
val_labels(StudyData$LikertQ1)
```

    ## Strongly Disagree          Disagree             Agree    Strongly Agree 
    ##                 1                 2                 3                 4

Q2 just needs value labels using
`labelled`

``` r
StudyData$LikertQ2<-labelled (StudyData$LikertQ2, c("Strongly Disagree"=1, "Disagree"=2,
                                                    "Agree"=3, "Strongly Agree"=4))
val_labels(StudyData$LikertQ2)
```

    ## Strongly Disagree          Disagree             Agree    Strongly Agree 
    ##                 1                 2                 3                 4

Q3 needs to be reverse coded and labels added using `recode` and
`labelled`

``` r
StudyData<-StudyData%>%mutate(LikertQ3.R=recode(LikertQ3, `1`=4, `2`=3, `3`=2, `4`=1))

StudyData$LikertQ3.R<-labelled (StudyData$LikertQ1, c("Strongly Disagree"=4, "Disagree"=3,
                                                    "Agree"=2, "Strongly Agree"=1))

table(StudyData$LikertQ3.R, StudyData$LikertQ3)
```

    ##    
    ##     1 4
    ##   1 1 0
    ##   3 0 1
    ##   4 1 0

``` r
StudyData<-StudyData%>%select(-LikertQ3)
```

Add value labels to treatment using
`labelled`

``` r
StudyData$Treatment<-labelled (StudyData$Treatment, c("Control"=0, "Treatment"=1))
```

Add variable labels using
`labelled`

``` r
var_label(StudyData) <- list(Study_ID = "Study ID",Student_ID="District ID", 
                        LikertQ1="I like school?", LikertQ2="I like my teacher", 
                        LikertQ3.R="I don't want to come to school", Treatment="Treatment status")
```

#### Now we can read in discipline and demographics so we can merge it with our study data

Read in our two district files that are
cleaned

``` r
Demo<-read_csv("https://raw.githubusercontent.com/Cghlewis/R-Ladies-STL-Cleaning-Data-R/master/Data/Clean_Demographics_2018_19.csv")
Discipline<-read_csv("https://raw.githubusercontent.com/Cghlewis/R-Ladies-STL-Cleaning-Data-R/master/Data/Clean_Discipline_2018_19.csv")
```

Merge study data with district data

``` r
StudyDataComplete<-StudyData%>%left_join(Demo,by="Student_ID")%>%
  left_join(Discipline, by="Student_ID")

knitr::kable(head(StudyDataComplete))
```

| Study\_ID | Student\_ID | Treatment | LikertQ1 | LikertQ2 | LikertQ3.R | School        | District   | Race  | Gender | Fall\_Attendance | Spring\_Attendance | Fall\_ISS | Spring\_ISS |
| --------: | ----------: | --------: | -------: | -------: | ---------: | :------------ | :--------- | :---- | :----- | ---------------: | -----------------: | --------: | ----------: |
|         1 | 71199755559 |         0 |        1 |        2 |          1 | Middle School | District A | White | Male   |            95.32 |              95.32 |         2 |           0 |
|         2 | 73951519331 |         1 |        4 |        3 |          4 | Middle School | District A | White | Male   |            98.25 |             100.00 |         1 |           2 |
|         3 | 71397595973 |         1 |        3 |        3 |          3 | High School   | District A | White | Male   |            97.37 |              98.25 |         0 |           1 |
