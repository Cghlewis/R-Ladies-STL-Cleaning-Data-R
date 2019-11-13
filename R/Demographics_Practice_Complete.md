Demographics Cleaning Practice
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

What does our data currently look
like?

| School District             | Student Name        | Student ID  | Race | Student Gender | ZipCode | % present fall | % present spring |
| :-------------------------- | :------------------ | :---------- | :--- | -------------: | ------: | :------------- | ---------------: |
| Primary School - District A | LastName, FirstName | 71111337335 | W    |              1 |  555444 | 99.42%         |            93.86 |
| Middle School - District A  | LastName, FirstName | \*          | W    |              2 |  555444 | 97.37%         |            96.78 |
| Primary School - District A | LastName, FirstName | 71113153739 | B    |              2 |  555444 | 97.08%         |            93.86 |

What do we want our data to look
like?

| School         | District   | Student\_ID | Race  | Gender | Fall\_Attendance | Spring\_Attendance |
| :------------- | :--------- | ----------: | :---- | :----- | ---------------: | -----------------: |
| Primary School | District A | 71111337335 | White | Male   |            99.42 |              93.86 |
| Primary School | District A | 71113153739 | Black | Female |            97.08 |              93.86 |
| High School    | District A | 71113195575 | White | Male   |            94.44 |              96.49 |

##### My plan

###### (1) Read the data in

###### (2) Select variables to keep

###### (3) Rename variables

###### (4) Filter out Students with NA Student IDs

###### (5) Check for any duplicate rows

###### (6) Transform the School District column

###### (7) Remove the % sign from the Fall\_Attendance column

###### (8) Recode Race and Gender

###### (9) Add variable labels

Read in our demographics file using
`read_csv`

``` r
Demo<-read_csv('https://raw.githubusercontent.com/Cghlewis/R-Ladies-STL-Cleaning-Data-R/master/Data/Demographics.csv', na="*")
```

Drop StudentName and ZipCode using `select`

``` r
names(Demo)
```

    ## [1] "School District"  "Student Name"     "Student ID"      
    ## [4] "Race"             "Student Gender"   "ZipCode"         
    ## [7] "% present fall"   "% present spring"

``` r
Demo<-Demo%>%select(-`Student Name`, -ZipCode)
```

Rename Columns using `setNames`

``` r
names(Demo)
Demo<-Demo%>%setNames(c("School_District", "Student_ID","Race", "Gender",
                                "Fall_Attendance", "Spring_Attendance"))
```

Filter out any NA StudentID using
    `filter`

``` r
summary(Demo$Student_ID)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
    ## 7.111e+10 7.320e+10 7.557e+10 7.557e+10 7.793e+10 8.000e+10         2

``` r
Demo<-Demo%>%filter(!is.na(Student_ID))

summary(Demo$Student_ID)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## 7.111e+10 7.320e+10 7.557e+10 7.557e+10 7.793e+10 8.000e+10

Check for duplicates and remove using `distinct`

``` r
dim(Demo)
```

    ## [1] 1382    6

``` r
distinct(Demo)%>%count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  1380

``` r
Demo<-distinct(Demo)

dim(Demo)
```

    ## [1] 1380    6

Transform School\_District using
    `separate`

``` r
head(as.data.frame(Demo),2)
```

    ##               School_District  Student_ID Race Gender Fall_Attendance
    ## 1 Primary School - District A 71111337335    W      1          99.42%
    ## 2 Primary School - District A 71113153739    B      2          97.08%
    ##   Spring_Attendance
    ## 1             93.86
    ## 2             93.86

``` r
Demo<-Demo%>%separate(School_District,into=c("School","District"), sep='-')
head(as.data.frame(Demo),2)
```

    ##            School    District  Student_ID Race Gender Fall_Attendance
    ## 1 Primary School   District A 71111337335    W      1          99.42%
    ## 2 Primary School   District A 71113153739    B      2          97.08%
    ##   Spring_Attendance
    ## 1             93.86
    ## 2             93.86

Change Fall\_Attendance class into numeric using `as.numeric` and remove
the % using
    `str_remove`

``` r
head(as.data.frame(Demo),2)
```

    ##            School    District  Student_ID Race Gender Fall_Attendance
    ## 1 Primary School   District A 71111337335    W      1          99.42%
    ## 2 Primary School   District A 71113153739    B      2          97.08%
    ##   Spring_Attendance
    ## 1             93.86
    ## 2             93.86

``` r
class(Demo$Fall_Attendance)
```

    ## [1] "character"

``` r
Demo<-Demo%>%mutate(Fall_Attendance=as.numeric(str_remove(Fall_Attendance,"%")))
head(as.data.frame(Demo),2)
```

    ##            School    District  Student_ID Race Gender Fall_Attendance
    ## 1 Primary School   District A 71111337335    W      1           99.42
    ## 2 Primary School   District A 71113153739    B      2           97.08
    ##   Spring_Attendance
    ## 1             93.86
    ## 2             93.86

Last, recode variables.

Change Race class into a factor and recode values using `recode_factor`

``` r
class(Demo$Race)
```

    ## [1] "character"

``` r
table(Demo$Race)
```

    ## 
    ##    A    B    H    I    P    W 
    ##   29   82   32   17    3 1217

``` r
Demo<-Demo%>%mutate(Race=recode_factor(Race, 'W'="White", 'I'="American Indian",
                                       'A'="Asian", 'B'="Black", 'H'="Latino/Hispanic",
                                       'P'="Native Hawaiian/Pacific Islander"))

levels(Demo$Race)
```

    ## [1] "White"                            "American Indian"                 
    ## [3] "Asian"                            "Black"                           
    ## [5] "Latino/Hispanic"                  "Native Hawaiian/Pacific Islander"

``` r
janitor::tabyl(Demo$Race)
```

    ##                         Demo$Race    n     percent
    ##                             White 1217 0.881884058
    ##                   American Indian   17 0.012318841
    ##                             Asian   29 0.021014493
    ##                             Black   82 0.059420290
    ##                   Latino/Hispanic   32 0.023188406
    ##  Native Hawaiian/Pacific Islander    3 0.002173913

Change Gender class into factor and recode values using `recode_factor`

``` r
class(Demo$Gender)
```

    ## [1] "numeric"

``` r
table(Demo$Gender)
```

    ## 
    ##   1   2 
    ## 744 636

``` r
Demo<-Demo%>%mutate(Gender=recode_factor(Gender,`1`="Male", `2`="Female"))

levels(Demo$Gender)
```

    ## [1] "Male"   "Female"

Add variable labels using `var_label`

``` r
names(Demo)
```

    ## [1] "School"            "District"          "Student_ID"       
    ## [4] "Race"              "Gender"            "Fall_Attendance"  
    ## [7] "Spring_Attendance"

``` r
var_label(Demo) <- list(School = "SchoolName",District="DistrictName", 
                                Student_ID="Student ID", Race="Student Race", 
                                Gender="Student Gender", Fall_Attendance="Percent Attendance Fall", 
                                Spring_Attendance="Percent Attendance Spring")
```

##### While the code above works just fine, we could also write code chunks together using %\>%. Then we only have to call the data once rather than 8 times.

``` r
Demo<-Demo%>%select(-`Student Name`, -ZipCode)%>%
  setNames(c("School_District", "Student_ID","Race", "Gender",
            "Fall_Attendance", "Spring_Attendance"))%>%
  filter(!is.na(Student_ID))%>%distinct()%>%
  separate(School_District,into=c("School","District"), sep='-')%>%
  mutate(Fall_Attendance=as.numeric(str_remove(Fall_Attendance,"%")),
                                    Race=recode_factor(Race, 'W'="White", 'I'="American Indian",
                                       'A'="Asian", 'B'="Black", 'H'="Latino/Hispanic",
                                       'P'="Native Hawaiian/Pacific Islander"),
                                    Gender=recode_factor(Gender,`1`="Male", `2`="Female"))
```

#### Last we can make a codebook of variable descriptives

Make a codebook using
`makeCodebook`

``` r
dataMaid::makeCodebook(Demo, reportTitle="Codebook for R-ladies Demographic Data")
```

#### And we can export our data to share with others or analyze in other programs.

Export Data using
`write_csv`

``` r
write_csv(Demo,"Clean_Demographics_2018_19.csv")
```

#### Also, if you wanted your data in tidy format, you could restructure from wide to long

``` r
DemoLong<-Demo%>%pivot_longer(cols=contains("Attend"), 
                                          names_to=c("Time",".value"), names_sep="_")

head(as.data.frame(DemoLong))
```

    ##            School    District  Student_ID  Race Gender   Time Attendance
    ## 1 Primary School   District A 71111337335 White   Male   Fall      99.42
    ## 2 Primary School   District A 71111337335 White   Male Spring      93.86
    ## 3 Primary School   District A 71113153739 Black Female   Fall      97.08
    ## 4 Primary School   District A 71113153739 Black Female Spring      93.86
    ## 5    High School   District A 71113195575 White   Male   Fall      94.44
    ## 6    High School   District A 71113195575 White   Male Spring      96.49
