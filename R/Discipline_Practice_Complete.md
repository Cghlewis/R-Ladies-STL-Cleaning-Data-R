Discipline Cleaning Practice
================

#### Crystal Lewis

#### 11/12/2019

#### Install and call packages we need

Install packages

``` r
install.packages(c("tidyverse", "lubridate","labelled", "dataMaid"))
```

Call the packages

``` r
library(tidyverse)
library(lubridate)
library(labelled)
```

What does our data currently look
like?

|  Student ID | Incident Description   | Incident Place | Action                       | Date      |
| ----------: | :--------------------- | :------------- | :--------------------------- | :-------- |
| 71519575957 | Incident that happened | Classroom      | WARNING                      | 1/16/2019 |
| 75533971751 | Incident that happened | School Bus     | PRINCIPAL/STUDENT CONFERENCE | 3/15/2019 |
| 75533971751 | Incident that happened | School Bus     | PARENT CONTACT / CONFERENCE  | 3/18/2019 |

What do we want our data to look like?

| Student\_ID | Fall\_ISS | Spring\_ISS |
| ----------: | --------: | ----------: |
| 71117331939 |         2 |           1 |
| 71199755559 |         2 |           0 |
| 71335157937 |         2 |           2 |

##### My plan

###### (1) Read in the file

###### (2) Drop unncessary variables

###### (3) Rename columns

###### (4) Create a mont column

###### (5) Create an ISS column

###### (6) Filter to only ISS cases

###### (7) Group by StudentID and Month and count ISS incidents for Fall and Spring

###### (8) Merge the Fall and Spring together

###### (9) Fill NA ISS with zeros

###### (10) Add variable labels

Read in our discipline file using
`read_csv`

``` r
ISS<-read_csv('https://raw.githubusercontent.com/Cghlewis/R-Ladies-STL-Cleaning-Data-R/master/Data/Discipline.csv')
```

Drop Incident Description and Incident Place using
    `select`

``` r
names(ISS)
```

    ## [1] "Student ID"           "Incident Description" "Incident Place"      
    ## [4] "Action"               "Date"

``` r
ISS<-ISS%>%select(-contains("Incident"))
```

Rename the Student ID column using `rename`

``` r
names(ISS)
ISS<-ISS%>%rename(Student_ID=`Student ID`)
```

Create a month column using `mutate`, `mdy` and `month`

``` r
glimpse(ISS)
```

    ## Observations: 1,132
    ## Variables: 3
    ## $ Student_ID <dbl> 71519575957, 75533971751, 75533971751, 75335915713,...
    ## $ Action     <chr> "WARNING", "PRINCIPAL/STUDENT CONFERENCE", "PARENT ...
    ## $ Date       <chr> "1/16/2019", "3/15/2019", "3/18/2019", "9/13/2018",...

``` r
ISS<-ISS%>%mutate(Month=month(mdy(Date)))
head(as.data.frame(ISS))
```

    ##    Student_ID                       Action      Date Month
    ## 1 71519575957                      WARNING 1/16/2019     1
    ## 2 75533971751 PRINCIPAL/STUDENT CONFERENCE 3/15/2019     3
    ## 3 75533971751  PARENT CONTACT / CONFERENCE 3/18/2019     3
    ## 4 75335915713 PRINCIPAL/STUDENT CONFERENCE 9/13/2018     9
    ## 5 75335915713 PRINCIPAL/STUDENT CONFERENCE 1/24/2019     1
    ## 6 71531797951                      WARNING 10/3/2018    10

Create a column with just ISS using `str_extract`

``` r
ISS<-ISS%>%mutate(ISSvar=str_extract(Action, "ISS"))
head(as.data.frame(ISS))
```

    ##    Student_ID                       Action      Date Month ISSvar
    ## 1 71519575957                      WARNING 1/16/2019     1   <NA>
    ## 2 75533971751 PRINCIPAL/STUDENT CONFERENCE 3/15/2019     3   <NA>
    ## 3 75533971751  PARENT CONTACT / CONFERENCE 3/18/2019     3   <NA>
    ## 4 75335915713 PRINCIPAL/STUDENT CONFERENCE 9/13/2018     9   <NA>
    ## 5 75335915713 PRINCIPAL/STUDENT CONFERENCE 1/24/2019     1   <NA>
    ## 6 71531797951                      WARNING 10/3/2018    10   <NA>

Filter to keep only the rows where ISS occurred using `filter`

``` r
ISS<-ISS%>%filter(ISSvar=="ISS")
```

Count the number of ISS per Student for fall/spring using `filter`,
`group_by` and `summarise`

``` r
ISS_S<-ISS%>%filter(Month<6)%>%group_by(Student_ID)%>%
  summarise(Spring_ISS=n())

ISS_F<-ISS%>%filter(Month>7)%>%group_by(Student_ID)%>%
  summarise(Fall_ISS=n())

head(as.data.frame(ISS_S))
```

    ##    Student_ID Spring_ISS
    ## 1 71117331939          1
    ## 2 71131591911          3
    ## 3 71131935757          1
    ## 4 71157179571          1
    ## 5 71331575397          1
    ## 6 71333779993          1

``` r
head(as.data.frame(ISS_F))
```

    ##    Student_ID Fall_ISS
    ## 1 71117331939        2
    ## 2 71199755559        2
    ## 3 71335157937        2
    ## 4 71353935953        1
    ## 5 71519711997        1
    ## 6 71551375335        2

Merge the two data together

``` r
ISSJoin<-full_join(ISS_F, ISS_S, by="Student_ID")
head(as.data.frame(ISSJoin))
```

    ##    Student_ID Fall_ISS Spring_ISS
    ## 1 71117331939        2          1
    ## 2 71199755559        2         NA
    ## 3 71335157937        2          2
    ## 4 71353935953        1         NA
    ## 5 71519711997        1         NA
    ## 6 71551375335        2          4

Fill NA ISS with 0s using `replace_na`

``` r
ISSJoin<-ISSJoin%>%tidyr::replace_na(list(Fall_ISS=0, Spring_ISS=0))
```

Add variable labels using
`var_label`

``` r
names(ISSJoin)
```

    ## [1] "Student_ID" "Fall_ISS"   "Spring_ISS"

``` r
var_label(ISSJoin) <- list(Student_ID="Student ID", Fall_ISS="Fall ISS referrals",
                        Spring_ISS="Spring ISS referrals")
```

##### While the code above works just fine, we could also write code chunks 2-6 together using %\>%. Then we only have to call the data once rather than 5 times.

``` r
ISS<-ISS%>%select(-contains("Incident"))%>%
  rename(Student_ID=`Student ID`)%>%
  mutate(Month=month(mdy(Date)), 
         ISSvar=str_extract(Action, "ISS"))%>%
  filter(ISSvar=="ISS")
```

#### Last we can make a codebook of variable descriptives

Make a codebook using
`makeCodebook`

``` r
dataMaid::makeCodebook(ISSJoin, reportTitle="Codebook for R-ladies Discipline Data")
```

#### And we can export our data to share with others or analyze in other programs.

Export Data using
`write_csv`

``` r
write_csv(ISSJoin,"Clean_Discipline_2018_19.csv")
```

#### Also, if you wanted your data in tidy format, you could restructure from wide to long

``` r
ISSLong<-ISSJoin%>%pivot_longer(cols=contains("ISS"), 
                              names_to=c("Time",".value"), names_sep="_")

head(as.data.frame(ISSLong))
```

    ##    Student_ID   Time ISS
    ## 1 71117331939   Fall   2
    ## 2 71117331939 Spring   1
    ## 3 71199755559   Fall   2
    ## 4 71199755559 Spring   0
    ## 5 71335157937   Fall   2
    ## 6 71335157937 Spring   2
