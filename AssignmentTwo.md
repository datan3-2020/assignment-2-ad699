AssignmentTwo
================
ad699
05/02/2020

In this assignment you will work with relational data, i.e. data coming from different data tables that you can combine using keys. Please read ch.13 from R for Data Science before completing this assignment -- <https://r4ds.had.co.nz/relational-data.html>. \# \#\# Read data

We will work with three different tables: household roster from wave 8 (*h\_egoalt*), stable characteristics of individuals (*xwavedat*), and household data from wave 8 (*h\_hhresp*).

``` r
library(tidyverse)
# You need to complete the paths to these files on your computer.
Egoalt8 <- read_tsv("/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w8/h_egoalt.tab")
Stable <- read_tsv("/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
Hh8 <- read_tsv("/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w8/h_hhresp.tab")
```

Filter household roster data (10 points)
----------------------------------------

The **egoalt8** data table contains data on the kin and other relationships between people in the same household. In each row in this table you will have a pair of individuals in the same household: ego (identified by *pidp*) and alter (identified by *apidp*). *h\_relationship\_dv* shows the type of relationship between ego and alter. You can check the codes in the Understanding Society codebooks here -- <https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation>.

First we want to select only pairs of individuals who are husbands and wives or cohabiting partners (codes 1 and 2). For convenience, we also want to keep only the variables *pidp*, *apidp*, *h\_hidp* (household identifier), *h\_relationship\_dv*, *h\_esex* (ego's sex), and *h\_asex* (alter's sex).

``` r
Partners8 <- Egoalt8 %>%
  filter(h_relationship_dv==1 | h_relationship_dv==2) %>%
  select(pidp, apidp, h_hidp, h_relationship_dv, h_sex, h_asex) 
```

Each couple now appears in the data twice: 1) with one partner as ego and the other as alter, 2) the other way round. Now we will only focus on heterosexual couples, and keep one observation per couple with women as egos and men as their alters.

``` r
 # filter out same-sex couples
Hetero8 <- Partners8 %>%
        filter((h_sex==1 & h_asex==2) | (h_sex==2 & h_asex==1))


# keep only one observation per couple with women as egos
Hetero8 <- Partners8 %>%
        filter(h_sex==2)
```

Recode data on ethnicity (10 points)
------------------------------------

In this assignment we will explore ethnic endogamy, i.e. marriages and partnerships within the same ethnic group. First, let us a create a version of the table with stable individual characteristics with two variables only: *pidp* and *racel\_dv* (ethnicity).

``` r
Stable2 <- Stable %>%
        select(pidp, racel_dv)
```

Let's code missing values on ethnicity (-9) as NA.

``` r
Stable2 <- Stable2 %>%
        mutate(racel_dv = recode(racel_dv, `-9` = NA_real_))
```

Now let us recode the variable on ethnicity into a new binary variable with the following values: "White" (codes 1 to 4) and "non-White" (all other codes).

``` r
Stable2 <- Stable2 %>%
        mutate(race = case_when(
          between(racel_dv, 1, 4) ~ "White", 
          racel_dv!="White" ~ "non-White"
        ))
```

Join data (30 points)
---------------------

Now we want to join data from the household roster (*Hetero8*) and the data table with ethnicity (*Stable2*). First let us merge in the data on ego's ethnicity. We want to keep all the observations we have in *Hetero8*, but we don't want to add any other individuals from *Stable2*.

``` r
JoinedEthn <- Hetero8 %>%
  inner_join(Stable2, by = "pidp")
```

Let us rename the variables for ethnicity to clearly indicate that they refer to egos.

``` r
JoinedEthn <- JoinedEthn %>%
        rename(egoRacel_dv = racel_dv) %>%
        rename(egoRace = race)
```

Now let us merge in the data on alter's ethnicity. Note that in this case the key variables have different names in two data tables; please refer to the documentation for your join function (or the relevant section from R for Data Science) to check the solution for this problem.

``` r
# alter are men atm, the alter data will be in the pidp data BEFORE we cut it. Therefore, if I merge the original pidp with racel_dv we should get accurate race values. 

# Partners8 is the dataset with DUPLICATE data. Therefore, I will select all the MEN, and create an alter dataset with race information. 

# Selecting for unique number and sex. 
Partners8.2 <- Partners8 %>%
  select(pidp, h_sex)

# Joining above with relevant racial data.
Partners8.2 <- Partners8.2 %>%
  inner_join(Stable2, by = "pidp")

# Filtering out females (females are the ego in the main data set)
Partners8.2 <- Partners8.2 %>%
  filter(h_sex == 1)

# Renaming the unique number so that it is the key to join to the main data. 
Partners8.2 <- Partners8.2 %>%
  rename(apidp = pidp)

# Getting rid of sex variable as it is already in the main data. 
Partners8.2 <- Partners8.2 %>%
  select(apidp, racel_dv, race)

# Joining to the main data
JoinedEthn <- JoinedEthn %>%
  inner_join(Partners8.2, by = "apidp")
```

*Renaming the variables for alters.*

``` r
JoinedEthn <- JoinedEthn %>%
        rename(alterRacel_dv = racel_dv) %>%
        rename(alterRace = race)
```

Explore probabilities of racial endogamy (20 points)
----------------------------------------------------

``` r
JoinedEthn <- JoinedEthn %>%
  mutate(EndoYes = case_when(
    egoRacel_dv - alterRacel_dv==0 ~ "1", 
    egoRacel_dv - alterRacel_dv!="1" ~ "0"
  ))

# Table 1.1
JoinedEthn %>%
  select(EndoYes) %>%
  table() %>%
  prop.table()
```

    ## .
    ##         0         1 
    ## 0.1020254 0.8979746

``` r
#### Ethnicity specific 

# Table 1.2: White Endo Prob
JoinedEthn %>%
  filter(egoRace=="White") %>%
  select(EndoYes) %>%
  table() %>%
  prop.table()
```

    ## .
    ##          0          1 
    ## 0.07494929 0.92505071

``` r
# Table 1.3: Non-White Endo Prob
JoinedEthn %>%
  filter(egoRace=="non-White") %>%
  select(EndoYes) %>%
  table() %>%
  prop.table()
```

    ## .
    ##         0         1 
    ## 0.2298851 0.7701149

*Explanation of Code* Table 1.1 shows that the probability of racial endogamy was ~90% for the entire dataset. However, tables 1.2 and 1.3 show that racial endogamy very much differs between "White" and "non-White" respondents. For "White" respondents, the probability of racial endogamy was ~92.5%; whereas the "non-White" cohort had a probability of ~77%.

Join with household data and calculate mean and median number of children by ethnic group (30 points)
-----------------------------------------------------------------------------------------------------

1.  Join the individual-level file with the household-level data from wave 8 (specifically, we want the variable for the number of children in the household).

``` r
Hh8 <- Hh8 %>%
  select(h_hidp, h_nkids_dv)

JoinedEthn <- JoinedEthn %>%
  inner_join(Hh8, by = "h_hidp")
```

1.  Select only couples that are ethnically endogamous (i.e. partners come from the same ethnic group) for the following groups: White British, Indian, and Pakistani.

``` r
# White British = 1, Indian = 9, Pakistani = 10

# Filtering relevant egos
JoinedEthn2 <- JoinedEthn %>%
  filter(egoRacel_dv==1 | egoRacel_dv==9 | egoRacel_dv==10)

# Filtering relevant alters
JoinedEthn2 <- JoinedEthn2 %>%
  filter(alterRacel_dv==1 | alterRacel_dv==9 | alterRacel_dv==10)

# Filtering endogamous couples
JoinedEthn2 <- JoinedEthn2 %>%
  filter(egoRacel_dv - alterRacel_dv == 0)

# Creating variable for endogamous relationship ethnicity
JoinedEthn2 <- JoinedEthn2 %>%   
  mutate(EndoCouple = case_when(
    egoRacel_dv==1 ~ "White British", 
    egoRacel_dv==9 ~ "Indian",
    egoRacel_dv==10 ~ "Pakistani"
  ))
```

1.  Produce a table showing the mean and median number of children in these households by ethnic group (make sure the table has meaningful labels for ethnic groups, not just numerical codes).

``` r
# Table showing mean number of children per household in a given ethnic group. 
with(JoinedEthn2, tapply(X = h_nkids_dv,
                         INDEX = EndoCouple, 
                         FUN = mean))
```

    ##        Indian     Pakistani White British 
    ##     0.9553753     1.8108747     0.5651314

``` r
# Table showing median number of children per household in a given ethnic group. 
with(JoinedEthn2, tapply(X = h_nkids_dv,
                         INDEX = EndoCouple, 
                         FUN = median))
```

    ##        Indian     Pakistani White British 
    ##             1             2             0

1.  

*Write a short interpretation of your results.*

The average number of children per endogamous household differs from ethnic group to ethnic group. White British (0.56 (2DP)), Indian (0.96 (2DP)) and Pakistani (1.81 (2DP)), per household. Furthermore, the median indicates that the majority of White British couples surveyed had no children.

*What could affect your findings?*

The age of couples could affect the findings. If, say, the White British participants were of a younger average age than their Indian and Pakistani counterparts, then the likelihood of them having children could be lower.

Furthermore, the number of observations in each group may affect the findings: the more observations (more couples) one has to survey, the more likely that the sample mean and median will reflect the population mean and median. The n for Indian and Pakistani endogamous couples may have been too small, and thus prone to skew.

Marriage may have also been an influential factor. Legally, marriage is a more secure context within which couples can have children. If marriage rates differed between the ethnicities, then this may partially explain the difference in average n children per household.
