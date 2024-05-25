# This is the final project for CUNY's DATA606 Course
## Part 1 - Introduction

Currently, the aviation industry is in a crisis. Several incidents, on
the ground and in the air have occured in the last few year. Whether
those are issues with air traffic control and tower–near misses between
planes on the ground, or technical issues in the air that led to an
increased number of emergency landings.

Especially in 2024, Boeing has gotten quite a bit of scrutiny, as many
aircrafts involved in the in-air incidents are Boeing-built. So much so
that the insitution responsible for air safety in the US started an
investigation, and top Boeing executives resign. With the aviation
industry being clearly a duopoly, Boeing and Airbus, several voices on
social media and news outlets have voiced to avoid Boeing-built planes.
This even has caused some flight prices to spike, such that airlines
that primarily fly airbus, such as Lufthansa, are now significantly more
expensive for transatlantic flights than airlines that fly more Boeing
planes, like KLM or Delta.

But is Boeing really involved in more incidents than Airbus is, or is
the media again subject to the availability bias, as it is the case so
often? Using a dataset described more below, this project aims to answer
this question. While airline crashes are still amazingly and extremely
rare, it is still an interesting question. Even though Boeing exists for
much longer than Airbus, I hypothesize that Boeing has been involved in
more crashes per year than Airbus has, since year 1971 (Airbus was
founded in 1970).

In order to answer this question I will be conducting a Chi-Square test
comparing the number of incidents between Boeing and Airbus.
Additionally, a two-samples t-test will be used to compare the average
crashes per year for each aircraft manufacturer. Below, several
indicators and summary statistics about the dataset are discussed.

## Part 2 - Data

The dataset was sourced from Kaggle, which was ultimately pulled by a
service called “Open Data” and is found here:
<https://www.kaggle.com/datasets/abeperez/historical-plane-crash-data>.

The dataset is significantly cleaned, as seen below, in order to filter
for only Boeing and Airbus planes, to exclude any military operations,
and to include only incidents after 01/01/1971 (as mentioned above,
Airbus was founded in 1970, including earlier dates would skew the
data). Additionally a new dataframe was computed in which the years were
grouped and then the occurences of Boeing and Airbus counted for the
time comparison. Both final data frames are shown below.

    df = read.csv("https://raw.githubusercontent.com/lucasweyrich958/Airplane_Manufacturer_Analysis/main/Plane%20Crashes.csv")
    library(tidyr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    df$Date = as.Date(df$Date)

    planes = df %>%
      filter(grepl("Airbus|Boeing", Aircraft)) %>%
      filter(Date > 1970-01-01)

    planes = planes %>%
      mutate(manufacturer = ifelse(grepl("Boeing", Aircraft), "Boeing",
                                   ifelse(grepl("Airbus", Aircraft), "Airbus", NA)))
      
    planes$year = format(as.Date(planes$Date), "%Y")

    final_data = planes %>%
      group_by(manufacturer) %>%
      summarise(count = n())

    final_data_year = planes %>%
      group_by(year, manufacturer) %>%
      summarise(count = n()) %>%
      pivot_wider(names_from = manufacturer, values_from = count, values_fill = 0)

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

    head(final_data)

    ## # A tibble: 2 × 2
    ##   manufacturer count
    ##   <chr>        <int>
    ## 1 Airbus          83
    ## 2 Boeing         538

    head(final_data_year)

    ## # A tibble: 6 × 3
    ## # Groups:   year [6]
    ##   year  Boeing Airbus
    ##   <chr>  <int>  <int>
    ## 1 1975       9      0
    ## 2 1976      14      0
    ## 3 1977      11      0
    ## 4 1978      11      0
    ## 5 1979      11      0
    ## 6 1980      13      0

## Part 3 - Exploratory data analysis

### Chi-Square Test

For the Chi-Square test, the assumptions require that the data are
absolute counts, not percentages or ratios, as well as the counts to be
mutually exclusive. Both of these assumptions are met. Below is a bar
plot to show the count of incidents by manufacturer overall.

    ggplot(final_data, aes(x = manufacturer, y = count)) +
      geom_bar(stat = "identity", fill = "#EFBC9B", color = "black", width = 0.5) +
      labs(title = "Incidents by Manufacturer", x = "Manufacturer", y = "Number of Incidents") +
      theme_minimal()

![](https://github.com/lucasweyrich958/Airplane_Manufacturer_Analysis/blob/main/figures/plot1.png)

### Comparison Over Time

To compare the number of incidients over time, the data was summarised
over years. This allows to visualize the data over time with a
time-series (see below), and also compare the data using a two-samples
t-test as a distribution can be computed, each year being an
observation. For this, the assumptions are that the data is normally
distributed, which is met for the Boeing counts, but not for the Airbus
counts (see histogram below). However, given that the counts of
incidents is a discrete variable, with 0 being an important number, it
is forgivable. Lastly, the boxplot suggests already that there may be a
greater average incident count per year with Boeing than with Airbus.

    summary(final_data_year)

    ##      year               Boeing          Airbus     
    ##  Length:48          Min.   : 2.00   Min.   :0.000  
    ##  Class :character   1st Qu.: 9.00   1st Qu.:0.000  
    ##  Mode  :character   Median :11.00   Median :1.000  
    ##                     Mean   :11.21   Mean   :1.729  
    ##                     3rd Qu.:14.00   3rd Qu.:2.250  
    ##                     Max.   :20.00   Max.   :9.000

    final_data_year$time = as.Date(paste0(final_data_year$year, "-01-01"))
    final_data_long <- final_data_year %>%
      pivot_longer(cols = c(Boeing, Airbus),
                   names_to = "Manufacturer",
                   values_to = "Count")

    ggplot(final_data_year, aes(x = time)) +
      geom_line(aes(y = Boeing, color = "Boeing")) +
      geom_line(aes(y = Airbus, color = "Airbus")) +
      labs(title = "Incidents by Manufacturer Over Time",
           x = "Year",
           y = "Number of Incidents",
           color = NULL) +
      scale_color_manual(values = c("Boeing" = "#D37676", "Airbus" = "#76885B"),
                         labels = c("Airbus", "Boeing")) +
      theme_minimal()

![](https://github.com/lucasweyrich958/Airplane_Manufacturer_Analysis/blob/main/figures/plot2.png)

    ggplot(final_data_year) +
      geom_histogram(aes(x = Airbus, fill = "Airbus"), alpha = 0.7, bins = 20, color = "black") +
      geom_histogram(aes(x = Boeing, fill = "Boeing"), alpha = 0.7, bins = 20, color = "black") +
      scale_fill_manual(values = c(Airbus = "#76885B", Boeing = "#D37676"), name = "Manufacturer") +
      labs(title = "Histogram of Airbus and Boeing",
           x = "Incidents",
           y = "Frequency") +
      theme_minimal() +
      theme(legend.position = "top")

![](https://github.com/lucasweyrich958/Airplane_Manufacturer_Analysis/blob/main/figures/plot3.png)

    ggplot(final_data_long, aes(x = Manufacturer, y = Count, fill = Manufacturer)) +
      geom_boxplot(width = 0.5) +
      labs(title = "Incidents by Manufacturer (year averages)",
           x = "Manufacturer",
           y = "Number of Incidents") +
      scale_fill_manual(values = c("Boeing" = "#D37676", "Airbus" = "#76885B")) +
      theme_minimal() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.title = element_blank(),
            legend.text = element_text(size = 12))

![](https://github.com/lucasweyrich958/Airplane_Manufacturer_Analysis/blob/main/figures/plot4.png)

    final_data_year$time = as.Date(paste0(final_data_year$year, "-01-01"))

## Part 4 - Inference

### Chi-Square Test

In order to perform the Chi-Square test of independence, the data has to
be pivoted to long format, so that there is a column for both
manufacturers. Following that, the test can be performed. In the tests
below, the alpha is 0.05

For the Chi-Square test, the null hypothesis (H0) is that there is no
significant association between the counts of incidents and aircraft
manufacturer, while the alternative hypothesis (H1) states that there
Boeing has more incidents than Airbus. In other words, the distribution
of incidents is not independent of the type of aircraft.

    final_data_pivot = final_data %>%
      pivot_wider(names_from = manufacturer, values_from = count)
    chisq.test(final_data_pivot)

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  final_data_pivot
    ## X-squared = 333.37, df = 1, p-value < 2.2e-16

The test shows a Chi-Squared value of 333.37 with a p-value below 0.05,
rejecting H0. This suggests that there is a significant association
between aircraft manufacturer and incidients frequency.

## Two-Samples t-Test

In order to consider this over time, a two-samples t-test is performed
on the data that is grouped by year. Here, the null hypothesis (H0)
suggests that there is no significant difference in the mean counts of
incidents, over the years from 1971 - 2022, between aircraft
manufacturer. The alternative hypothesis, however, states that there is
a significant difference.

    t.test(final_data_year$Boeing, final_data_year$Airbus, var.equal = T)

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  final_data_year$Boeing and final_data_year$Airbus
    ## t = 14.228, df = 94, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##   8.156323 10.802010
    ## sample estimates:
    ## mean of x mean of y 
    ## 11.208333  1.729167

The two-samples t-test shows a t-value of 14.23, a p-value below 0.05
and 95% CI bounds of 8.14 - 10.80. Since the p-value is below 0.05, and
the 95% bounds do not include the null hypothesis (in this case the
number 0), the null hypothesis may be rejected. This suggests that there
is in fact a statistically significant difference in the mean numbers of
incidents per year between Boeing and Airbus.

## Part 5 - Conclusion

In this project, I investigated whether there was in fact a significant
difference in the number of incidents between the aircraft manufacturer
Boeing and Airbus since 1971. To investigate this, two hypothesis tests
were utilized, the Chi-Square test of independence to compare absolute
counts, as well as the two-sample t-test to compare means grouped by
year. Both tests rejected the null hypothesis by exhibiting p-values
below 0.05, which suggests that there is in fact a statistically
significant difference between number of incidents between commercial
flights with Boeing and Airbus aircraft. Specifically, it appears that
Boeing has more incidents than Airbus, both absolut from 1971 - 2022,
and on average for each year. This furthermore suggests that the voices
heard from the media to rather Boeing planes are SOMEWHAT warranted.

However, the media is not fully vindicated, as there are several caveats
with this project. Firstly, relative occurrence of ANY incident
occurring during commercial flights is extremely low: 3.42 accidents per
ONE MILLION departures (ICAO, 2024). Therefore, even though it does
appear Boeing’s internal leadership and quality control issues are real,
it is by no means less safe to board a Boeing plane over an Airbus
plane. Additionally, as can be inferred from the time-series graph
above, incidents in the air for Boeing planes have decreased over time,
compared to the late 20th century. Lastly, the variable in this project
consisted of incident observations. Incidents are defined by the ICAO
(2019) as “An occurrence, associated with. the operation of an aircraft
which affects or could affect. the safety of operation.” Therefore, it
important to remember that an incident may also be something minor that
still allowed the plane to safely arrive at its planned destination.

Concluding, this project assessed aircraft incidents between Boeing and
Airbus. While the results suggest that there are more Boeing incidients
than Airbus incidents, the results have to be taken with many caveats.

## References

International Civil Aviation Organization. 2024. Accident Statistics.
<https://www.icao.int/safety/iStars/Pages/Accident-Statistics.aspx>

International Civil Aviation Organization. 2019. Requirements of Annex
13 and SMS.
<https://www.icao.int/NACC/Documents/Meetings/2019/SMSANSP/SMSxANSP-P07.pdf>

Kaggle. 2022. Historical Plane Crash Data.
<https://www.kaggle.com/datasets/abeperez/historical-plane-crash-data>
