library(tidyverse)
library(lubridate)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# import data

setwd("C:/Users/Cheeson Lau/OneDrive/Case Study online/Data")
jun2023 <- read.csv("202306-divvy-tripdata.csv")
may2023 <- read.csv("202305-divvy-tripdata.csv")
apr2023 <- read.csv("202304-divvy-tripdata.csv")
mar2023 <- read.csv("202303-divvy-tripdata.csv")
feb2023 <- read.csv("202302-divvy-tripdata.csv")
jan2023 <- read.csv("202301-divvy-tripdata.csv")
dec2022 <- read.csv("202212-divvy-tripdata.csv")
nov2022 <- read.csv("202211-divvy-tripdata.csv")
oct2022 <- read.csv("202210-divvy-tripdata.csv")
sep2022 <- read.csv("202209-divvy-tripdata.csv")
aug2022 <- read.csv("202208-divvy-tripdata.csv")
jul2022 <- read.csv("202207-divvy-tripdata.csv")

# Some column has blank data. When those columns are considered in the analysis,
# the rows with those blank cells will be filtered out.

# Calculate the mean of ride_length of each month, filter out invalid data.

jun2023_no_negative_rl <- filter(jun2023, ride_length >= 00:00:00)
may2023_no_negative_rl <- filter(may2023, ride_length >= 00:00:00)
apr2023_no_negative_rl <- filter(apr2023, ride_length >= 00:00:00)
mar2023_no_negative_rl <- filter(mar2023, ride_length >= 00:00:00)
feb2023_no_negative_rl <- filter(feb2023, ride_length >= 00:00:00)
jan2023_no_negative_rl <- filter(jan2023, ride_length >= 00:00:00)
dec2022_no_negative_rl <- filter(dec2022, ride_length >= 00:00:00)
nov2022_no_negative_rl <- filter(nov2022, ride_length >= 00:00:00)
oct2022_no_negative_rl <- filter(oct2022, ride_length >= 00:00:00)
sep2022_no_negative_rl <- filter(sep2022, ride_length >= 00:00:00)
aug2022_no_negative_rl <- filter(aug2022, ride_length >= 00:00:00)
jul2022_no_negative_rl <- filter(jul2022, ride_length >= 00:00:00)

jun2023_mean_rl <- mean(seconds(hms(jun2023_no_negative_rl$ride_length)))
may2023_mean_rl <- mean(seconds(hms(may2023_no_negative_rl$ride_length)))
apr2023_mean_rl <- mean(seconds(hms(apr2023_no_negative_rl$ride_length)))
mar2023_mean_rl <- mean(seconds(hms(mar2023_no_negative_rl$ride_length)))
feb2023_mean_rl <- mean(seconds(hms(feb2023_no_negative_rl$ride_length)))
jan2023_mean_rl <- mean(seconds(hms(jan2023_no_negative_rl$ride_length)))
dec2022_mean_rl <- mean(seconds(hms(dec2022_no_negative_rl$ride_length)))
nov2022_mean_rl <- mean(seconds(hms(nov2022_no_negative_rl$ride_length)))
oct2022_mean_rl <- mean(seconds(hms(oct2022_no_negative_rl$ride_length)))
sep2022_mean_rl <- mean(seconds(hms(sep2022_no_negative_rl$ride_length)))
aug2022_mean_rl <- mean(seconds(hms(aug2022_no_negative_rl$ride_length)))
jul2022_mean_rl <- mean(seconds(hms(jul2022_no_negative_rl$ride_length)))
month_mean_rl_table <- data.frame(month = c("jun2023", "may2023", "apr2023",
                                            "mar2023", "feb2023", "jan2023", 
                                            "dec2022", "nov2022", "oct2022",
                                            "sep2022", "aug2022", "jul2022"),
                                  ride_length_mean = c(jun2023_mean_rl, may2023_mean_rl, apr2023_mean_rl,
                                                       mar2023_mean_rl, feb2023_mean_rl, jan2023_mean_rl,
                                                       dec2022_mean_rl, nov2022_mean_rl, oct2022_mean_rl,
                                                       sep2022_mean_rl, aug2022_mean_rl, jul2022_mean_rl))

# Calculate the max ride_length of each month

jun2023_max_rl <- max(seconds(hms(jun2023_no_negative_rl$ride_length)))
may2023_max_rl <- max(seconds(hms(may2023_no_negative_rl$ride_length)))
apr2023_max_rl <- max(seconds(hms(apr2023_no_negative_rl$ride_length)))
mar2023_max_rl <- max(seconds(hms(mar2023_no_negative_rl$ride_length)))
feb2023_max_rl <- max(seconds(hms(feb2023_no_negative_rl$ride_length)))
jan2023_max_rl <- max(seconds(hms(jan2023_no_negative_rl$ride_length)))
dec2022_max_rl <- max(seconds(hms(dec2022_no_negative_rl$ride_length)))
nov2022_max_rl <- max(seconds(hms(nov2022_no_negative_rl$ride_length)))
oct2022_max_rl <- max(seconds(hms(oct2022_no_negative_rl$ride_length)))
sep2022_max_rl <- max(seconds(hms(sep2022_no_negative_rl$ride_length)))
aug2022_max_rl <- max(seconds(hms(aug2022_no_negative_rl$ride_length)))
jul2022_max_rl <- max(seconds(hms(jul2022_no_negative_rl$ride_length)))
month_max_rl_table <- data.frame(month = c("jun2023", "may2023", "apr2023",
                                            "mar2023", "feb2023", "jan2023", 
                                            "dec2022", "nov2022", "oct2022",
                                            "sep2022", "aug2022", "jul2022"),
                                 ride_length_max = c(jun2023_max_rl, may2023_max_rl, apr2023_max_rl,
                                                     mar2023_max_rl, feb2023_max_rl, jan2023_max_rl,
                                                     dec2022_max_rl, nov2022_max_rl, oct2022_max_rl,
                                                     sep2022_max_rl, aug2022_max_rl, jul2022_max_rl))

# Calculate the mode of day_of_week of each month, no invalid data in any month

jun2023_mode_dow <- Mode(jun2023$day_of_week)
may2023_mode_dow <- Mode(may2023$day_of_week)
apr2023_mode_dow <- Mode(apr2023$day_of_week)
mar2023_mode_dow <- Mode(mar2023$day_of_week)
feb2023_mode_dow <- Mode(feb2023$day_of_week)
jan2023_mode_dow <- Mode(jan2023$day_of_week)
dec2022_mode_dow <- Mode(dec2022$day_of_week)
nov2022_mode_dow <- Mode(nov2022$day_of_week)
oct2022_mode_dow <- Mode(oct2022$day_of_week)
sep2022_mode_dow <- Mode(sep2022$day_of_week)
aug2022_mode_dow <- Mode(aug2022$day_of_week)
jul2022_mode_dow <- Mode(jul2022$day_of_week)

month_mode_dow_table <- data.frame(month = c("jun2023", "may2023", "apr2023",
                                             "mar2023", "feb2023", "jan2023", 
                                             "dec2022", "nov2022", "oct2022",
                                             "sep2022", "aug2022", "jul2022"),
                                   ride_length_max = c(jun2023_mode_dow, may2023_mode_dow, apr2023_mode_dow,
                                                       mar2023_mode_dow, feb2023_mode_dow, jan2023_mode_dow,
                                                       dec2022_mode_dow, nov2022_mode_dow, oct2022_mode_dow,
                                                       sep2022_mode_dow, aug2022_mode_dow, jul2022_mode_dow))

# Compare members and casual riders.

## Filter member only data frame

jun2023_member <- filter(jun2023_no_negative_rl, member_casual == "member")
may2023_member <- filter(may2023_no_negative_rl, member_casual == "member")
apr2023_member <- filter(apr2023_no_negative_rl, member_casual == "member")
mar2023_member <- filter(mar2023_no_negative_rl, member_casual == "member")
feb2023_member <- filter(feb2023_no_negative_rl, member_casual == "member")
jan2023_member <- filter(jan2023_no_negative_rl, member_casual == "member")
dec2022_member <- filter(dec2022_no_negative_rl, member_casual == "member")
nov2022_member <- filter(nov2022_no_negative_rl, member_casual == "member")
oct2022_member <- filter(oct2022_no_negative_rl, member_casual == "member")
sep2022_member <- filter(sep2022_no_negative_rl, member_casual == "member")
aug2022_member <- filter(aug2022_no_negative_rl, member_casual == "member")
jul2022_member <- filter(jul2022_no_negative_rl, member_casual == "member")

jun2023_casual <- filter(jun2023_no_negative_rl, member_casual == "casual")
may2023_casual <- filter(may2023_no_negative_rl, member_casual == "casual")
apr2023_casual <- filter(apr2023_no_negative_rl, member_casual == "casual")
mar2023_casual <- filter(mar2023_no_negative_rl, member_casual == "casual")
feb2023_casual <- filter(feb2023_no_negative_rl, member_casual == "casual")
jan2023_casual <- filter(jan2023_no_negative_rl, member_casual == "casual")
dec2022_casual <- filter(dec2022_no_negative_rl, member_casual == "casual")
nov2022_casual <- filter(nov2022_no_negative_rl, member_casual == "casual")
oct2022_casual <- filter(oct2022_no_negative_rl, member_casual == "casual")
sep2022_casual <- filter(sep2022_no_negative_rl, member_casual == "casual")
aug2022_casual <- filter(aug2022_no_negative_rl, member_casual == "casual")
jul2022_casual <- filter(jul2022_no_negative_rl, member_casual == "casual")

## Average ride_length for members and casual riders each month

jun2023_mean_rl_member <- mean(seconds(hms(jun2023_member$ride_length)))
may2023_mean_rl_member <- mean(seconds(hms(may2023_member$ride_length)))
apr2023_mean_rl_member <- mean(seconds(hms(apr2023_member$ride_length)))
mar2023_mean_rl_member <- mean(seconds(hms(mar2023_member$ride_length)))
feb2023_mean_rl_member <- mean(seconds(hms(feb2023_member$ride_length)))
jan2023_mean_rl_member <- mean(seconds(hms(jan2023_member$ride_length)))
dec2022_mean_rl_member <- mean(seconds(hms(dec2022_member$ride_length)))
nov2022_mean_rl_member <- mean(seconds(hms(nov2022_member$ride_length)))
oct2022_mean_rl_member <- mean(seconds(hms(oct2022_member$ride_length)))
sep2022_mean_rl_member <- mean(seconds(hms(sep2022_member$ride_length)))
aug2022_mean_rl_member <- mean(seconds(hms(aug2022_member$ride_length)))
jul2022_mean_rl_member <- mean(seconds(hms(jul2022_member$ride_length)))

jun2023_mean_rl_casual <- mean(seconds(hms(jun2023_casual$ride_length)))
may2023_mean_rl_casual <- mean(seconds(hms(may2023_casual$ride_length)))
apr2023_mean_rl_casual <- mean(seconds(hms(apr2023_casual$ride_length)))
mar2023_mean_rl_casual <- mean(seconds(hms(mar2023_casual$ride_length)))
feb2023_mean_rl_casual <- mean(seconds(hms(feb2023_casual$ride_length)))
jan2023_mean_rl_casual <- mean(seconds(hms(jan2023_casual$ride_length)))
dec2022_mean_rl_casual <- mean(seconds(hms(dec2022_casual$ride_length)))
nov2022_mean_rl_casual <- mean(seconds(hms(nov2022_casual$ride_length)))
oct2022_mean_rl_casual <- mean(seconds(hms(oct2022_casual$ride_length)))
sep2022_mean_rl_casual <- mean(seconds(hms(sep2022_casual$ride_length)))
aug2022_mean_rl_casual <- mean(seconds(hms(aug2022_casual$ride_length)))
jul2022_mean_rl_casual <- mean(seconds(hms(jul2022_casual$ride_length)))

month_mean_rl_member_casual_table <- data.frame(month = c("jun2023", "may2023", "apr2023",
                                            "mar2023", "feb2023", "jan2023", 
                                            "dec2022", "nov2022", "oct2022",
                                            "sep2022", "aug2022", "jul2022"),
                                  member_ride_length_mean = c(jun2023_mean_rl_member, may2023_mean_rl_member, apr2023_mean_rl_member,
                                                              mar2023_mean_rl_member, feb2023_mean_rl_member, jan2023_mean_rl_member,
                                                              dec2022_mean_rl_member, nov2022_mean_rl_member, oct2022_mean_rl_member,
                                                              sep2022_mean_rl_member, aug2022_mean_rl_member, jul2022_mean_rl_member),
                                  casual_ride_length_mean = c(jun2023_mean_rl_casual, may2023_mean_rl_casual, apr2023_mean_rl_casual,
                                                              mar2023_mean_rl_casual, feb2023_mean_rl_casual, jan2023_mean_rl_casual,
                                                              dec2022_mean_rl_casual, nov2022_mean_rl_casual, oct2022_mean_rl_casual,
                                                              sep2022_mean_rl_casual, aug2022_mean_rl_casual, jul2022_mean_rl_casual))

## Average ride_length for members and casual riders each month, also considering the day

### Filter phase

### June, Member

jun2023_member_1 <- filter(jun2023_member, day_of_week == 1)
jun2023_member_2 <- filter(jun2023_member, day_of_week == 2)
jun2023_member_3 <- filter(jun2023_member, day_of_week == 3)
jun2023_member_4 <- filter(jun2023_member, day_of_week == 4)
jun2023_member_5 <- filter(jun2023_member, day_of_week == 5)
jun2023_member_6 <- filter(jun2023_member, day_of_week == 6)
jun2023_member_7 <- filter(jun2023_member, day_of_week == 7)

### May, Member

may2023_member_1 <- filter(may2023_member, day_of_week == 1)
may2023_member_2 <- filter(may2023_member, day_of_week == 2)
may2023_member_3 <- filter(may2023_member, day_of_week == 3)
may2023_member_4 <- filter(may2023_member, day_of_week == 4)
may2023_member_5 <- filter(may2023_member, day_of_week == 5)
may2023_member_6 <- filter(may2023_member, day_of_week == 6)
may2023_member_7 <- filter(may2023_member, day_of_week == 7)

### April, Member

apr2023_member_1 <- filter(apr2023_member, day_of_week == 1)
apr2023_member_2 <- filter(apr2023_member, day_of_week == 2)
apr2023_member_3 <- filter(apr2023_member, day_of_week == 3)
apr2023_member_4 <- filter(apr2023_member, day_of_week == 4)
apr2023_member_5 <- filter(apr2023_member, day_of_week == 5)
apr2023_member_6 <- filter(apr2023_member, day_of_week == 6)
apr2023_member_7 <- filter(apr2023_member, day_of_week == 7)

### March, Member

mar2023_member_1 <- filter(mar2023_member, day_of_week == 1)
mar2023_member_2 <- filter(mar2023_member, day_of_week == 2)
mar2023_member_3 <- filter(mar2023_member, day_of_week == 3)
mar2023_member_4 <- filter(mar2023_member, day_of_week == 4)
mar2023_member_5 <- filter(mar2023_member, day_of_week == 5)
mar2023_member_6 <- filter(mar2023_member, day_of_week == 6)
mar2023_member_7 <- filter(mar2023_member, day_of_week == 7)

### February, Member

feb2023_member_1 <- filter(feb2023_member, day_of_week == 1)
feb2023_member_2 <- filter(feb2023_member, day_of_week == 2)
feb2023_member_3 <- filter(feb2023_member, day_of_week == 3)
feb2023_member_4 <- filter(feb2023_member, day_of_week == 4)
feb2023_member_5 <- filter(feb2023_member, day_of_week == 5)
feb2023_member_6 <- filter(feb2023_member, day_of_week == 6)
feb2023_member_7 <- filter(feb2023_member, day_of_week == 7)

### January, Member

jan2023_member_1 <- filter(jan2023_member, day_of_week == 1)
jan2023_member_2 <- filter(jan2023_member, day_of_week == 2)
jan2023_member_3 <- filter(jan2023_member, day_of_week == 3)
jan2023_member_4 <- filter(jan2023_member, day_of_week == 4)
jan2023_member_5 <- filter(jan2023_member, day_of_week == 5)
jan2023_member_6 <- filter(jan2023_member, day_of_week == 6)
jan2023_member_7 <- filter(jan2023_member, day_of_week == 7)

### December, Member

dec2022_member_1 <- filter(dec2022_member, day_of_week == 1)
dec2022_member_2 <- filter(dec2022_member, day_of_week == 2)
dec2022_member_3 <- filter(dec2022_member, day_of_week == 3)
dec2022_member_4 <- filter(dec2022_member, day_of_week == 4)
dec2022_member_5 <- filter(dec2022_member, day_of_week == 5)
dec2022_member_6 <- filter(dec2022_member, day_of_week == 6)
dec2022_member_7 <- filter(dec2022_member, day_of_week == 7)

### November, Member

nov2022_member_1 <- filter(nov2022_member, day_of_week == 1)
nov2022_member_2 <- filter(nov2022_member, day_of_week == 2)
nov2022_member_3 <- filter(nov2022_member, day_of_week == 3)
nov2022_member_4 <- filter(nov2022_member, day_of_week == 4)
nov2022_member_5 <- filter(nov2022_member, day_of_week == 5)
nov2022_member_6 <- filter(nov2022_member, day_of_week == 6)
nov2022_member_7 <- filter(nov2022_member, day_of_week == 7)

### October, Member

oct2022_member_1 <- filter(oct2022_member, day_of_week == 1)
oct2022_member_2 <- filter(oct2022_member, day_of_week == 2)
oct2022_member_3 <- filter(oct2022_member, day_of_week == 3)
oct2022_member_4 <- filter(oct2022_member, day_of_week == 4)
oct2022_member_5 <- filter(oct2022_member, day_of_week == 5)
oct2022_member_6 <- filter(oct2022_member, day_of_week == 6)
oct2022_member_7 <- filter(oct2022_member, day_of_week == 7)

### September, Member

sep2022_member_1 <- filter(sep2022_member, day_of_week == 1)
sep2022_member_2 <- filter(sep2022_member, day_of_week == 2)
sep2022_member_3 <- filter(sep2022_member, day_of_week == 3)
sep2022_member_4 <- filter(sep2022_member, day_of_week == 4)
sep2022_member_5 <- filter(sep2022_member, day_of_week == 5)
sep2022_member_6 <- filter(sep2022_member, day_of_week == 6)
sep2022_member_7 <- filter(sep2022_member, day_of_week == 7)

### August, Member

aug2022_member_1 <- filter(aug2022_member, day_of_week == 1)
aug2022_member_2 <- filter(aug2022_member, day_of_week == 2)
aug2022_member_3 <- filter(aug2022_member, day_of_week == 3)
aug2022_member_4 <- filter(aug2022_member, day_of_week == 4)
aug2022_member_5 <- filter(aug2022_member, day_of_week == 5)
aug2022_member_6 <- filter(aug2022_member, day_of_week == 6)
aug2022_member_7 <- filter(aug2022_member, day_of_week == 7)

### July, Member

jul2022_member_1 <- filter(jul2022_member, day_of_week == 1)
jul2022_member_2 <- filter(jul2022_member, day_of_week == 2)
jul2022_member_3 <- filter(jul2022_member, day_of_week == 3)
jul2022_member_4 <- filter(jul2022_member, day_of_week == 4)
jul2022_member_5 <- filter(jul2022_member, day_of_week == 5)
jul2022_member_6 <- filter(jul2022_member, day_of_week == 6)
jul2022_member_7 <- filter(jul2022_member, day_of_week == 7)

### June, Casual

jun2023_casual_1 <- filter(jun2023_casual, day_of_week == 1)
jun2023_casual_2 <- filter(jun2023_casual, day_of_week == 2)
jun2023_casual_3 <- filter(jun2023_casual, day_of_week == 3)
jun2023_casual_4 <- filter(jun2023_casual, day_of_week == 4)
jun2023_casual_5 <- filter(jun2023_casual, day_of_week == 5)
jun2023_casual_6 <- filter(jun2023_casual, day_of_week == 6)
jun2023_casual_7 <- filter(jun2023_casual, day_of_week == 7)

### May, Casual

may2023_casual_1 <- filter(may2023_casual, day_of_week == 1)
may2023_casual_2 <- filter(may2023_casual, day_of_week == 2)
may2023_casual_3 <- filter(may2023_casual, day_of_week == 3)
may2023_casual_4 <- filter(may2023_casual, day_of_week == 4)
may2023_casual_5 <- filter(may2023_casual, day_of_week == 5)
may2023_casual_6 <- filter(may2023_casual, day_of_week == 6)
may2023_casual_7 <- filter(may2023_casual, day_of_week == 7)

### April, Casual

apr2023_casual_1 <- filter(apr2023_casual, day_of_week == 1)
apr2023_casual_2 <- filter(apr2023_casual, day_of_week == 2)
apr2023_casual_3 <- filter(apr2023_casual, day_of_week == 3)
apr2023_casual_4 <- filter(apr2023_casual, day_of_week == 4)
apr2023_casual_5 <- filter(apr2023_casual, day_of_week == 5)
apr2023_casual_6 <- filter(apr2023_casual, day_of_week == 6)
apr2023_casual_7 <- filter(apr2023_casual, day_of_week == 7)

### March, Casual

mar2023_casual_1 <- filter(mar2023_casual, day_of_week == 1)
mar2023_casual_2 <- filter(mar2023_casual, day_of_week == 2)
mar2023_casual_3 <- filter(mar2023_casual, day_of_week == 3)
mar2023_casual_4 <- filter(mar2023_casual, day_of_week == 4)
mar2023_casual_5 <- filter(mar2023_casual, day_of_week == 5)
mar2023_casual_6 <- filter(mar2023_casual, day_of_week == 6)
mar2023_casual_7 <- filter(mar2023_casual, day_of_week == 7)

### February, Casual

feb2023_casual_1 <- filter(feb2023_casual, day_of_week == 1)
feb2023_casual_2 <- filter(feb2023_casual, day_of_week == 2)
feb2023_casual_3 <- filter(feb2023_casual, day_of_week == 3)
feb2023_casual_4 <- filter(feb2023_casual, day_of_week == 4)
feb2023_casual_5 <- filter(feb2023_casual, day_of_week == 5)
feb2023_casual_6 <- filter(feb2023_casual, day_of_week == 6)
feb2023_casual_7 <- filter(feb2023_casual, day_of_week == 7)

### January, Casual

jan2023_casual_1 <- filter(jan2023_casual, day_of_week == 1)
jan2023_casual_2 <- filter(jan2023_casual, day_of_week == 2)
jan2023_casual_3 <- filter(jan2023_casual, day_of_week == 3)
jan2023_casual_4 <- filter(jan2023_casual, day_of_week == 4)
jan2023_casual_5 <- filter(jan2023_casual, day_of_week == 5)
jan2023_casual_6 <- filter(jan2023_casual, day_of_week == 6)
jan2023_casual_7 <- filter(jan2023_casual, day_of_week == 7)

### December, Casual

dec2022_casual_1 <- filter(dec2022_casual, day_of_week == 1)
dec2022_casual_2 <- filter(dec2022_casual, day_of_week == 2)
dec2022_casual_3 <- filter(dec2022_casual, day_of_week == 3)
dec2022_casual_4 <- filter(dec2022_casual, day_of_week == 4)
dec2022_casual_5 <- filter(dec2022_casual, day_of_week == 5)
dec2022_casual_6 <- filter(dec2022_casual, day_of_week == 6)
dec2022_casual_7 <- filter(dec2022_casual, day_of_week == 7)

### November, Casual

nov2022_casual_1 <- filter(nov2022_casual, day_of_week == 1)
nov2022_casual_2 <- filter(nov2022_casual, day_of_week == 2)
nov2022_casual_3 <- filter(nov2022_casual, day_of_week == 3)
nov2022_casual_4 <- filter(nov2022_casual, day_of_week == 4)
nov2022_casual_5 <- filter(nov2022_casual, day_of_week == 5)
nov2022_casual_6 <- filter(nov2022_casual, day_of_week == 6)
nov2022_casual_7 <- filter(nov2022_casual, day_of_week == 7)

### October, Casual

oct2022_casual_1 <- filter(oct2022_casual, day_of_week == 1)
oct2022_casual_2 <- filter(oct2022_casual, day_of_week == 2)
oct2022_casual_3 <- filter(oct2022_casual, day_of_week == 3)
oct2022_casual_4 <- filter(oct2022_casual, day_of_week == 4)
oct2022_casual_5 <- filter(oct2022_casual, day_of_week == 5)
oct2022_casual_6 <- filter(oct2022_casual, day_of_week == 6)
oct2022_casual_7 <- filter(oct2022_casual, day_of_week == 7)

### September, Casual

sep2022_casual_1 <- filter(sep2022_casual, day_of_week == 1)
sep2022_casual_2 <- filter(sep2022_casual, day_of_week == 2)
sep2022_casual_3 <- filter(sep2022_casual, day_of_week == 3)
sep2022_casual_4 <- filter(sep2022_casual, day_of_week == 4)
sep2022_casual_5 <- filter(sep2022_casual, day_of_week == 5)
sep2022_casual_6 <- filter(sep2022_casual, day_of_week == 6)
sep2022_casual_7 <- filter(sep2022_casual, day_of_week == 7)

### August, Casual

aug2022_casual_1 <- filter(aug2022_casual, day_of_week == 1)
aug2022_casual_2 <- filter(aug2022_casual, day_of_week == 2)
aug2022_casual_3 <- filter(aug2022_casual, day_of_week == 3)
aug2022_casual_4 <- filter(aug2022_casual, day_of_week == 4)
aug2022_casual_5 <- filter(aug2022_casual, day_of_week == 5)
aug2022_casual_6 <- filter(aug2022_casual, day_of_week == 6)
aug2022_casual_7 <- filter(aug2022_casual, day_of_week == 7)

### July, Casual

jul2022_casual_1 <- filter(jul2022_casual, day_of_week == 1)
jul2022_casual_2 <- filter(jul2022_casual, day_of_week == 2)
jul2022_casual_3 <- filter(jul2022_casual, day_of_week == 3)
jul2022_casual_4 <- filter(jul2022_casual, day_of_week == 4)
jul2022_casual_5 <- filter(jul2022_casual, day_of_week == 5)
jul2022_casual_6 <- filter(jul2022_casual, day_of_week == 6)
jul2022_casual_7 <- filter(jul2022_casual, day_of_week == 7)

### Calculate mean phase

### June, Member

jun2023_mean_rl_member_1 <- mean(seconds(hms(jun2023_member_1$ride_length)))
jun2023_mean_rl_member_2 <- mean(seconds(hms(jun2023_member_2$ride_length)))
jun2023_mean_rl_member_3 <- mean(seconds(hms(jun2023_member_3$ride_length)))
jun2023_mean_rl_member_4 <- mean(seconds(hms(jun2023_member_4$ride_length)))
jun2023_mean_rl_member_5 <- mean(seconds(hms(jun2023_member_5$ride_length)))
jun2023_mean_rl_member_6 <- mean(seconds(hms(jun2023_member_6$ride_length)))
jun2023_mean_rl_member_7 <- mean(seconds(hms(jun2023_member_7$ride_length)))

### May, Member

may2023_mean_rl_member_1 <- mean(seconds(hms(may2023_member_1$ride_length)))
may2023_mean_rl_member_2 <- mean(seconds(hms(may2023_member_2$ride_length)))
may2023_mean_rl_member_3 <- mean(seconds(hms(may2023_member_3$ride_length)))
may2023_mean_rl_member_4 <- mean(seconds(hms(may2023_member_4$ride_length)))
may2023_mean_rl_member_5 <- mean(seconds(hms(may2023_member_5$ride_length)))
may2023_mean_rl_member_6 <- mean(seconds(hms(may2023_member_6$ride_length)))
may2023_mean_rl_member_7 <- mean(seconds(hms(may2023_member_7$ride_length)))

### April, Member

apr2023_mean_rl_member_1 <- mean(seconds(hms(apr2023_member_1$ride_length)))
apr2023_mean_rl_member_2 <- mean(seconds(hms(apr2023_member_2$ride_length)))
apr2023_mean_rl_member_3 <- mean(seconds(hms(apr2023_member_3$ride_length)))
apr2023_mean_rl_member_4 <- mean(seconds(hms(apr2023_member_4$ride_length)))
apr2023_mean_rl_member_5 <- mean(seconds(hms(apr2023_member_5$ride_length)))
apr2023_mean_rl_member_6 <- mean(seconds(hms(apr2023_member_6$ride_length)))
apr2023_mean_rl_member_7 <- mean(seconds(hms(apr2023_member_7$ride_length)))

### March, Member

mar2023_mean_rl_member_1 <- mean(seconds(hms(mar2023_member_1$ride_length)))
mar2023_mean_rl_member_2 <- mean(seconds(hms(mar2023_member_2$ride_length)))
mar2023_mean_rl_member_3 <- mean(seconds(hms(mar2023_member_3$ride_length)))
mar2023_mean_rl_member_4 <- mean(seconds(hms(mar2023_member_4$ride_length)))
mar2023_mean_rl_member_5 <- mean(seconds(hms(mar2023_member_5$ride_length)))
mar2023_mean_rl_member_6 <- mean(seconds(hms(mar2023_member_6$ride_length)))
mar2023_mean_rl_member_7 <- mean(seconds(hms(mar2023_member_7$ride_length)))

### February, Member

feb2023_mean_rl_member_1 <- mean(seconds(hms(feb2023_member_1$ride_length)))
feb2023_mean_rl_member_2 <- mean(seconds(hms(feb2023_member_2$ride_length)))
feb2023_mean_rl_member_3 <- mean(seconds(hms(feb2023_member_3$ride_length)))
feb2023_mean_rl_member_4 <- mean(seconds(hms(feb2023_member_4$ride_length)))
feb2023_mean_rl_member_5 <- mean(seconds(hms(feb2023_member_5$ride_length)))
feb2023_mean_rl_member_6 <- mean(seconds(hms(feb2023_member_6$ride_length)))
feb2023_mean_rl_member_7 <- mean(seconds(hms(feb2023_member_7$ride_length)))

### January, Member

jan2023_mean_rl_member_1 <- mean(seconds(hms(jan2023_member_1$ride_length)))
jan2023_mean_rl_member_2 <- mean(seconds(hms(jan2023_member_2$ride_length)))
jan2023_mean_rl_member_3 <- mean(seconds(hms(jan2023_member_3$ride_length)))
jan2023_mean_rl_member_4 <- mean(seconds(hms(jan2023_member_4$ride_length)))
jan2023_mean_rl_member_5 <- mean(seconds(hms(jan2023_member_5$ride_length)))
jan2023_mean_rl_member_6 <- mean(seconds(hms(jan2023_member_6$ride_length)))
jan2023_mean_rl_member_7 <- mean(seconds(hms(jan2023_member_7$ride_length)))

### December, Member

dec2022_mean_rl_member_1 <- mean(seconds(hms(dec2022_member_1$ride_length)))
dec2022_mean_rl_member_2 <- mean(seconds(hms(dec2022_member_2$ride_length)))
dec2022_mean_rl_member_3 <- mean(seconds(hms(dec2022_member_3$ride_length)))
dec2022_mean_rl_member_4 <- mean(seconds(hms(dec2022_member_4$ride_length)))
dec2022_mean_rl_member_5 <- mean(seconds(hms(dec2022_member_5$ride_length)))
dec2022_mean_rl_member_6 <- mean(seconds(hms(dec2022_member_6$ride_length)))
dec2022_mean_rl_member_7 <- mean(seconds(hms(dec2022_member_7$ride_length)))

### November, Member

nov2022_mean_rl_member_1 <- mean(seconds(hms(nov2022_member_1$ride_length)))
nov2022_mean_rl_member_2 <- mean(seconds(hms(nov2022_member_2$ride_length)))
nov2022_mean_rl_member_3 <- mean(seconds(hms(nov2022_member_3$ride_length)))
nov2022_mean_rl_member_4 <- mean(seconds(hms(nov2022_member_4$ride_length)))
nov2022_mean_rl_member_5 <- mean(seconds(hms(nov2022_member_5$ride_length)))
nov2022_mean_rl_member_6 <- mean(seconds(hms(nov2022_member_6$ride_length)))
nov2022_mean_rl_member_7 <- mean(seconds(hms(nov2022_member_7$ride_length)))

### October, Member

oct2022_mean_rl_member_1 <- mean(seconds(hms(oct2022_member_1$ride_length)))
oct2022_mean_rl_member_2 <- mean(seconds(hms(oct2022_member_2$ride_length)))
oct2022_mean_rl_member_3 <- mean(seconds(hms(oct2022_member_3$ride_length)))
oct2022_mean_rl_member_4 <- mean(seconds(hms(oct2022_member_4$ride_length)))
oct2022_mean_rl_member_5 <- mean(seconds(hms(oct2022_member_5$ride_length)))
oct2022_mean_rl_member_6 <- mean(seconds(hms(oct2022_member_6$ride_length)))
oct2022_mean_rl_member_7 <- mean(seconds(hms(oct2022_member_7$ride_length)))

### September, Member

sep2022_mean_rl_member_1 <- mean(seconds(hms(sep2022_member_1$ride_length)))
sep2022_mean_rl_member_2 <- mean(seconds(hms(sep2022_member_2$ride_length)))
sep2022_mean_rl_member_3 <- mean(seconds(hms(sep2022_member_3$ride_length)))
sep2022_mean_rl_member_4 <- mean(seconds(hms(sep2022_member_4$ride_length)))
sep2022_mean_rl_member_5 <- mean(seconds(hms(sep2022_member_5$ride_length)))
sep2022_mean_rl_member_6 <- mean(seconds(hms(sep2022_member_6$ride_length)))
sep2022_mean_rl_member_7 <- mean(seconds(hms(sep2022_member_7$ride_length)))

### August, Member

aug2022_mean_rl_member_1 <- mean(seconds(hms(aug2022_member_1$ride_length)))
aug2022_mean_rl_member_2 <- mean(seconds(hms(aug2022_member_2$ride_length)))
aug2022_mean_rl_member_3 <- mean(seconds(hms(aug2022_member_3$ride_length)))
aug2022_mean_rl_member_4 <- mean(seconds(hms(aug2022_member_4$ride_length)))
aug2022_mean_rl_member_5 <- mean(seconds(hms(aug2022_member_5$ride_length)))
aug2022_mean_rl_member_6 <- mean(seconds(hms(aug2022_member_6$ride_length)))
aug2022_mean_rl_member_7 <- mean(seconds(hms(aug2022_member_7$ride_length)))

### July, Member

jul2022_mean_rl_member_1 <- mean(seconds(hms(jul2022_member_1$ride_length)))
jul2022_mean_rl_member_2 <- mean(seconds(hms(jul2022_member_2$ride_length)))
jul2022_mean_rl_member_3 <- mean(seconds(hms(jul2022_member_3$ride_length)))
jul2022_mean_rl_member_4 <- mean(seconds(hms(jul2022_member_4$ride_length)))
jul2022_mean_rl_member_5 <- mean(seconds(hms(jul2022_member_5$ride_length)))
jul2022_mean_rl_member_6 <- mean(seconds(hms(jul2022_member_6$ride_length)))
jul2022_mean_rl_member_7 <- mean(seconds(hms(jul2022_member_7$ride_length)))

### June, Casual

jun2023_mean_rl_casual_1 <- mean(seconds(hms(jun2023_casual_1$ride_length)))
jun2023_mean_rl_casual_2 <- mean(seconds(hms(jun2023_casual_2$ride_length)))
jun2023_mean_rl_casual_3 <- mean(seconds(hms(jun2023_casual_3$ride_length)))
jun2023_mean_rl_casual_4 <- mean(seconds(hms(jun2023_casual_4$ride_length)))
jun2023_mean_rl_casual_5 <- mean(seconds(hms(jun2023_casual_5$ride_length)))
jun2023_mean_rl_casual_6 <- mean(seconds(hms(jun2023_casual_6$ride_length)))
jun2023_mean_rl_casual_7 <- mean(seconds(hms(jun2023_casual_7$ride_length)))

### May, Casual

may2023_mean_rl_casual_1 <- mean(seconds(hms(may2023_casual_1$ride_length)))
may2023_mean_rl_casual_2 <- mean(seconds(hms(may2023_casual_2$ride_length)))
may2023_mean_rl_casual_3 <- mean(seconds(hms(may2023_casual_3$ride_length)))
may2023_mean_rl_casual_4 <- mean(seconds(hms(may2023_casual_4$ride_length)))
may2023_mean_rl_casual_5 <- mean(seconds(hms(may2023_casual_5$ride_length)))
may2023_mean_rl_casual_6 <- mean(seconds(hms(may2023_casual_6$ride_length)))
may2023_mean_rl_casual_7 <- mean(seconds(hms(may2023_casual_7$ride_length)))

### April, Casual

apr2023_mean_rl_casual_1 <- mean(seconds(hms(apr2023_casual_1$ride_length)))
apr2023_mean_rl_casual_2 <- mean(seconds(hms(apr2023_casual_2$ride_length)))
apr2023_mean_rl_casual_3 <- mean(seconds(hms(apr2023_casual_3$ride_length)))
apr2023_mean_rl_casual_4 <- mean(seconds(hms(apr2023_casual_4$ride_length)))
apr2023_mean_rl_casual_5 <- mean(seconds(hms(apr2023_casual_5$ride_length)))
apr2023_mean_rl_casual_6 <- mean(seconds(hms(apr2023_casual_6$ride_length)))
apr2023_mean_rl_casual_7 <- mean(seconds(hms(apr2023_casual_7$ride_length)))

### March, Casual

mar2023_mean_rl_casual_1 <- mean(seconds(hms(mar2023_casual_1$ride_length)))
mar2023_mean_rl_casual_2 <- mean(seconds(hms(mar2023_casual_2$ride_length)))
mar2023_mean_rl_casual_3 <- mean(seconds(hms(mar2023_casual_3$ride_length)))
mar2023_mean_rl_casual_4 <- mean(seconds(hms(mar2023_casual_4$ride_length)))
mar2023_mean_rl_casual_5 <- mean(seconds(hms(mar2023_casual_5$ride_length)))
mar2023_mean_rl_casual_6 <- mean(seconds(hms(mar2023_casual_6$ride_length)))
mar2023_mean_rl_casual_7 <- mean(seconds(hms(mar2023_casual_7$ride_length)))

### February, Casual

feb2023_mean_rl_casual_1 <- mean(seconds(hms(feb2023_casual_1$ride_length)))
feb2023_mean_rl_casual_2 <- mean(seconds(hms(feb2023_casual_2$ride_length)))
feb2023_mean_rl_casual_3 <- mean(seconds(hms(feb2023_casual_3$ride_length)))
feb2023_mean_rl_casual_4 <- mean(seconds(hms(feb2023_casual_4$ride_length)))
feb2023_mean_rl_casual_5 <- mean(seconds(hms(feb2023_casual_5$ride_length)))
feb2023_mean_rl_casual_6 <- mean(seconds(hms(feb2023_casual_6$ride_length)))
feb2023_mean_rl_casual_7 <- mean(seconds(hms(feb2023_casual_7$ride_length)))

### January, Casual

jan2023_mean_rl_casual_1 <- mean(seconds(hms(jan2023_casual_1$ride_length)))
jan2023_mean_rl_casual_2 <- mean(seconds(hms(jan2023_casual_2$ride_length)))
jan2023_mean_rl_casual_3 <- mean(seconds(hms(jan2023_casual_3$ride_length)))
jan2023_mean_rl_casual_4 <- mean(seconds(hms(jan2023_casual_4$ride_length)))
jan2023_mean_rl_casual_5 <- mean(seconds(hms(jan2023_casual_5$ride_length)))
jan2023_mean_rl_casual_6 <- mean(seconds(hms(jan2023_casual_6$ride_length)))
jan2023_mean_rl_casual_7 <- mean(seconds(hms(jan2023_casual_7$ride_length)))

### December, Casual

dec2022_mean_rl_casual_1 <- mean(seconds(hms(dec2022_casual_1$ride_length)))
dec2022_mean_rl_casual_2 <- mean(seconds(hms(dec2022_casual_2$ride_length)))
dec2022_mean_rl_casual_3 <- mean(seconds(hms(dec2022_casual_3$ride_length)))
dec2022_mean_rl_casual_4 <- mean(seconds(hms(dec2022_casual_4$ride_length)))
dec2022_mean_rl_casual_5 <- mean(seconds(hms(dec2022_casual_5$ride_length)))
dec2022_mean_rl_casual_6 <- mean(seconds(hms(dec2022_casual_6$ride_length)))
dec2022_mean_rl_casual_7 <- mean(seconds(hms(dec2022_casual_7$ride_length)))

### November, Casual

nov2022_mean_rl_casual_1 <- mean(seconds(hms(nov2022_casual_1$ride_length)))
nov2022_mean_rl_casual_2 <- mean(seconds(hms(nov2022_casual_2$ride_length)))
nov2022_mean_rl_casual_3 <- mean(seconds(hms(nov2022_casual_3$ride_length)))
nov2022_mean_rl_casual_4 <- mean(seconds(hms(nov2022_casual_4$ride_length)))
nov2022_mean_rl_casual_5 <- mean(seconds(hms(nov2022_casual_5$ride_length)))
nov2022_mean_rl_casual_6 <- mean(seconds(hms(nov2022_casual_6$ride_length)))
nov2022_mean_rl_casual_7 <- mean(seconds(hms(nov2022_casual_7$ride_length)))

### October, Casual

oct2022_mean_rl_casual_1 <- mean(seconds(hms(oct2022_casual_1$ride_length)))
oct2022_mean_rl_casual_2 <- mean(seconds(hms(oct2022_casual_2$ride_length)))
oct2022_mean_rl_casual_3 <- mean(seconds(hms(oct2022_casual_3$ride_length)))
oct2022_mean_rl_casual_4 <- mean(seconds(hms(oct2022_casual_4$ride_length)))
oct2022_mean_rl_casual_5 <- mean(seconds(hms(oct2022_casual_5$ride_length)))
oct2022_mean_rl_casual_6 <- mean(seconds(hms(oct2022_casual_6$ride_length)))
oct2022_mean_rl_casual_7 <- mean(seconds(hms(oct2022_casual_7$ride_length)))

### September, Casual

sep2022_mean_rl_casual_1 <- mean(seconds(hms(sep2022_casual_1$ride_length)))
sep2022_mean_rl_casual_2 <- mean(seconds(hms(sep2022_casual_2$ride_length)))
sep2022_mean_rl_casual_3 <- mean(seconds(hms(sep2022_casual_3$ride_length)))
sep2022_mean_rl_casual_4 <- mean(seconds(hms(sep2022_casual_4$ride_length)))
sep2022_mean_rl_casual_5 <- mean(seconds(hms(sep2022_casual_5$ride_length)))
sep2022_mean_rl_casual_6 <- mean(seconds(hms(sep2022_casual_6$ride_length)))
sep2022_mean_rl_casual_7 <- mean(seconds(hms(sep2022_casual_7$ride_length)))

### August, Casual

aug2022_mean_rl_casual_1 <- mean(seconds(hms(aug2022_casual_1$ride_length)))
aug2022_mean_rl_casual_2 <- mean(seconds(hms(aug2022_casual_2$ride_length)))
aug2022_mean_rl_casual_3 <- mean(seconds(hms(aug2022_casual_3$ride_length)))
aug2022_mean_rl_casual_4 <- mean(seconds(hms(aug2022_casual_4$ride_length)))
aug2022_mean_rl_casual_5 <- mean(seconds(hms(aug2022_casual_5$ride_length)))
aug2022_mean_rl_casual_6 <- mean(seconds(hms(aug2022_casual_6$ride_length)))
aug2022_mean_rl_casual_7 <- mean(seconds(hms(aug2022_casual_7$ride_length)))

### July, Casual

jul2022_mean_rl_casual_1 <- mean(seconds(hms(jul2022_casual_1$ride_length)))
jul2022_mean_rl_casual_2 <- mean(seconds(hms(jul2022_casual_2$ride_length)))
jul2022_mean_rl_casual_3 <- mean(seconds(hms(jul2022_casual_3$ride_length)))
jul2022_mean_rl_casual_4 <- mean(seconds(hms(jul2022_casual_4$ride_length)))
jul2022_mean_rl_casual_5 <- mean(seconds(hms(jul2022_casual_5$ride_length)))
jul2022_mean_rl_casual_6 <- mean(seconds(hms(jul2022_casual_6$ride_length)))
jul2022_mean_rl_casual_7 <- mean(seconds(hms(jul2022_casual_7$ride_length)))

month_mean_rldow_member_casual_table <- data.frame(month = c("jun2023", "may2023", "apr2023",
                                                          "mar2023", "feb2023", "jan2023", 
                                                          "dec2022", "nov2022", "oct2022",
                                                          "sep2022", "aug2022", "jul2022"),
                                                   member_ride_length_mean_1 = c(jun2023_mean_rl_member_1, may2023_mean_rl_member_1, apr2023_mean_rl_member_1,
                                                                                 mar2023_mean_rl_member_1, feb2023_mean_rl_member_1, jan2023_mean_rl_member_1,
                                                                                 dec2022_mean_rl_member_1, nov2022_mean_rl_member_1, oct2022_mean_rl_member_1,
                                                                                 sep2022_mean_rl_member_1, aug2022_mean_rl_member_1, jul2022_mean_rl_member_1),
                                                   casual_ride_length_mean_1 = c(jun2023_mean_rl_casual_1, may2023_mean_rl_casual_1, apr2023_mean_rl_casual_1,
                                                                                 mar2023_mean_rl_casual_1, feb2023_mean_rl_casual_1, jan2023_mean_rl_casual_1,
                                                                                 dec2022_mean_rl_casual_1, nov2022_mean_rl_casual_1, oct2022_mean_rl_casual_1,
                                                                                 sep2022_mean_rl_casual_1, aug2022_mean_rl_casual_1, jul2022_mean_rl_casual_1),
                                                   member_ride_length_mean_2 = c(jun2023_mean_rl_member_2, may2023_mean_rl_member_2, apr2023_mean_rl_member_2,
                                                                                 mar2023_mean_rl_member_2, feb2023_mean_rl_member_2, jan2023_mean_rl_member_2,
                                                                                 dec2022_mean_rl_member_2, nov2022_mean_rl_member_2, oct2022_mean_rl_member_2,
                                                                                 sep2022_mean_rl_member_2, aug2022_mean_rl_member_2, jul2022_mean_rl_member_2),
                                                   casual_ride_length_mean_2 = c(jun2023_mean_rl_casual_2, may2023_mean_rl_casual_2, apr2023_mean_rl_casual_2,
                                                                                 mar2023_mean_rl_casual_2, feb2023_mean_rl_casual_2, jan2023_mean_rl_casual_2,
                                                                                 dec2022_mean_rl_casual_2, nov2022_mean_rl_casual_2, oct2022_mean_rl_casual_2,
                                                                                 sep2022_mean_rl_casual_2, aug2022_mean_rl_casual_2, jul2022_mean_rl_casual_2),
                                                   member_ride_length_mean_3 = c(jun2023_mean_rl_member_3, may2023_mean_rl_member_3, apr2023_mean_rl_member_3,
                                                                                 mar2023_mean_rl_member_3, feb2023_mean_rl_member_3, jan2023_mean_rl_member_3,
                                                                                 dec2022_mean_rl_member_3, nov2022_mean_rl_member_3, oct2022_mean_rl_member_3,
                                                                                 sep2022_mean_rl_member_3, aug2022_mean_rl_member_3, jul2022_mean_rl_member_3),
                                                   casual_ride_length_mean_3 = c(jun2023_mean_rl_casual_3, may2023_mean_rl_casual_3, apr2023_mean_rl_casual_3,
                                                                                 mar2023_mean_rl_casual_3, feb2023_mean_rl_casual_3, jan2023_mean_rl_casual_3,
                                                                                 dec2022_mean_rl_casual_3, nov2022_mean_rl_casual_3, oct2022_mean_rl_casual_3,
                                                                                 sep2022_mean_rl_casual_3, aug2022_mean_rl_casual_3, jul2022_mean_rl_casual_3),
                                                   member_ride_length_mean_4 = c(jun2023_mean_rl_member_4, may2023_mean_rl_member_4, apr2023_mean_rl_member_4,
                                                                                 mar2023_mean_rl_member_4, feb2023_mean_rl_member_4, jan2023_mean_rl_member_4,
                                                                                 dec2022_mean_rl_member_4, nov2022_mean_rl_member_4, oct2022_mean_rl_member_4,
                                                                                 sep2022_mean_rl_member_4, aug2022_mean_rl_member_4, jul2022_mean_rl_member_4),
                                                   casual_ride_length_mean_4 = c(jun2023_mean_rl_casual_4, may2023_mean_rl_casual_4, apr2023_mean_rl_casual_4,
                                                                                 mar2023_mean_rl_casual_4, feb2023_mean_rl_casual_4, jan2023_mean_rl_casual_4,
                                                                                 dec2022_mean_rl_casual_4, nov2022_mean_rl_casual_4, oct2022_mean_rl_casual_4,
                                                                                 sep2022_mean_rl_casual_4, aug2022_mean_rl_casual_4, jul2022_mean_rl_casual_4),
                                                   member_ride_length_mean_5 = c(jun2023_mean_rl_member_5, may2023_mean_rl_member_5, apr2023_mean_rl_member_5,
                                                                                 mar2023_mean_rl_member_5, feb2023_mean_rl_member_5, jan2023_mean_rl_member_5,
                                                                                 dec2022_mean_rl_member_5, nov2022_mean_rl_member_5, oct2022_mean_rl_member_5,
                                                                                 sep2022_mean_rl_member_5, aug2022_mean_rl_member_5, jul2022_mean_rl_member_5),
                                                   casual_ride_length_mean_5 = c(jun2023_mean_rl_casual_5, may2023_mean_rl_casual_5, apr2023_mean_rl_casual_5,
                                                                                 mar2023_mean_rl_casual_5, feb2023_mean_rl_casual_5, jan2023_mean_rl_casual_5,
                                                                                 dec2022_mean_rl_casual_5, nov2022_mean_rl_casual_5, oct2022_mean_rl_casual_5,
                                                                                 sep2022_mean_rl_casual_5, aug2022_mean_rl_casual_5, jul2022_mean_rl_casual_5),
                                                   member_ride_length_mean_6 = c(jun2023_mean_rl_member_6, may2023_mean_rl_member_6, apr2023_mean_rl_member_6,
                                                                                 mar2023_mean_rl_member_6, feb2023_mean_rl_member_6, jan2023_mean_rl_member_6,
                                                                                 dec2022_mean_rl_member_6, nov2022_mean_rl_member_6, oct2022_mean_rl_member_6,
                                                                                 sep2022_mean_rl_member_6, aug2022_mean_rl_member_6, jul2022_mean_rl_member_6),
                                                   casual_ride_length_mean_6 = c(jun2023_mean_rl_casual_6, may2023_mean_rl_casual_6, apr2023_mean_rl_casual_6,
                                                                                 mar2023_mean_rl_casual_6, feb2023_mean_rl_casual_6, jan2023_mean_rl_casual_6,
                                                                                 dec2022_mean_rl_casual_6, nov2022_mean_rl_casual_6, oct2022_mean_rl_casual_6,
                                                                                 sep2022_mean_rl_casual_6, aug2022_mean_rl_casual_6, jul2022_mean_rl_casual_6),
                                                   member_ride_length_mean_7 = c(jun2023_mean_rl_member_7, may2023_mean_rl_member_7, apr2023_mean_rl_member_7,
                                                                                 mar2023_mean_rl_member_7, feb2023_mean_rl_member_7, jan2023_mean_rl_member_7,
                                                                                 dec2022_mean_rl_member_7, nov2022_mean_rl_member_7, oct2022_mean_rl_member_7,
                                                                                 sep2022_mean_rl_member_7, aug2022_mean_rl_member_7, jul2022_mean_rl_member_7),
                                                   casual_ride_length_mean_7 = c(jun2023_mean_rl_casual_7, may2023_mean_rl_casual_7, apr2023_mean_rl_casual_7,
                                                                                 mar2023_mean_rl_casual_7, feb2023_mean_rl_casual_7, jan2023_mean_rl_casual_7,
                                                                                 dec2022_mean_rl_casual_7, nov2022_mean_rl_casual_7, oct2022_mean_rl_casual_7,
                                                                                 sep2022_mean_rl_casual_7, aug2022_mean_rl_casual_7, jul2022_mean_rl_casual_7))

## Count of trip_id for members and casual riders each month, also considering the day

### June, Member

jun2023_count_trip_id_member_1 <- nrow(jun2023_member_1)
jun2023_count_trip_id_member_2 <- nrow(jun2023_member_2)
jun2023_count_trip_id_member_3 <- nrow(jun2023_member_3)
jun2023_count_trip_id_member_4 <- nrow(jun2023_member_4)
jun2023_count_trip_id_member_5 <- nrow(jun2023_member_5)
jun2023_count_trip_id_member_6 <- nrow(jun2023_member_6)
jun2023_count_trip_id_member_7 <- nrow(jun2023_member_7)

### May, Member

may2023_count_trip_id_member_1 <- nrow(may2023_member_1)
may2023_count_trip_id_member_2 <- nrow(may2023_member_2)
may2023_count_trip_id_member_3 <- nrow(may2023_member_3)
may2023_count_trip_id_member_4 <- nrow(may2023_member_4)
may2023_count_trip_id_member_5 <- nrow(may2023_member_5)
may2023_count_trip_id_member_6 <- nrow(may2023_member_6)
may2023_count_trip_id_member_7 <- nrow(may2023_member_7)

### April, Member

apr2023_count_trip_id_member_1 <- nrow(apr2023_member_1)
apr2023_count_trip_id_member_2 <- nrow(apr2023_member_2)
apr2023_count_trip_id_member_3 <- nrow(apr2023_member_3)
apr2023_count_trip_id_member_4 <- nrow(apr2023_member_4)
apr2023_count_trip_id_member_5 <- nrow(apr2023_member_5)
apr2023_count_trip_id_member_6 <- nrow(apr2023_member_6)
apr2023_count_trip_id_member_7 <- nrow(apr2023_member_7)

### March, Member

mar2023_count_trip_id_member_1 <- nrow(mar2023_member_1)
mar2023_count_trip_id_member_2 <- nrow(mar2023_member_2)
mar2023_count_trip_id_member_3 <- nrow(mar2023_member_3)
mar2023_count_trip_id_member_4 <- nrow(mar2023_member_4)
mar2023_count_trip_id_member_5 <- nrow(mar2023_member_5)
mar2023_count_trip_id_member_6 <- nrow(mar2023_member_6)
mar2023_count_trip_id_member_7 <- nrow(mar2023_member_7)

### February, Member

feb2023_count_trip_id_member_1 <- nrow(feb2023_member_1)
feb2023_count_trip_id_member_2 <- nrow(feb2023_member_2)
feb2023_count_trip_id_member_3 <- nrow(feb2023_member_3)
feb2023_count_trip_id_member_4 <- nrow(feb2023_member_4)
feb2023_count_trip_id_member_5 <- nrow(feb2023_member_5)
feb2023_count_trip_id_member_6 <- nrow(feb2023_member_6)
feb2023_count_trip_id_member_7 <- nrow(feb2023_member_7)

### January, Member

jan2023_count_trip_id_member_1 <- nrow(jan2023_member_1)
jan2023_count_trip_id_member_2 <- nrow(jan2023_member_2)
jan2023_count_trip_id_member_3 <- nrow(jan2023_member_3)
jan2023_count_trip_id_member_4 <- nrow(jan2023_member_4)
jan2023_count_trip_id_member_5 <- nrow(jan2023_member_5)
jan2023_count_trip_id_member_6 <- nrow(jan2023_member_6)
jan2023_count_trip_id_member_7 <- nrow(jan2023_member_7)

### December, Member

dec2022_count_trip_id_member_1 <- nrow(dec2022_member_1)
dec2022_count_trip_id_member_2 <- nrow(dec2022_member_2)
dec2022_count_trip_id_member_3 <- nrow(dec2022_member_3)
dec2022_count_trip_id_member_4 <- nrow(dec2022_member_4)
dec2022_count_trip_id_member_5 <- nrow(dec2022_member_5)
dec2022_count_trip_id_member_6 <- nrow(dec2022_member_6)
dec2022_count_trip_id_member_7 <- nrow(dec2022_member_7)

### November, Member

nov2022_count_trip_id_member_1 <- nrow(nov2022_member_1)
nov2022_count_trip_id_member_2 <- nrow(nov2022_member_2)
nov2022_count_trip_id_member_3 <- nrow(nov2022_member_3)
nov2022_count_trip_id_member_4 <- nrow(nov2022_member_4)
nov2022_count_trip_id_member_5 <- nrow(nov2022_member_5)
nov2022_count_trip_id_member_6 <- nrow(nov2022_member_6)
nov2022_count_trip_id_member_7 <- nrow(nov2022_member_7)

### October, Member

oct2022_count_trip_id_member_1 <- nrow(oct2022_member_1)
oct2022_count_trip_id_member_2 <- nrow(oct2022_member_2)
oct2022_count_trip_id_member_3 <- nrow(oct2022_member_3)
oct2022_count_trip_id_member_4 <- nrow(oct2022_member_4)
oct2022_count_trip_id_member_5 <- nrow(oct2022_member_5)
oct2022_count_trip_id_member_6 <- nrow(oct2022_member_6)
oct2022_count_trip_id_member_7 <- nrow(oct2022_member_7)

### September, Member

sep2022_count_trip_id_member_1 <- nrow(sep2022_member_1)
sep2022_count_trip_id_member_2 <- nrow(sep2022_member_2)
sep2022_count_trip_id_member_3 <- nrow(sep2022_member_3)
sep2022_count_trip_id_member_4 <- nrow(sep2022_member_4)
sep2022_count_trip_id_member_5 <- nrow(sep2022_member_5)
sep2022_count_trip_id_member_6 <- nrow(sep2022_member_6)
sep2022_count_trip_id_member_7 <- nrow(sep2022_member_7)

### August, Member

aug2022_count_trip_id_member_1 <- nrow(aug2022_member_1)
aug2022_count_trip_id_member_2 <- nrow(aug2022_member_2)
aug2022_count_trip_id_member_3 <- nrow(aug2022_member_3)
aug2022_count_trip_id_member_4 <- nrow(aug2022_member_4)
aug2022_count_trip_id_member_5 <- nrow(aug2022_member_5)
aug2022_count_trip_id_member_6 <- nrow(aug2022_member_6)
aug2022_count_trip_id_member_7 <- nrow(aug2022_member_7)

### July, Member

jul2022_count_trip_id_member_1 <- nrow(jul2022_member_1)
jul2022_count_trip_id_member_2 <- nrow(jul2022_member_2)
jul2022_count_trip_id_member_3 <- nrow(jul2022_member_3)
jul2022_count_trip_id_member_4 <- nrow(jul2022_member_4)
jul2022_count_trip_id_member_5 <- nrow(jul2022_member_5)
jul2022_count_trip_id_member_6 <- nrow(jul2022_member_6)
jul2022_count_trip_id_member_7 <- nrow(jul2022_member_7)

### June, Casual

jun2023_count_trip_id_casual_1 <- nrow(jun2023_casual_1)
jun2023_count_trip_id_casual_2 <- nrow(jun2023_casual_2)
jun2023_count_trip_id_casual_3 <- nrow(jun2023_casual_3)
jun2023_count_trip_id_casual_4 <- nrow(jun2023_casual_4)
jun2023_count_trip_id_casual_5 <- nrow(jun2023_casual_5)
jun2023_count_trip_id_casual_6 <- nrow(jun2023_casual_6)
jun2023_count_trip_id_casual_7 <- nrow(jun2023_casual_7)

### May, Casual

may2023_count_trip_id_casual_1 <- nrow(may2023_casual_1)
may2023_count_trip_id_casual_2 <- nrow(may2023_casual_2)
may2023_count_trip_id_casual_3 <- nrow(may2023_casual_3)
may2023_count_trip_id_casual_4 <- nrow(may2023_casual_4)
may2023_count_trip_id_casual_5 <- nrow(may2023_casual_5)
may2023_count_trip_id_casual_6 <- nrow(may2023_casual_6)
may2023_count_trip_id_casual_7 <- nrow(may2023_casual_7)

### April, Casual

apr2023_count_trip_id_casual_1 <- nrow(apr2023_casual_1)
apr2023_count_trip_id_casual_2 <- nrow(apr2023_casual_2)
apr2023_count_trip_id_casual_3 <- nrow(apr2023_casual_3)
apr2023_count_trip_id_casual_4 <- nrow(apr2023_casual_4)
apr2023_count_trip_id_casual_5 <- nrow(apr2023_casual_5)
apr2023_count_trip_id_casual_6 <- nrow(apr2023_casual_6)
apr2023_count_trip_id_casual_7 <- nrow(apr2023_casual_7)

### March, Casual

mar2023_count_trip_id_casual_1 <- nrow(mar2023_casual_1)
mar2023_count_trip_id_casual_2 <- nrow(mar2023_casual_2)
mar2023_count_trip_id_casual_3 <- nrow(mar2023_casual_3)
mar2023_count_trip_id_casual_4 <- nrow(mar2023_casual_4)
mar2023_count_trip_id_casual_5 <- nrow(mar2023_casual_5)
mar2023_count_trip_id_casual_6 <- nrow(mar2023_casual_6)
mar2023_count_trip_id_casual_7 <- nrow(mar2023_casual_7)

### February, Casual

feb2023_count_trip_id_casual_1 <- nrow(feb2023_casual_1)
feb2023_count_trip_id_casual_2 <- nrow(feb2023_casual_2)
feb2023_count_trip_id_casual_3 <- nrow(feb2023_casual_3)
feb2023_count_trip_id_casual_4 <- nrow(feb2023_casual_4)
feb2023_count_trip_id_casual_5 <- nrow(feb2023_casual_5)
feb2023_count_trip_id_casual_6 <- nrow(feb2023_casual_6)
feb2023_count_trip_id_casual_7 <- nrow(feb2023_casual_7)

### January, Casual

jan2023_count_trip_id_casual_1 <- nrow(jan2023_casual_1)
jan2023_count_trip_id_casual_2 <- nrow(jan2023_casual_2)
jan2023_count_trip_id_casual_3 <- nrow(jan2023_casual_3)
jan2023_count_trip_id_casual_4 <- nrow(jan2023_casual_4)
jan2023_count_trip_id_casual_5 <- nrow(jan2023_casual_5)
jan2023_count_trip_id_casual_6 <- nrow(jan2023_casual_6)
jan2023_count_trip_id_casual_7 <- nrow(jan2023_casual_7)

### December, Casual

dec2022_count_trip_id_casual_1 <- nrow(dec2022_casual_1)
dec2022_count_trip_id_casual_2 <- nrow(dec2022_casual_2)
dec2022_count_trip_id_casual_3 <- nrow(dec2022_casual_3)
dec2022_count_trip_id_casual_4 <- nrow(dec2022_casual_4)
dec2022_count_trip_id_casual_5 <- nrow(dec2022_casual_5)
dec2022_count_trip_id_casual_6 <- nrow(dec2022_casual_6)
dec2022_count_trip_id_casual_7 <- nrow(dec2022_casual_7)

### November, Casual

nov2022_count_trip_id_casual_1 <- nrow(nov2022_casual_1)
nov2022_count_trip_id_casual_2 <- nrow(nov2022_casual_2)
nov2022_count_trip_id_casual_3 <- nrow(nov2022_casual_3)
nov2022_count_trip_id_casual_4 <- nrow(nov2022_casual_4)
nov2022_count_trip_id_casual_5 <- nrow(nov2022_casual_5)
nov2022_count_trip_id_casual_6 <- nrow(nov2022_casual_6)
nov2022_count_trip_id_casual_7 <- nrow(nov2022_casual_7)

### October, Casual

oct2022_count_trip_id_casual_1 <- nrow(oct2022_casual_1)
oct2022_count_trip_id_casual_2 <- nrow(oct2022_casual_2)
oct2022_count_trip_id_casual_3 <- nrow(oct2022_casual_3)
oct2022_count_trip_id_casual_4 <- nrow(oct2022_casual_4)
oct2022_count_trip_id_casual_5 <- nrow(oct2022_casual_5)
oct2022_count_trip_id_casual_6 <- nrow(oct2022_casual_6)
oct2022_count_trip_id_casual_7 <- nrow(oct2022_casual_7)

### September, Casual

sep2022_count_trip_id_casual_1 <- nrow(sep2022_casual_1)
sep2022_count_trip_id_casual_2 <- nrow(sep2022_casual_2)
sep2022_count_trip_id_casual_3 <- nrow(sep2022_casual_3)
sep2022_count_trip_id_casual_4 <- nrow(sep2022_casual_4)
sep2022_count_trip_id_casual_5 <- nrow(sep2022_casual_5)
sep2022_count_trip_id_casual_6 <- nrow(sep2022_casual_6)
sep2022_count_trip_id_casual_7 <- nrow(sep2022_casual_7)

### August, Casual

aug2022_count_trip_id_casual_1 <- nrow(aug2022_casual_1)
aug2022_count_trip_id_casual_2 <- nrow(aug2022_casual_2)
aug2022_count_trip_id_casual_3 <- nrow(aug2022_casual_3)
aug2022_count_trip_id_casual_4 <- nrow(aug2022_casual_4)
aug2022_count_trip_id_casual_5 <- nrow(aug2022_casual_5)
aug2022_count_trip_id_casual_6 <- nrow(aug2022_casual_6)
aug2022_count_trip_id_casual_7 <- nrow(aug2022_casual_7)

### July, Casual

jul2022_count_trip_id_casual_1 <- nrow(jul2022_casual_1)
jul2022_count_trip_id_casual_2 <- nrow(jul2022_casual_2)
jul2022_count_trip_id_casual_3 <- nrow(jul2022_casual_3)
jul2022_count_trip_id_casual_4 <- nrow(jul2022_casual_4)
jul2022_count_trip_id_casual_5 <- nrow(jul2022_casual_5)
jul2022_count_trip_id_casual_6 <- nrow(jul2022_casual_6)
jul2022_count_trip_id_casual_7 <- nrow(jul2022_casual_7)

month_count_trip_id_member_casual_table <- data.frame(month = c("jun2023", "may2023", "apr2023",
                                                                "mar2023", "feb2023", "jan2023", 
                                                                "dec2022", "nov2022", "oct2022",
                                                                "sep2022", "aug2022", "jul2022"),
                                                      member_trip_id_count_1 = c(jun2023_count_trip_id_member_1, may2023_count_trip_id_member_1, apr2023_count_trip_id_member_1,
                                                                                 mar2023_count_trip_id_member_1, feb2023_count_trip_id_member_1, jan2023_count_trip_id_member_1,
                                                                                 dec2022_count_trip_id_member_1, nov2022_count_trip_id_member_1, oct2022_count_trip_id_member_1,
                                                                                 sep2022_count_trip_id_member_1, aug2022_count_trip_id_member_1, jul2022_count_trip_id_member_1),
                                                      casual_trip_id_count_1 = c(jun2023_count_trip_id_casual_1, may2023_count_trip_id_casual_1, apr2023_count_trip_id_casual_1,
                                                                                 mar2023_count_trip_id_casual_1, feb2023_count_trip_id_casual_1, jan2023_count_trip_id_casual_1,
                                                                                 dec2022_count_trip_id_casual_1, nov2022_count_trip_id_casual_1, oct2022_count_trip_id_casual_1,
                                                                                 sep2022_count_trip_id_casual_1, aug2022_count_trip_id_casual_1, jul2022_count_trip_id_casual_1),
                                                      member_trip_id_count_2 = c(jun2023_count_trip_id_member_2, may2023_count_trip_id_member_2, apr2023_count_trip_id_member_2,
                                                                                 mar2023_count_trip_id_member_2, feb2023_count_trip_id_member_2, jan2023_count_trip_id_member_2,
                                                                                 dec2022_count_trip_id_member_2, nov2022_count_trip_id_member_2, oct2022_count_trip_id_member_2,
                                                                                 sep2022_count_trip_id_member_2, aug2022_count_trip_id_member_2, jul2022_count_trip_id_member_2),
                                                      casual_trip_id_count_2 = c(jun2023_count_trip_id_casual_2, may2023_count_trip_id_casual_2, apr2023_count_trip_id_casual_2,
                                                                                 mar2023_count_trip_id_casual_2, feb2023_count_trip_id_casual_2, jan2023_count_trip_id_casual_2,
                                                                                 dec2022_count_trip_id_casual_2, nov2022_count_trip_id_casual_2, oct2022_count_trip_id_casual_2,
                                                                                 sep2022_count_trip_id_casual_2, aug2022_count_trip_id_casual_2, jul2022_count_trip_id_casual_2),
                                                      member_trip_id_count_3 = c(jun2023_count_trip_id_member_3, may2023_count_trip_id_member_3, apr2023_count_trip_id_member_3,
                                                                                 mar2023_count_trip_id_member_3, feb2023_count_trip_id_member_3, jan2023_count_trip_id_member_3,
                                                                                 dec2022_count_trip_id_member_3, nov2022_count_trip_id_member_3, oct2022_count_trip_id_member_3,
                                                                                 sep2022_count_trip_id_member_3, aug2022_count_trip_id_member_3, jul2022_count_trip_id_member_3),
                                                      casual_trip_id_count_3 = c(jun2023_count_trip_id_casual_3, may2023_count_trip_id_casual_3, apr2023_count_trip_id_casual_3,
                                                                                 mar2023_count_trip_id_casual_3, feb2023_count_trip_id_casual_3, jan2023_count_trip_id_casual_3,
                                                                                 dec2022_count_trip_id_casual_3, nov2022_count_trip_id_casual_3, oct2022_count_trip_id_casual_3,
                                                                                 sep2022_count_trip_id_casual_3, aug2022_count_trip_id_casual_3, jul2022_count_trip_id_casual_3),
                                                      member_trip_id_count_4 = c(jun2023_count_trip_id_member_4, may2023_count_trip_id_member_4, apr2023_count_trip_id_member_4,
                                                                                 mar2023_count_trip_id_member_4, feb2023_count_trip_id_member_4, jan2023_count_trip_id_member_4,
                                                                                 dec2022_count_trip_id_member_4, nov2022_count_trip_id_member_4, oct2022_count_trip_id_member_4,
                                                                                 sep2022_count_trip_id_member_4, aug2022_count_trip_id_member_4, jul2022_count_trip_id_member_4),
                                                      casual_trip_id_count_4 = c(jun2023_count_trip_id_casual_4, may2023_count_trip_id_casual_4, apr2023_count_trip_id_casual_4,
                                                                                 mar2023_count_trip_id_casual_4, feb2023_count_trip_id_casual_4, jan2023_count_trip_id_casual_4,
                                                                                 dec2022_count_trip_id_casual_4, nov2022_count_trip_id_casual_4, oct2022_count_trip_id_casual_4,
                                                                                 sep2022_count_trip_id_casual_4, aug2022_count_trip_id_casual_4, jul2022_count_trip_id_casual_4),
                                                      member_trip_id_count_5 = c(jun2023_count_trip_id_member_5, may2023_count_trip_id_member_5, apr2023_count_trip_id_member_5,
                                                                                 mar2023_count_trip_id_member_5, feb2023_count_trip_id_member_5, jan2023_count_trip_id_member_5,
                                                                                 dec2022_count_trip_id_member_5, nov2022_count_trip_id_member_5, oct2022_count_trip_id_member_5,
                                                                                 sep2022_count_trip_id_member_5, aug2022_count_trip_id_member_5, jul2022_count_trip_id_member_5),
                                                      casual_trip_id_count_5 = c(jun2023_count_trip_id_casual_5, may2023_count_trip_id_casual_5, apr2023_count_trip_id_casual_5,
                                                                                 mar2023_count_trip_id_casual_5, feb2023_count_trip_id_casual_5, jan2023_count_trip_id_casual_5,
                                                                                 dec2022_count_trip_id_casual_5, nov2022_count_trip_id_casual_5, oct2022_count_trip_id_casual_5,
                                                                                 sep2022_count_trip_id_casual_5, aug2022_count_trip_id_casual_5, jul2022_count_trip_id_casual_5),
                                                      member_trip_id_count_6 = c(jun2023_count_trip_id_member_6, may2023_count_trip_id_member_6, apr2023_count_trip_id_member_6,
                                                                                 mar2023_count_trip_id_member_6, feb2023_count_trip_id_member_6, jan2023_count_trip_id_member_6,
                                                                                 dec2022_count_trip_id_member_6, nov2022_count_trip_id_member_6, oct2022_count_trip_id_member_6,
                                                                                 sep2022_count_trip_id_member_6, aug2022_count_trip_id_member_6, jul2022_count_trip_id_member_6),
                                                      casual_trip_id_count_6 = c(jun2023_count_trip_id_casual_6, may2023_count_trip_id_casual_6, apr2023_count_trip_id_casual_6,
                                                                                 mar2023_count_trip_id_casual_6, feb2023_count_trip_id_casual_6, jan2023_count_trip_id_casual_6,
                                                                                 dec2022_count_trip_id_casual_6, nov2022_count_trip_id_casual_6, oct2022_count_trip_id_casual_6,
                                                                                 sep2022_count_trip_id_casual_6, aug2022_count_trip_id_casual_6, jul2022_count_trip_id_casual_6),
                                                      member_trip_id_count_7 = c(jun2023_count_trip_id_member_7, may2023_count_trip_id_member_7, apr2023_count_trip_id_member_7,
                                                                                 mar2023_count_trip_id_member_7, feb2023_count_trip_id_member_7, jan2023_count_trip_id_member_7,
                                                                                 dec2022_count_trip_id_member_7, nov2022_count_trip_id_member_7, oct2022_count_trip_id_member_7,
                                                                                 sep2022_count_trip_id_member_7, aug2022_count_trip_id_member_7, jul2022_count_trip_id_member_7),
                                                      casual_trip_id_count_7 = c(jun2023_count_trip_id_casual_7, may2023_count_trip_id_casual_7, apr2023_count_trip_id_casual_7,
                                                                                 mar2023_count_trip_id_casual_7, feb2023_count_trip_id_casual_7, jan2023_count_trip_id_casual_7,
                                                                                 dec2022_count_trip_id_casual_7, nov2022_count_trip_id_casual_7, oct2022_count_trip_id_casual_7,
                                                                                 sep2022_count_trip_id_casual_7, aug2022_count_trip_id_casual_7, jul2022_count_trip_id_casual_7))
# Full year view

full_year_no_negative_rl <- rbind(jun2023_no_negative_rl, may2023_no_negative_rl, apr2023_no_negative_rl,
                                  mar2023_no_negative_rl, feb2023_no_negative_rl, jan2023_no_negative_rl,
                                  dec2022_no_negative_rl, nov2022_no_negative_rl, oct2022_no_negative_rl,
                                  sep2022_no_negative_rl, aug2022_no_negative_rl, jul2022_no_negative_rl)

full_year <- rbind(jun2023, may2023, apr2023,
                   mar2023, feb2023, jan2023,
                   dec2022, nov2022, oct2022,
                   sep2022, aug2022, jul2022)

full_year_member <- filter(full_year_no_negative_rl, member_casual == "member")
full_year_casual <- filter(full_year_no_negative_rl, member_casual == "casual")

full_year_member_1 <- filter(full_year_member, day_of_week == 1)
full_year_member_2 <- filter(full_year_member, day_of_week == 2)
full_year_member_3 <- filter(full_year_member, day_of_week == 3)
full_year_member_4 <- filter(full_year_member, day_of_week == 4)
full_year_member_5 <- filter(full_year_member, day_of_week == 5)
full_year_member_6 <- filter(full_year_member, day_of_week == 6)
full_year_member_7 <- filter(full_year_member, day_of_week == 7)

full_year_casual_1 <- filter(full_year_casual, day_of_week == 1)
full_year_casual_2 <- filter(full_year_casual, day_of_week == 2)
full_year_casual_3 <- filter(full_year_casual, day_of_week == 3)
full_year_casual_4 <- filter(full_year_casual, day_of_week == 4)
full_year_casual_5 <- filter(full_year_casual, day_of_week == 5)
full_year_casual_6 <- filter(full_year_casual, day_of_week == 6)
full_year_casual_7 <- filter(full_year_casual, day_of_week == 7)

## mean of ride_length

full_year_mean_rl <- mean(seconds(hms(full_year_no_negative_rl$ride_length)))

## max ride_length

full_year_max_rl <- max(seconds(hms(full_year_no_negative_rl$ride_length)))

## mode of day_of_week

full_year_mode_dow <- Mode(full_year$day_of_week)

# Compare members and casual riders, but full year

## Average ride_length for members and casual riders full year

full_year_mean_rl_member <- mean(seconds(hms(full_year_member$ride_length)))
full_year_mean_rl_casual <- mean(seconds(hms(full_year_casual$ride_length)))

## Average ride_length for members and casual riders full year, also considering the day

full_year_mean_rl_member_1 <- mean(seconds(hms(full_year_member_1$ride_length)))
full_year_mean_rl_member_2 <- mean(seconds(hms(full_year_member_2$ride_length)))
full_year_mean_rl_member_3 <- mean(seconds(hms(full_year_member_3$ride_length)))
full_year_mean_rl_member_4 <- mean(seconds(hms(full_year_member_4$ride_length)))
full_year_mean_rl_member_5 <- mean(seconds(hms(full_year_member_5$ride_length)))
full_year_mean_rl_member_6 <- mean(seconds(hms(full_year_member_6$ride_length)))
full_year_mean_rl_member_7 <- mean(seconds(hms(full_year_member_7$ride_length)))

full_year_mean_rl_casual_1 <- mean(seconds(hms(full_year_casual_1$ride_length)))
full_year_mean_rl_casual_2 <- mean(seconds(hms(full_year_casual_2$ride_length)))
full_year_mean_rl_casual_3 <- mean(seconds(hms(full_year_casual_3$ride_length)))
full_year_mean_rl_casual_4 <- mean(seconds(hms(full_year_casual_4$ride_length)))
full_year_mean_rl_casual_5 <- mean(seconds(hms(full_year_casual_5$ride_length)))
full_year_mean_rl_casual_6 <- mean(seconds(hms(full_year_casual_6$ride_length)))
full_year_mean_rl_casual_7 <- mean(seconds(hms(full_year_casual_7$ride_length)))

## Count of trip_id for members and casual riders full year, also considering the day

full_year_count_trip_id_member_1 <- nrow(full_year_member_1)
full_year_count_trip_id_member_2 <- nrow(full_year_member_2)
full_year_count_trip_id_member_3 <- nrow(full_year_member_3)
full_year_count_trip_id_member_4 <- nrow(full_year_member_4)
full_year_count_trip_id_member_5 <- nrow(full_year_member_5)
full_year_count_trip_id_member_6 <- nrow(full_year_member_6)
full_year_count_trip_id_member_7 <- nrow(full_year_member_7)
full_year_count_trip_id_member <- nrow(full_year_member)

full_year_count_trip_id_casual_1 <- nrow(full_year_casual_1)
full_year_count_trip_id_casual_2 <- nrow(full_year_casual_2)
full_year_count_trip_id_casual_3 <- nrow(full_year_casual_3)
full_year_count_trip_id_casual_4 <- nrow(full_year_casual_4)
full_year_count_trip_id_casual_5 <- nrow(full_year_casual_5)
full_year_count_trip_id_casual_6 <- nrow(full_year_casual_6)
full_year_count_trip_id_casual_7 <- nrow(full_year_casual_7)
full_year_count_trip_id_casual <- nrow(full_year_casual)

# Visualization

## compare mean of ride_length, member vs casual, by month

viz1 <- month_mean_rl_member_casual_table %>%
  add_row(month = "full year", member_ride_length_mean = full_year_mean_rl_member, casual_ride_length_mean = full_year_mean_rl_casual)

viz1 <- data.frame(month = c("jun2023", "may2023", "apr2023",
                             "mar2023", "feb2023", "jan2023", 
                             "dec2022", "nov2022", "oct2022",
                             "sep2022", "aug2022", "jul2022", "full year",
                             "jun2023", "may2023", "apr2023",
                             "mar2023", "feb2023", "jan2023", 
                             "dec2022", "nov2022", "oct2022",
                             "sep2022", "aug2022", "jul2022", "full year"),
                   ride_length_mean = c(jun2023_mean_rl_member, may2023_mean_rl_member, apr2023_mean_rl_member,
                                        mar2023_mean_rl_member, feb2023_mean_rl_member, jan2023_mean_rl_member,
                                        dec2022_mean_rl_member, nov2022_mean_rl_member, oct2022_mean_rl_member,
                                        sep2022_mean_rl_member, aug2022_mean_rl_member, jul2022_mean_rl_member, full_year_mean_rl_member,
                                        jun2023_mean_rl_casual, may2023_mean_rl_casual, apr2023_mean_rl_casual,
                                        mar2023_mean_rl_casual, feb2023_mean_rl_casual, jan2023_mean_rl_casual,
                                        dec2022_mean_rl_casual, nov2022_mean_rl_casual, oct2022_mean_rl_casual,
                                        sep2022_mean_rl_casual, aug2022_mean_rl_casual, jul2022_mean_rl_casual, full_year_mean_rl_casual),
                   member_casual = c("member", "member", "member",
                                     "member", "member", "member",
                                     "member", "member", "member",
                                     "member", "member", "member", "member",
                                     "casual", "casual", "casual",
                                     "casual", "casual", "casual",
                                     "casual", "casual", "casual",
                                     "casual", "casual", "casual", "casual"))

mean_ride_length_member_casual_graph <- ggplot(viz1, aes(fill = member_casual, x = fct_inorder(month), y = ride_length_mean)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Comparing the Average Ride Length of Casual Riders and Cylistic Member by Month") +
  xlab("Month") + ylab("Average Ride Length") + labs(fill = "Member or Casual")

## Compare mean of ride_length, member vs casual, by day of week, full year (no by month, otherwise too many graphs)

viz2 <- data.frame(day_of_week = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "full year",
                                   "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "full year"),
                   ride_length_mean = c(full_year_mean_rl_member_1, full_year_mean_rl_member_2, full_year_mean_rl_member_3,
                                        full_year_mean_rl_member_4, full_year_mean_rl_member_5, full_year_mean_rl_member_6,
                                        full_year_mean_rl_member_7, full_year_mean_rl_member, full_year_mean_rl_casual_1,
                                        full_year_mean_rl_casual_2, full_year_mean_rl_casual_3, full_year_mean_rl_casual_4,
                                        full_year_mean_rl_casual_5, full_year_mean_rl_casual_6, full_year_mean_rl_casual_7,
                                        full_year_mean_rl_casual),
                   member_casual = c("member", "member", "member", "member", "member", "member", "member", "member",
                                     "casual", "casual", "casual", "casual", "casual", "casual", "casual", "casual"))

mean_ride_length_dow_graph <- ggplot(viz2, aes(fill = member_casual, x = fct_inorder(day_of_week), y = ride_length_mean)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Comparing the Average Ride Length of Casual Riders and Cylistic Member by the Day of Week") +
  xlab("Day of Week") + ylab("Average Ride Length") + labs(fill = "Member or Casual")

## Compare the number of rides, member vs casual, by day of week, full year (no by month, otherwise too many graphs)

viz3 <- data.frame(day_of_week = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
                                   "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                   ride_count = c(full_year_count_trip_id_member_1, full_year_count_trip_id_member_2, full_year_count_trip_id_member_3,
                                  full_year_count_trip_id_member_4, full_year_count_trip_id_member_5, full_year_count_trip_id_member_6,
                                  full_year_count_trip_id_member_7, full_year_count_trip_id_casual_1,
                                  full_year_count_trip_id_casual_2, full_year_count_trip_id_casual_3, full_year_count_trip_id_casual_4,
                                  full_year_count_trip_id_casual_5, full_year_count_trip_id_casual_6, full_year_count_trip_id_casual_7),
                   member_casual = c("member", "member", "member", "member", "member", "member", "member",
                                     "casual", "casual", "casual", "casual", "casual", "casual", "casual"))

ride_count_dow_graph <- ggplot(viz3, aes(fill = member_casual, x = fct_inorder(day_of_week), y = ride_count)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Comparing the Number of Rides of Casual Riders and Cylistic Member by the Day of Week") +
  xlab("Day of Week") + ylab("Number of Rides") + labs(fill = "Member of Casual")
options(scipen=999)

## Compare the number of rides, member vs casual, by month

jun2023_count_trip_id_member <- nrow(jun2023_member)
may2023_count_trip_id_member <- nrow(may2023_member)
apr2023_count_trip_id_member <- nrow(apr2023_member)
mar2023_count_trip_id_member <- nrow(mar2023_member)
feb2023_count_trip_id_member <- nrow(feb2023_member)
jan2023_count_trip_id_member <- nrow(jan2023_member)
dec2022_count_trip_id_member <- nrow(dec2022_member)
nov2022_count_trip_id_member <- nrow(nov2022_member)
oct2022_count_trip_id_member <- nrow(oct2022_member)
sep2022_count_trip_id_member <- nrow(sep2022_member)
aug2022_count_trip_id_member <- nrow(aug2022_member)
jul2022_count_trip_id_member <- nrow(jul2022_member)

jun2023_count_trip_id_casual <- nrow(jun2023_casual)
may2023_count_trip_id_casual <- nrow(may2023_casual)
apr2023_count_trip_id_casual <- nrow(apr2023_casual)
mar2023_count_trip_id_casual <- nrow(mar2023_casual)
feb2023_count_trip_id_casual <- nrow(feb2023_casual)
jan2023_count_trip_id_casual <- nrow(jan2023_casual)
dec2022_count_trip_id_casual <- nrow(dec2022_casual)
nov2022_count_trip_id_casual <- nrow(nov2022_casual)
oct2022_count_trip_id_casual <- nrow(oct2022_casual)
sep2022_count_trip_id_casual <- nrow(sep2022_casual)
aug2022_count_trip_id_casual <- nrow(aug2022_casual)
jul2022_count_trip_id_casual <- nrow(jul2022_casual)

viz4 <- data.frame(month = c("jun2023", "may2023", "apr2023",
                             "mar2023", "feb2023", "jan2023", 
                             "dec2022", "nov2022", "oct2022",
                             "sep2022", "aug2022", "jul2022",
                             "jun2023", "may2023", "apr2023",
                             "mar2023", "feb2023", "jan2023", 
                             "dec2022", "nov2022", "oct2022",
                             "sep2022", "aug2022", "jul2022"),
                   ride_count = c(jun2023_count_trip_id_member, may2023_count_trip_id_member, apr2023_count_trip_id_member,
                                  mar2023_count_trip_id_member, feb2023_count_trip_id_member, jan2023_count_trip_id_member,
                                  dec2022_count_trip_id_member, nov2022_count_trip_id_member, oct2022_count_trip_id_member,
                                  sep2022_count_trip_id_member, aug2022_count_trip_id_member, jul2022_count_trip_id_member,
                                  jun2023_count_trip_id_casual, may2023_count_trip_id_casual, apr2023_count_trip_id_casual,
                                  mar2023_count_trip_id_casual, feb2023_count_trip_id_casual, jan2023_count_trip_id_casual,
                                  dec2022_count_trip_id_casual, nov2022_count_trip_id_casual, oct2022_count_trip_id_casual,
                                  sep2022_count_trip_id_casual, aug2022_count_trip_id_casual, jul2022_count_trip_id_casual),
                   member_casual = c("member", "member", "member",
                                     "member", "member", "member",
                                     "member", "member", "member",
                                     "member", "member", "member",
                                     "casual", "casual", "casual",
                                     "casual", "casual", "casual",
                                     "casual", "casual", "casual",
                                     "casual", "casual", "casual"))

ride_count_member_casual_graph <- ggplot(viz4, aes(fill = member_casual, x = fct_inorder(month), y = ride_count)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Comparing the Number of Rides of Casual Riders and Cylistic Member by Month") +
  xlab("Month") + ylab("Number of Rides") + labs(fill = "Member of Casual")
