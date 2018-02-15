# Title: "Flight Delays"
# Name: Alexandre Zajic

# Load required functions and packages
library(tidyverse)
library(smodels)
library(forcats)
library(ggrepel)

theme_set(theme_minimal())

##############################################################################
# 01. This data analysis looks at flight data from a year of domestic US
# flights. As the data is fairly large, I took a 10% sample of the entire
# dataset, though in your analysis you may treat this as a complete sample.

airports <- read_csv("https://statsmaths.github.io/stat_data/airports_10.csv")

# The following fields are available for you to work with:
#
#   dep_month       - month of the year
#   dep_wday        - day of the week, 1 = Sunday, 7 = Saturday
#   dep_doy         - integer giving the day of the year, starting Jan 01
#   dep_week        - integer giving the week number (1-53), starting in Jan.
#   dep_hour        - departure hour; local time at departure
#   arr_hour        - arrival hour; local time at departure
#   arr_hour_local  - arrival hour; local time of the destination airport
#   origin          - IATA code of the arrival airport
#   dest            - IATA code of the destination airport
#   carrier         - the airline operating the flight
#   dep_delay       - actual departure delay in seconds
#   arr_delay       - actual arrival delay in seconds
#   dist            - distance of the flight, in kilometers
#   sched_tot_time  - scheduled time of the flight in seconds
#   origin_lat      - latitude of the flight's origin airport
#   origin_lon      - longitude of the flight's origin airport
#   dest_lat        - latitude of the flight's destination airport
#   dest_lon        - longitude of the flight's destination airport

##############################################################################
##############################################################################
##############################################################################
# 02. You might start by analyzing flight delays. What aspects lead a flight
# taking off late? Are these the same as those factors that lead it to arrive
# late? Are there specific airport effects?


##############################################################################

model <- lm_basic(dep_delay ~ 1 + carrier, data = airports)
reg_table(model, level = 0.95)

# Departure delays seem to vary from almost nothing (for AQ and HA) to 15 minutes
# (AA). This means that in a 95% confidence interval, the average of a further
# flight would be no delay for AQ and HA, with a 15 minute delay for American Airlines.

###############################################################################

# Now I want to filter out "negative delays", since those are flights that are too early,
# and aren't relevant in measuring the average amount of time a flight is delayed, and it skews the data.

no_neg_dep_delay <- filter(airports, dep_delay >= 0)
no_neg_delay <- filter(no_neg_dep_delay, arr_delay >= 0)

# I will now rerun the model with "negative delays" removed

model <- lm_basic(arr_delay ~ 1 + carrier, data = no_neg_delay)
reg_table(model, level = 0.95)

model <- lm_basic(dep_delay ~ 1 + carrier, data = no_neg_delay)
reg_table(model, level = 0.95)

# Removing negative delays increases the average delay, and the new intercept is 1,935 seconds for
# all carriers.

  
###############################################################################

# I am going to model delays for both arrival and departure by carrier, and see if they
# are similar or different. They SHOULD be similar.

model <- lm_basic(arr_delay ~ 1 + carrier, data = no_neg_delay)
reg_table(model, level = 0.95)

model <- lm_basic(dep_delay ~ 1 + carrier, data = no_neg_delay)
reg_table(model, level = 0.95)


# Logically, arrival delays may be a large part of what cause departure delays
# for the next flight.

# I will measure Arr_delay:Dep_delay ratios (These are significant because they will show us
# how much each airline "recovers" from their arrival delays, to a lower departure
# delay, since it seems like arrival delays are what "feed" departure delays.)

#Delta
1993/1718 #(1.160)

#JetBlue
3191/2887 #(1.105)

#AmericanAirlines
2689/2452 #(1.097)

#Southwest
1496/1559 #(0.960)

# Note: Southwest is the only airline in this sample that does not "recover" from 
# arr_delays, and their dep_delays are longer than arr_delays.


# When the numbers are crunched, this means that in the average flight, Delta recovers
# 4.5 minutes of delays from arrival to departure, while Southwest loses 1 minute.
# We can extrapolate this to mean that over every flight, Southwest is 5.5 minutes
# slower in the gate than Delta. This information would be useful to the company.


# Now.... can we model arr_delay as a function of carrier and dep_delay? I'm trying to
# combine delays into one variable that is easy to see.

model <- lm_basic(arr_delay ~ 1 + carrier + dep_delay, data = no_neg_delay)
reg_table(model, level = 0.95)

qplot(dep_delay, arr_delay, data = no_neg_delay) + geom_smooth(method = "lm")

# dep_delay and arr_delay are almost in a perfect 1:1 relationship, but with a slight
# increase of delays on the arrival side. This suggests that the delays are largely caused
# mid-flight, and need to be compensated for by adjustments on the ground.

no_neg_delay_filtered <- filter(no_neg_delay, dep_delay <= 7200)

model <- lm_basic(arr_delay ~ 1 + carrier + dep_delay, data = no_neg_delay_filtered)
reg_table(model, level = 0.95)

model <- lm_basic(dep_delay ~ 1 + carrier + dep_delay, data = no_neg_delay_filtered)
reg_table(model, level = 0.95)

#Delta
393/1325
  
#Southwest
47/1298

qplot(dep_delay, arr_delay, data = no_neg_delay_filtered, alpha = I(0.1)) + 
  geom_smooth(method = "lm")

model <- lm_basic(arr_delay ~ 1 + carrier + dep_delay, data = no_neg_delay)
reg_table(model, level = 0.95)

###############################################################################

# Now that I have explored the data, I am trying to find data that is interesting
# to write an article on.

# Instead of filtering out big delays and small delays, group_summarize over a binned
# dataset (Professor's advice to summarize over the bins, but I want to summarize by carrier)

delay_bin <- mutate(no_neg_delay, bin(dep_delay, 4))

delay_bin <- mutate(delay_bin, bin(arr_delay, 4))




z <- group_summarize(delay_bin, carrier, bin(dep_delay), bin(arr_delay))

x <- group_summarize(no_neg_delay_filtered, carrier)




qplot(dep_delay_mean, arr_delay_mean, data = z) + geom_smooth(method = "lm")

qplot(dep_delay_mean, arr_delay_mean, data = x) + geom_smooth(method = "lm")




###############################################################################
# Note:
# Article could be written informally, aimed at finding how much of one's carrier delays
# are their fault, and different from airline to airline. A "Which Airline Should You
# Choose for the Least Delays?" kind of article


no_neg_dep_delay <- filter(airports, dep_delay >= 0)
no_neg_delay <- filter(no_neg_dep_delay, arr_delay >= 0)


###############################################################################
# Point 1:
# Flight delays by carrier vary substantially. It matters what airline you choose in
# terms of your likelihood for delays. (Show average departure delays by carrier)









###############################################################################
# Point 2: But, the kinds of delays experienced between carriers vary. Some carriers
# have a high number of big delays, while others do not. (Use true:false ratio of 30 minutes
# as the cutoff point)
# 
#
#



# This flags all departure delays under 30 minutes as "TRUE", and over as "FALSE"


temp <- mutate(no_neg_delay, dep_delay_flag = as.numeric(dep_delay >= 1800))
z <- group_summarize(temp, carrier)

qplot(carrier, dep_delay_flag_mean, data = z)



# Now I can also increase or decrease the size of the dots based on the average departure delay...

temp <- mutate(temp, arr_delay_flag = as.numeric(arr_delay <= 1800))
z <- group_summarize(temp, carrier)

top_5 <- filter(z, n > 13500)

# Now I will create my final, polished graphs that can be included in my article


qplot(carrier, dep_delay_flag_mean * 100, data = z, size = n, alpha = dep_delay_mean) +
  xlab("Airline carrier") +
  ylab("% Chance of departure delays > 30 min") +
  ggtitle("What's your risk of a delay greater than 30 minutes?")

qplot(carrier, dep_delay_flag_mean * 100, data = z, size = n) +
  geom_point(data = top_5, color = I("red")) +
  xlab("Airline carrier") +
  ylab("% Chance of departure delays > 30 min") +
  ggtitle("What's your risk of a delay greater than 30 minutes?")



model <- lm_basic(dep_delay_mean ~ 1 + dist_mean, data = z)
reg_table(model, level = 0.95)



###############################################################################
# Point 3: Outlier delays count for a huge proportion of overall flight delays -
# so if you have a small delay, you're relatively lucky (incorporate w/ average departure delay by a
# few different carriers, like Delta = 17, Southwest = 20, etc). Mention that most flights have
# absolutely no delays at all,
#
#
#






###############################################################################
# EXPERIMENTAL CODE

temp2 <- mutate(airports, dep_delay_flag = as.numeric(dep_delay <= 1800, dep_delay >= 0))
x <- group_summarize(temp2, carrier)
z <- group_summarize(temp, carrier)

temp2 <- mutate(temp2, dep_delay_flag = as.numeric(arr_delay <= 1800))
z <- group_summarize(temp, carrier)



