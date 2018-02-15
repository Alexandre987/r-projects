# Title: "ACS Tract Data"
# Name: Alexandre Zajic

# Load required functions and packages
library(tidyverse)
library(smodels)
library(forcats)
library(ggrepel)

theme_set(theme_minimal())

##############################################################################
# 01. The data for this data analysis has demographic data at a fairly
# grainular level called a Census Tract. Within cities these roughly
# correspond to neighborhoods. Read the data in with:

tract <- na.omit(read_csv("https://statsmaths.github.io/stat_data/acs_tract.csv"))

# The following fields are available for you to work with:
#
#   state                    - two letter state code
#   county                   - name of the county
#   cbsa_name                - name of core Based Statistical Area
#   cbsa_type                - type of core Based Statistical Area
#   median_income            - median household income
#   income_q1                - 20th percentile of household income
#   income_q2                - 40th percentile of household income
#   income_q3                - 60th percentile of household income
#   income_q4                - 80th percentile of household income
#   income_p95               - 95th percentile of household income
#   median_age               - median age of all people in area
#   age_00_02                - average children in age range per household
#   age_03_04                - average children in age range per household
#   age_05_05                - average children in age range per household
#   age_06_08                - average children in age range per household
#   age_09_11                - average children in age range per household
#   age_12_14                - average children in age range per household
#   age_15_18                - average children in age range per household
#   same_house               - percentage of households who lived in the same
#                              house one year ago
#   same_county              - percentage of households who moved within the
#                              county in the past year
#   same_state               - percentage of households who moved within the
#                              state, but outside the county, in the past year
#   avg_duration             - average duration of commute (DON'T USE)
#   car_alone                - percentage of workers who drive alone to work
#   carpool                  - percentage of workers who carpool to work
#   public_transit           - percentage of workers who take public transit
#   walked                   - percentage of workers who walk to work
#   transit_other            - percentage of workers who take "other" transit
#   at_home                  - percentage of workers who work at home



#   white_alone              - percentage of people identifying as white
#   native_american_alone    - percentage of people identifying as Native
#                              Americans
#   asian_alone              - percentage of people identifying as Asian
#   pacific_alone            - percentage of people identifying as Pacific
#                              Islanders or Hawaiians
#   black_alone              - percentage of people identifying as Black
#   other_alone              - percentage of people identifying as a single
#                              "other" race
#   multiracial              - percentage of people identifying as multiracial



#   healthcare_private       - percentage of people with private heathcare
#   healthcare_public        - percentage of people with public heathcare
#   healthcare_none          - percentage of people with no heathcare
#   housing_costs            - mean yearly housing costs
#   housing_000000_000000    - percentage of houses assessed in this range ($)
#   housing_010000_014999    - percentage of houses assessed in this range ($)
#   housing_015000_019999    - percentage of houses assessed in this range ($)
#   housing_020000_024999    - percentage of houses assessed in this range ($)
#   housing_025000_029999    - percentage of houses assessed in this range ($)
#   housing_030000_034999    - percentage of houses assessed in this range ($)
#   housing_035000_039999    - percentage of houses assessed in this range ($)
#   housing_040000_049999    - percentage of houses assessed in this range ($)
#   housing_050000_059999    - percentage of houses assessed in this range ($)
#   housing_060000_069999    - percentage of houses assessed in this range ($)
#   housing_070000_079999    - percentage of houses assessed in this range ($)
#   housing_080000_089999    - percentage of houses assessed in this range ($)
#   housing_090000_099999    - percentage of houses assessed in this range ($)
#   housing_100000_124999    - percentage of houses assessed in this range ($)
#   housing_125000_149999    - percentage of houses assessed in this range ($)
#   housing_150000_174999    - percentage of houses assessed in this range ($)
#   housing_175000_199999    - percentage of houses assessed in this range ($)
#   housing_200000_249999    - percentage of houses assessed in this range ($)
#   housing_250000_299999    - percentage of houses assessed in this range ($)
#   housing_300000_399999    - percentage of houses assessed in this range ($)
#   housing_400000_499999    - percentage of houses assessed in this range ($)
#   housing_500000_749999    - percentage of houses assessed in this range ($)
#   housing_750000_999999    - percentage of houses assessed in this range ($)
#   housing_above_1_million  - percentage of houses assessed in this range ($)
#   gini                     - GINI coefficient
#   lon                      - longitude
#   lat                      - latitude

##############################################################################
# 02. You are free to work on whatever you would like, here are some ideas
# to get you started:
#
# Can you build a measurement of income inequality within each tract? How is
# this distributed across the country? How does it relate the city or state
# that the tract is in?
#
# Similarly, can you build a measurment of racial segregation?
#
# Try using the grouped mutate to build a measurment of a tracts relative
# affluence.
#
# Perhaps summarize your results at the CBSA level? What are the most and
# least seggregated metropolitan areas, for example?



temp <- summarize_all(select_if(group_by(tract, cbsa_name), is.numeric), funs(mean))





##############################################################################

# IDEAS:

# I might want to find a way to compare income distribution to housing price
# distribution, and see if the relationship is direct, or if poorer people
# spend a larger percentage of their income on housing.

qplot(median_income, housing_costs, data = temp) + geom_smooth()

model <- lm_basic(housing_costs ~ 1 + median_income, data = temp)
reg_table(model, level = 0.95)


# Or I could see if this effect changes by race...

# Use the GINI coefficient along with race, with the dataset "temp"

model <- lm_basic(gini ~ 1 + lat + black_alone, data = temp)
reg_table(model, level = 0.95)



temp2 <- filter(temp, cbsa_name == "Starkville, MS")
temp3 <- filter(temp, cbsa_name == "Fort Polk South, LA")


qplot(lat, gini, label = cbsa_name, size = black_alone, alpha = (0.2), data = temp) +
  geom_point(data = temp2, color = I("red")) +
  geom_point(data = temp3, color = I("red")) +
  geom_text_repel(data = temp2) +
  geom_text_repel(data = temp3) +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 40.7) +
  ggtitle("Income Inequality of US neighborhoods, compared with latitude and race") +
  xlab("Latitude") +
  ylab("GINI coefficient")

# geom_vline is set to the latitude of NYC, for explanatory purposes



##############################################################################


# Professor Arnold recommended summarizing on the level of the cbsa_name, to
# make the data more broad and interesting.


# This is code that Professor Arnold wrote himself to separate the components
# of group_summarize. This one ONLY does the mean (but does not add the "_mean"
# tag after the variable name, so be careful!). Sub out "median" or "sum" or "sd"
# for funs(mean) at the end, for the others.



qplot(total_population_sum, median_income_sd, data = temp) + scale_x_log10()

# This shows how heterogeneous (segregated?) neighborhoods are in terms of
# income inequality. We find that as neighborhoods increase in population,
# the wealth inequality within these neighborhoods increases.


##############################################################################


# If I want to convert a categorical variable into a numeric, I'd create a
# "flag" for that variable. This one makes a flag for if a home is in
# a Metropolitan area. "metro_flag" is the new variable that I'm creating.

temp <- mutate(tract, metro_flag = as.numeric(cbsa_type == "Metropolitan"))


# To add up all the population in a column...
sum(tract$total_population)





##############################################################################





