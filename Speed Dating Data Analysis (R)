# Title: "Speed Dating"
# Name: Alexandre Zajic

# Load required functions and packages
library(tidyverse)
library(smodels)
library(forcats)
library(ggrepel)

theme_set(theme_minimal())

##############################################################################
# 01. This data analysis studies a survey dataset from a series of speed
# dating events. There are two rows of the dataset for each date; one from
# the perspective of each person. All non-heterosexual relationships were
# removed from the dataset I received, so these are all Male-Female pairings.

speed <- read_csv("https://statsmaths.github.io/stat_data/speed_dating.csv")

# The following fields are available for you to work with:
#
#   match              - equl to 1 if both partners wanted to date one another
#   decision           - equal to 1 if respondent wanted to date
#   decision_partner   - equal to 1 if the partner wanted to date
#   male_flag          - equal to 1 if respondent is a male
#   samerace           - equal to 1 if partners are the same race
#   age                - age of the respondent
#   age_partner        - age of the partner
#   goal_code          - code indicating why the respondent participated
#
#                           1 - Seemed like a fun night out
#                           2 - To meet new people
#                           3 - To get a date
#                           4 - Looking for a serious relationship
#                           5 - To say I did it
#                           6 - Other
#
#   date_frequency     - code indicating how frequently the respondent goes
#                        out on dates
#
#                           1 - Several times a week
#                           2 - Twice a week
#                           3 - Once a week
#                           4 - Twice a month
#                           5 - Once a month
#                           6 - Several times a year
#                           7 - Almost never
#
#   go_out_frequency   - code indicating how frequently the respondent goes
#                        out socially
#
#                           1 - Several times a week
#                           2 - Twice a week
#                           3 - Once a week
#                           4 - Twice a month
#                           5 - Once a month
#                           6 - Several times a year
#                           7 - Almost never
#
#   expected_happy     - on a scale of 1-10, how happy did the respondent
#                        expect to be with the people they meet during the
#                        speed-dating event
#   score_attractive   - how attractive did the respondent find their
#                        partner (scale of 1-10)
#   score_sincere      - how sincere did the respondent find their partner
#                        (scale of 1-10)
#   score_intelligence - how intelligent did the respondent find their
#                        partner (scale of 1-10)
#   score_fun          - how fun did the respondent find their partner (scale
#                        of 1-10)
#   score_ambition     - how ambitious did the respondent find their partner
#                        (scale of 1-10)
#   score_interest     - how interesting did the respondent find their partner
#                        (scale of 1-10)
#   score_overall      - overall, how did the respondent find their partner
#                        (scale of 1-10)
#   score_prob_match   - how likely do you think you are to match with your
#                        partner? (scale 1-10)

##############################################################################
# 02. Task: There are several directions you could go with this, but a good
# place to start is looking at what features predict whether a respondent's
# partner matched with them.

z <- group_summarize(speed, decision)

# Next, I will separate men and women into new datasets through filtering,
# and group_summarize by these new datasets, to further explore.

male <- filter(speed, male_flag == "1")
female <- filter(speed, male_flag == "0")

z_male <- group_summarize(male, decision)
z_female <- group_summarize(female, decision)
z_decision <- group_summarize(speed, male_flag)


# Decision breakdown based on ratings of their partner:

model <- lm_basic(decision ~ 1 + score_attractive + score_sincere + score_intelligence +
                    score_fun + score_ambition + score_interest, data = male)
reg_table(model, level = 0.95)

model <- lm_basic(decision ~ 1 + score_attractive + score_sincere + score_intelligence +
                    score_fun + score_ambition + score_interest, data = female)
reg_table(model, level = 0.95)


# Decision breakdown based on race

model <- lm_basic(decision ~ 1 + samerace, data = female)
reg_table(model, level = 0.95)
model <- lm_basic(decision ~ 1 + samerace + score_attractive, data = female)
reg_table(model, level = 0.95)




##############################################################################


model <- lm_basic(decision ~ 1 + male_flag + decision_partner, data = speed)
reg_table(model, level = 0.95)

# This model attempts to filter out an individual's decision-making, based on whether
# they are male, and how their partner decided. Males are 10% more likely to say yes
# independently of their partner's decision, and partners are 5% less likely to say
# yes if the respondent said yes (and vice-versa - 5% more likely to say yes if
# the respondent said no). This hints that there exists "leagues" of people, where
# people are more likely to try to "date up", 5% of the time.

model <- lm_basic(decision ~ 1 + score_attractive + score_sincere + score_intelligence +
                    score_fun + score_ambition + score_interest, data = speed)
reg_table(model, level = 0.95)

# This model (which does not separate males and females), says that attractiveness,
# fun, and interest of the partner improve the respondent's chance of deciding yes.
# Perceived ambition and sincerity of the partner were detrimental to the respondent's
# chances of saying yes. Intelligence was not significant either way.

model <- lm_basic(decision ~ 1 + score_attractive + score_sincere + score_intelligence +
                    score_fun + score_ambition + score_interest + score_prob_match, data = speed)
reg_table(model, level = 0.95)

# Next step would be to filter out males (or females), and conduct the same model
# under those datasets, to see if there are gendered differences in personality.

male <- filter(speed, male_flag == "1")
female <- filter(speed, male_flag == "0")

# The model below will be for personality scores from male respondents

model <- lm_basic(decision ~ 1 + score_attractive + score_sincere + score_intelligence +
                    score_fun + score_ambition + score_interest, data = male)
reg_table(model, level = 0.95)

model <- lm_basic(decision ~ 1 + score_attractive + score_sincere + score_intelligence +
                    score_fun + score_ambition + score_interest, data = female)
reg_table(model, level = 0.95)



# How do these models account for intelligence levels? For instance, do they explain
# Will's thesis that people prefer those of equal intelligence?

# There are problems with this model, for it models from negative numbers. But the worst a person can get is 0,
# not a negative number. Though this is probably just the model's accounting for the fact
# that people receive 0's on decision without having zero scores on their traits, but
# could this still disturb the results?

# What would it mean to add up the numbers
# of a hypothetical person and get a decision score of 0.7? Does this directly translate
# to the respondent having a 70% chance of saying yes to their partner?

# Also, it can't possibly be that 0 sincerity and 0 ambition are the best, though
# they score negatively in a linear relationship for both men and women. How can a model account for these
# correlations that should be more of a "bell curve" when visualized, rather than
# a linear model?

# Is there any way to get a model that is non-linear? In other words, to see how much the
# marginal change from 5 to 6 sincerity is "worth", compared to 0 to 1.




# Let's see if we can't get a more direct result from isolating the variables
# in our model...

model <- lm_basic(decision ~ 1 + score_attractive + score_intelligence, data = female)
reg_table(model, level = 0.95)


# Should I only model all of the personality traits at the same time, otherwise the
# modeling is not legitimate? If the answer is yes (which it probably is), then what
# does that mean for my model's results, considering there are only 6 personality
# traits queried for?



##############################################################################

# Now I will create a model that models respondents' likelihood to agree to a
# second date based on other factors like race, and attractiveness.


model <- lm_basic(decision ~ 1 + samerace, data = male)
reg_table(model, level = 0.95)

# Men's change in willingness for a second date is statistically insignificant
# based on being the same race as their partner.

model <- lm_basic(decision ~ 1 + samerace, data = female)
reg_table(model, level = 0.95)


model <- lm_basic(decision ~ 1 + samerace + score_attractive, data = female)
reg_table(model, level = 0.95)

model <- lm_basic(decision ~ 1 + samerace + score_attractive + score_sincere +
                    score_intelligence + score_ambition + score_fun + score_interest, data = female)
reg_table(model, level = 0.95)

#### FINDING #####
# From the above two models, female preferences for same race partners IS statistically
# significant in deciding partners, even though it is not for men. However, it is possible that
# race is hiding the importance of other factors. Once attractiveness was factored out in addition
# to race, the model found that being the same race as the female respondent gave men no advantage
# over men of a different race. This suggests that the female respondents submitting the survey
# found men of the same race more attractive on average than men of other races. This same effect
# was not observed in data from male respondents.


# Do a table, and group_summarize by the samerace and gender, and see how score_attractive
# works respectively.

sr <- group_summarize(speed, samerace, male_flag)

# Scroll over to score_attractive_mean, and my effect is confirmed.

#### CONCLUSION: ####
# Women are 5% more likely to agree to a date with a person of the
# same race. When we adjust for score_attractive, we find that the effect
# disappears. This suggests that women consider members of the same race more attractive.
# The effect is not statistically significant for men.
# (Men find samerace people .08 more attractive, while women find them .37 more
# attractive). The effect holds even more clearly for rating the overall date.
# (Men rate the overall date .006 lower for samerace -- insignificant.
# Women rate the overall date .32 higher for samerace).





# IMPORTANT: According to this, samerace has no significant difference for males, BUT it
# increases women's likelihood by 5%. Is it this simple? When looking at samerace_mean
# for women, it is 6% higher for women, and .05% for males (not significant)

# When adjusting for score_attractive, the effect of samerace becomes insignificant
# for both men and women, which suggests that women consider people of the same
# race more attractive, whereas men show no preference.

# Time to do an analysis of racial and gender dynamics through a speed dating dataset!

model <- lm_basic(decision ~ 1 + samerace, data = female)
reg_table(model, level = 0.95)

model <- lm_basic(decision ~ 1 + samerace + score_sincere, data = female)
reg_table(model, level = 0.95)






model <- lm_basic(decision ~ 1 + expected_happy, data = female)
reg_table(model, level = 0.95)

# There is a statistical significance for females and expectations, though not for males.
# Females on dates who decision = 1, had a mean of 5.5 expected happiness mean, while those
# w/ 0 had a mean of 5.0. Men had roughly the same scores of 5.8/5.7, overall expecting
# the speed dates to go better.

# Women had on average, a 46.4% chance of getting a date when they decided yes, while men
# had, on average, a 35.8% chance of the same. 

# Both men and women have equal confidence in the success of the date





##############################################################################

# Let's look for other correlations that might not suffer from the same shortcomings
# of the linear model as the ratings.

model <- lm_basic(decision ~ 1 + age + age_partner, data = female)
reg_table(model, level = 0.95)







##############################################################################

qplot(date_frequency, data = female)

qplot(date_frequency, data = male)


##############################################################################


# I wonder if there's a way to test for how important attractiveness is over age
# differences.... for younger and older individuals. Can't do a model of
# (decision ~ 1 + score_attractive + age), because that is just measuring likelihood of
# saying yes by age...

female <- mutate(female, age_partner - age)

model <- lm_basic(decision ~ 1 + score_attractive * (age_partner - age), data = female)
reg_table(model, level = 0.95)

#  Through multiplying, you can see one variable's dependence on another.
# (age's importance on attractiveness's importance on decision)




female <- mutate(female, age_cat = "young")
female <- mutate(female, age_cat = ifelse( age >= 27, "middle", age_cat))
female <- mutate(female, age_cat = ifelse( age >= 35, "old", age_cat))


model <- lm_basic(decision ~ 1 + score_attractive * age_cat, data = female)
reg_table(model, level = 0.95)


# Try to fct_relevel then, to young being first.



##############################################################################




##############################################################################

# This is useful for score, because the columns are measuring different values of
# the same thing (scores on traits). This takes the aggregate of all scores, and
# plots them together.

# For males...

temp <- select(male, age, score_attractive, score_sincere, score_intelligence,
               score_fun, score_ambition, score_interest)
temp <- gather(temp, personality, score, -age)


qplot(score, data = temp) + facet_wrap(~personality)


# Now for females....


temp <- select(female, age, score_attractive, score_sincere, score_intelligence,
               score_fun, score_ambition, score_interest)
temp <- gather(temp, personality, score, -age)


qplot(score, data = temp) + facet_wrap(~personality)


# This doesn't really show interesting results....


##############################################################################

# Let's try the same thing, but this time with the other personality factors,
# like going out frequency and number of dates in a month



temp <- select(female, age, goal_code, date_frequency, go_out_frequency, expected_happy)
temp <- gather(temp, personality, score, -age)


qplot(score, data = temp) + facet_wrap(~personality)





