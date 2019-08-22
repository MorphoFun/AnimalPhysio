#########################################################
##                Thermal Physiology                   ##
##                 Shrimp heart rate                   ##
##               Last updated 7-30-19                  ##
##                                                     ##
#########################################################

## Click on the "Run" button to run the entire script OR
## Run through line-by-line by hitting Control+Enter (windows) or Command+Enter (Macs)

#### Load in the data ####
## Load in the data from your computer
## This will open a new window to allow users to manually select their data
## Keep in mind that this will only open .csv files
## Since "data" is often used in functions, we try to avoid using it as a name for one of our variables
## Instead, we'll refer to it as "df"

df <- read.csv(file.choose(), header = TRUE)

## Let's look at the data
df

## Does it contain all of the rows and columns that we expect? How are the data types categorized? 
## Let's look at the data structure using the str() function
str(df)

## Good! We can confirm that there are 60 observations/rows because we have 20 student groups with three water temperatures each
## We also see that there are four variables:
## First, "Group" which is an integer (int) because we counted off the groups from 1 to 20
## Second, we have Temperature_Group which is categorized as a "Factor" which is R's way of saying it's a Grouping Category
## and includes three levels (Cold, Normal, and Warm)
## Third, we have HeartRate_bpm which is an integer (int) because we only recorded the beats per min in integers (no decimal places)

## Later we'll want to know the number of groups to calculate the standard error.
## How can we easily extract that information from our dataset?
## There are MANY ways to do this and students can get creative with this 
## but one way is to count the number of unique entries in our variable.
## The length() function counts the number of entries in a vector whereas the
## unqiue() function identifies each unique entry in a variable.

NumberOfGroups <- length(unique(df$Group))

## This tells us that there are 20 student groups, which is correct!
NumberOfGroups


#### Load in the libraries ####
## Libraries are add-ons to R that aren't immediately available when we install R
## This will install the ggplot2 graphing package if it isn't already installed
## and then will open up the ggplot2 package with the library() function

if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}

#### Let's visualize the data ####

## Rearrange the order of the labels so the "Normal" temperature is in the middle
temp_rearrange <- factor(df$Temperature_Group, levels = c("Cold", "Normal", "Warm"))

## Create an empty graph to then plot the data
## We're going to store this empty graph into p1 so we can call it back again later
p1 <- ggplot(df, aes(x = temp_rearrange, y = HeartRate_bpm))

## Add the boxplots to the empty graph that you just made
p1 + geom_boxplot(aes(color = Temperature_Group, fill = Temperature_Group), alpha =0.5)  + 
  scale_color_manual(values = c("blue", "black", "red")) +
  scale_fill_manual(values = c("blue", "white", "red")) +
  labs(x = "\n Temperature Group", y = "Heart Rate (bpm) \n") + 
  theme_classic()

## Now make a grayscale version of the graph 
p1 + geom_boxplot(aes(color = Temperature_Group, fill = Temperature_Group), alpha =0.5)  + 
  scale_fill_grey() +
  scale_colour_grey() +
  labs(x = "\n Temperature Group", y = "Heart Rate (bpm) \n") +
  theme_classic()


## Our box plots seem to suggest that the heart rate is highest in the warm temperature
## and lowest in the cold temperature. What do the numbers say, though?

### Calculating the mean and standard error for each group
## There are MANY different ways to do this but
## We can use the aggregate() function to quickly provide us our summary statistics

## Calculate the mean values for each Temperature group
HeartRate_mean <- aggregate(df$HeartRate_bpm,list("Temperature" = df$Temperature_Group), function(x) mean(x))

HeartRate_mean

## Calculate the standard error for each Temperature group
## To do this, we have to program a function for the equation to calculate standard error
## Standard error is based on standard deviation (sd() function) and the sample size.
## Luckily, we calculate the sample size for each Temperature Group using the NumberOfGroups variable earlier. 
## Why didn't we just type in 20 into our code? Because what if our sample size changed? 
## We would have to remember to change "20" in our code. Instead, our code retrieves that information from the
## dataset automatically, we actually don't have to change our code later!
HeartRate_se <- aggregate(df$HeartRate_bpm,list("Temperature" = df$Temperature_Group), function(x) sd(x)/sqrt(NumberOfGroups))

HeartRate_se

## Our mean values help to confirm our observations from the graphs. 

## However, how do we then statistically test some of these hypotheses? 

#### Statistics ####
## Please note: it is the responsibility of the instructor and student to 
## become familiar with statistical techniques and the assumptions underlying each test.
## This information should ideally be introduced prior to this activity.
## Also, the following code is only using the simplest options for the statistical analyses (e.g., two-sided p-values)
## Users are encouraged to explore further to address more complex questions!

#### ANOVA ####
## What if we wanted to test whether these mean values were different between the three temperatures?
## Our null is that there is no difference in heart rate between the water temperatures.
## Additional information can be found here: http://www.sthda.com/english/wiki/one-way-anova-test-in-r

HeartRate_ANOVA <- aov(HeartRate_bpm ~ Temperature_Group, data = df)

## Let's look at the results
HeartRate_ANOVA

## But what... where's our p-value?!
## We have to use summary() to see that information
summary(HeartRate_ANOVA)

## What can be conclude? Only that we can reject our null hypothesis and accept the alternative hypothesis
## that the means are different between the three groups. 
## Can we conclude that the warmer temperature is higher than the cold temperature? No.
## Can we conclude that the normal temperature is lower than the warm temperature? No.
## We would need post-hoc pair-wise comparison tests to make those conclusions. 


#### Linear regression ####
## It looks like there is a linear increase in heart rate as temperature increases but can we test that? Yes!
## Our null is that there is no linear relationship between heart rate and temperature. 
## Linear regressions require two continuous variables, so we'll want to use our Temperature_Celsius column rather than Temperature_Group

HeartRate_lm <- lm(HeartRate_bpm ~ Temperature_Celsius, data = df)

## Again, this only gives us limited information
HeartRate_lm

## Most of the useful information is revealed with summary()
summary(HeartRate_lm)

## Looks like we can reject our null hypothesis and accept our alternative hypothesis that temperature is a good predictor of heart rate


#### Correlation test ####
## What if we wanted to test how strong that relationship between heart rate and temperature was?
## The null hypothesis would be that there is no significant linear relationship between heart rate and temperature.
## Additional information can be found here: http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

HeartRate_cor <- cor.test(x = df$Temperature_Celsius, y = df$HeartRate_bpm)  

HeartRate_cor

## Looks like we can reject our null hypothesis and accept our alternative hypothesis that there is a 
## significant linear relationship between temperature and heart rate. 

## What is the correlation value? 
## some of the values are hidden within the variable but we can find them by using the names() function
names(HeartRate_cor)

## The correlation value is hidden under "estimate", so we can display it by typing:
HeartRate_cor$estimate

## The strongest correlations are closer to 1.0. The interpretation of whether 0.5 is considered "strong" 
## varies by discipline. Some ecologists are luckily to see a correlation of 0.3 since variation can be really high
## in more complex datasets, so 0.50 may not be too bad! Others would interpret 0.5 to be "moderate".



#### Thermal sensitivity ####
## Is the heart rate of the shrimp more sensitive to colder or warmer temperatures?
## We can assess that using Q10. 
## We will compare the mean values of each Temperature Group. 
## Future experiments could expose individual shrimp to all three water temperatures
## to calculate a Q10 value for each individual but repeated measures tests or mixed effects models
## would need to be incorporated to account for the multiple sampling within an individual (i.e., no longer independent samples)

## The mean heart rates can be extracted from our variable: HeartRate_mean
## We'll treat R1 (rate 1) as the normal temperature and R2 as the temperature we're comparing the normal temperature to.

## The normal temperature heart rate is the second row and and second column of HeartRate_mean, which we can access by typing:
HeartRate_mean[2,2]

## The cold temperature heart rate is the first row and second column:
HeartRate_mean[1,2]

## And the warm temperature heart rate is the third row and second column:
HeartRate_mean[3,2]

## The normal temperature is the first unique value in our Temperature Celsius variable: 
unique(df$Temperature_Celsius)[1]

## The cold temperature is the second unique value in our Temperature Celsius variable
unique(df$Temperature_Celsius)[2]

## The warm temperature is the third unique value in our Temperature Celsius variable
unique(df$Temperature_Celsius)[3]


HeartRate_Q10_cold <- (HeartRate_mean[1,2]/HeartRate_mean[2,2])^(10/(unique(df$Temperature_Celsius)[2] - unique(df$Temperature_Celsius)[1]))

HeartRate_Q10_warm <- (HeartRate_mean[3,2]/HeartRate_mean[2,2])^(10/(unique(df$Temperature_Celsius)[3] - unique(df$Temperature_Celsius)[1]))

