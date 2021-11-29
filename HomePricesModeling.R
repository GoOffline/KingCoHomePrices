

# Getting Started------------------------------------------------------------------------------------------------------------------------------------------

#Setting the Working Directory...................................................................................................................................
setwd("//Users//alexandra//Desktop//Super Senior Year//BUAN 4310 01 Data Mining and Big Data")
#Load in the relevant libraries.......................................................................................................................................

#Reading and Loading in the Data...........................................................................................................................................
mydata <- read.csv("houseprice.csv",header = TRUE) #Reads and loads in the data set
#Add calculated columns
mydata$Age_of_Home <- mydata$Year - mydata$yr_built
mydata$old <- ifelse(mydata$Age_of_Home > 25,1,0)
mydata$Has_Basement <- ifelse(mydata$sqft_basement != 0,1,0)
# 47.590627, -122.253413  Point of reference for computing geographic location
mydata$Eastside <- ifelse(mydata$long > -122.253413,1,0) #Long coordinate used -122.253413
mydata$Northern_KC <- ifelse(mydata$lat > 47.590627,1,0) # Lat coordinate used # 47.590627
mydata$Avg_construction_design <- ifelse(mydata$grade == 7| mydata$grade ==8 |mydata$grade ==9| mydata$grade ==10, 1,0)
mydata$poor_construction_design <- ifelse(mydata$grade ==6| mydata$grade ==5 | mydata$grade == 4,1,0)
mydata$unconstructed_design <- ifelse(mydata$grade ==3| mydata$grade == 2| mydata$grade == 1,1,0) #the one dummy variable left out is ecxcellent quality construction design
mydata$badview <-ifelse(mydata$view ==0| mydata$view ==1,1,0)
mydata$acceptableview <- ifelse(mydata$view == 2| mydata$view == 3,1,0) #Leaving out the variable excellent view with a score of 4
mydata$sqft_yard <-mydata$sqft_lot - mydata$sqft_above
mydata$renovated <-ifelse(mydata$yr_renovated > 0, 1, 0)


nrow(mydata) #12,093 rows
colnames(mydata)
boxplot(mydata$bedrooms, plot=FALSE)$out
outliers <- boxplot(mydata$bedrooms, plot=FALSE)$out
x<-mydata
x<- x[-which(x$bedrooms %in% outliers),]
mydata <- x
nrow(mydata) #After removing outliers in bedrooms, we now are left with 11,794 rows

boxplot(mydata$bathrooms, plot=FALSE)$out
outliers <- boxplot(mydata$bathrooms, plot=FALSE)$out
x<-mydata
x<- x[-which(x$bathrooms %in% outliers),]
mydata <- x
nrow(mydata) # After removing bathroom outliers, we are left with 11,550 rows

boxplot(mydata$sqft_living, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_living, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_living %in% outliers),]
mydata <- x
nrow(mydata) # After removing sqft_living outliers, we are left with 11,346 rows

boxplot(mydata$sqft_lot, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_lot, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_lot %in% outliers),]
mydata <- x
nrow(mydata) # After removing sqft_lot outliers, we are left with 10,157 rows


boxplot(mydata$sqft_above, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_above, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_above %in% outliers),]
mydata <- x
nrow(mydata) #After removing outliers for sqft_above, there are 9,912 rows

boxplot(mydata$sqft_basement, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_basement, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_basement %in% outliers),]
mydata <- x
nrow(mydata) #After removing outliers for sqft_basement, there are 9,750


boxplot(mydata$sqft_living15, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_living15, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_living15 %in% outliers),]
mydata <- x
nrow(mydata) #After removing outliers for yr_renovated, there are 9,627 rows

boxplot(mydata$sqft_lot15, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_lot15, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_lot15 %in% outliers),]
mydata <- x
nrow(mydata) #After removing outliers for sqft_lot15, there are 9,390

# We decided to only remove outliers for things specific to an average house in King County. We did not remove outliers for the day of the week, condition(1-5 scale), grade (1-5 scale), view (1-4) or dummy variables. We also did not remove outliers for zipcode, lat, or long since all of these houses are located in King County and satisfy that requirement 

# Part 2 Training and Validation Split -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#set the seed using our fav number
set.seed(666)
# create the indices for the split. This samples the row indices to split the data into training (60%) and validation (40%)
train_index <- sample(1:nrow(mydata), 0.6 * nrow(mydata))
valid_index <- setdiff(1:nrow(mydata), train_index)
# using the indices, create the training and validation sets, in essence, splitting a data frame by row
train_df_st <- mydata[train_index,]
valid_df_st <-mydata[valid_index,]
#Check the rows after splitting
nrow(train_df_st) #5,634 rows of data
nrow(valid_df_st) #3,756 rows of data

#PART 3: Building the model ONLY on the training set-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
price_lm <- lm(price ~ ., data = train_df_st) #The original model has an adjusted R-squared of 0.6954, p-value: < 0.00000000000000022
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(price_lm)

#Use BACKWARD elimination for reducing the predictors in housing price.....................................................................................................................................................................
price_lm1 <- step(price_lm, direction = "backward")
summary(price_lm_step_b) #The adjusted R-squared is 0.6994. p-value: < 0.00000000000000022. Variables included in this model are:

#Use forward elimation for reducing the predictors in the housing price 
#use step() to run forward selection 
#price_lm_step_f <- step(price_lm, scope = list(lower = price_lm, upper = price_lm), direction = "forward") 
#summary(price_lm_step_f) # The adjusted R-squared is 0.6992. p-value: < 0.00000000000000022


#Use both stepwise regression for reducing predictors in the housing price 
#price_lm_step_bf <- step(price_lm, direction = "both")
#summary(price_lm) # The adjusted R-squared is 0.6992. p-value: < 0.00000000000000022

#We will utilize the bacward elimination since it has the highest adjusted R-squared. We need to evaluate the soundness of the model for multi-colineraity, and if it logically makes sense
# First we will focus on the variables that are significant at the 5% level. 

# Using information we found from the backwards regression, at the five percent significance level, we will include all of those variables before narrowing it down further
price_lm2 <- lm(price~ Year + bathrooms +sqft_living +sqft_lot + waterfront + view + condition + grade + sqft_above +yr_built + yr_renovated + zipcode + lat + sqft_living15 +sqft_lot15 + Has_Basement + Northern_KC + Avg_construction_design +poor_construction_design + acceptableview + renovated, data = train_df_st)
summary(price_lm2) # In this first model, we were only acessing for significance at the 5% level. In our next model, we will need to remove variables that obviously have multi-collinearity. Adjusted R-squared is 0.6985

price_lm3 <- lm(price~ Year + bathrooms +sqft_living +sqft_lot + waterfront + view + condition + grade + sqft_above +yr_built + yr_renovated + zipcode + lat + sqft_living15 +sqft_lot15 + Has_Basement + Northern_KC + Avg_construction_design +poor_construction_design +renovated, data = train_df_st)
summary(price_lm3) #We deleted acceptable view, since it was no longer significant at the 5% level. There was also multi-collinearity between the variables view, and acceptable view. So even though our adjusted R-squared is now a bit lower (0.698) it makes more logical sense

price_lm4 <- lm(price~ Year + yr_built + bathrooms +sqft_living +sqft_lot + waterfront + view + condition + grade + sqft_above + zipcode + lat + sqft_living15 +sqft_lot15 + Has_Basement + Northern_KC + Avg_construction_design +poor_construction_design +renovated, data = train_df_st)
summary(price_lm4) #Looking at year and yr_built. Keeping them in the model adjusted r-squared is .698. Deleting them, and substiuting it with old makes the adjusted r-squared .6727. This means that the actual age of the house (rather than a proxy like "OLD") offers value to the model. 
# We will also test yr_renovated, and renovated to see which one is more appropriate for the model. Including only the dummy variable yields an adjusted R squared of .6958. Only including yr_renovated provides an adjusted r-squared of .6958. This means that being renovated or not is a bigger deal, than as to when the house got renovated. We will keep renovated the dummy variable in the model. 
  
price_lm5 <- lm(price~ Year + yr_built + bathrooms  +sqft_lot + waterfront + view + condition + grade + sqft_above + lat + sqft_living15 +sqft_lot15 + Has_Basement + Northern_KC + Avg_construction_design +poor_construction_design +renovated, data = train_df_st)
summary(price_lm5) # We are going to remove zipcode since its coded incorrectly (integer rather than categorical). We are removing sqft_living and keeping with sqft_above. There is multi-collinearity between sqft_living and has_basement. our new adjusted r-squared is .6921

price_lm6 <- lm(price~ Year + yr_built + bathrooms  +sqft_lot  + waterfront + view + condition + grade + sqft_above + sqft_living15 +sqft_lot15 + Has_Basement +lat + Avg_construction_design +poor_construction_design +renovated + Eastside, data = train_df_st)
summary(price_lm6) # We need to decide whether to keep lat, long, or North or East.
                  # First we will remove lat and see how removing it affects the model( Removing negatively affects it -0.6698) . Then we will try replacing it with North and see if it improves the accuracy (it doesnt). So we will leave Lat in (.6893). Now we decide whether we want long or East.Adding East, caused the adjusted R-squared to go up to .6894

price_lm7 <- lm(price~ Year + yr_built + bathrooms  + sqft_lot  + waterfront + view + condition  + sqft_above + sqft_living15 +sqft_lot15 + Has_Basement +lat + grade +renovated + Eastside, data = train_df_st)
summary(price_lm7) #There is multi-collinearity with grade, avg_construction_design, poor_construction_design. We need to get rid of some variables
                  # First we will delete grade and see if it improves the fit of our model. Deleting grade made the adjusted r-squared go down to .6253. 
                  # We will add grade back in, and delete avg_construction_design, and poor _construction_design. Deleting them, and adding grade in, caused the adjusted R squared to go up to .6804

price_lm8 <- lm(price~ Year + yr_built + bathrooms  + sqft_yard + waterfront + view + condition  + sqft_above + sqft_living15 +sqft_lot15 + Has_Basement +lat + grade +renovated + Eastside, data = train_df_st)
summary(price_lm8) #sqft above gives us the amount of space the house takes up, sqft_lot incorporates the space of the house. We will use sqft_yard instead. It kept the adjusted r-sqaured at .6804

price_lm9<- lm(price~ Year + yr_built + bathrooms + bedrooms  + sqft_yard + sqft_above + Has_Basement  + waterfront + view  +  condition + grade + renovated + Eastside + lat + sqft_living15 +sqft_lot15, data = train_df_st)
summary(price_lm9) #adding in bedrooms caused the model's r squared to go slightly up to .6808.
#This is the best model we have. If we were to include sqft_living, we would have had multi-collinearity with sqft_above, sqft_yard, and sqft_lot. To still capture the necessary information, we dropped sqft_living, 
# and include sqft_above (capture the space the home actually takes up), Has_basement, and sqft_yard to capture extra lot space not being used by the house. We included sqft_living15 and sqft_lot15 since those are the measurements of k-15 nearest neighbors. We were not provided with basement (but thats ok since its included in the sqft_living15 metric)

#We first took a stab at the problem by trying to use logic. While it may not have yielded the best model, it was worth a try! See appendix for those models and calculations...


# Before we test the models on the testing set, let's consider what metrics we will evaluate against 

summary(price_lm9) #The p-value for our model is p-value: < 0.00000000000000022. Our entire model except for three variables were significant at the *** level















##Appendix................................................................................................................
#Models made off of logic from prior-housing experience.......................................................................................
price_lm3 <- lm(price ~ Year + bedrooms + bathrooms + sqft_lot + waterfront + view + condition + zipcode, data = train_df_st) 
summary(price_lm3) #Adjusted R-squared: 0.244 ,p-value: < 0.00000000000000022

# Bedrooms - the size of the home for this model will use bedrooms to determine price 
# Bathrooms - again, this will be used for size not sqft to not have milticollinearity 
# sqft_lot - the size of the lot will factor into price, variable to home size 
# Waterfront - this variable has a large determining factor on price 
# Condition - if the home is in good condition will factor into price 
# zipcode - price reflected in zip codes may show neighborhood differences 


price_lm4 <-lm(price ~ Year + sqft_living + sqft_lot + waterfront + view + grade + zipcode, data = train_df_st) 
summary(price_lm4) #Adjusted R-squared: 0.4589 ,p-value: < 0.00000000000000022

#Month - Real Estate markets eb and flow over the year and have busy seasons 
# sqft_living - this will be the size determinate for this model 
# sqft_lot - the size of the lot will factor into price, variable to home size 
# view - this variable has a large determining factor on price 
# grade - for this model, instead of condition we will look at overall grade 
# yr_renovated - if a renovation was done this will impact home price 
# zipcode - price reflected in zip codes may show neighborhood differences 



price_lm5 <-lm(price ~ bedrooms + bathrooms + sqft_lot + waterfront + condition + zipcode + yr_built, data = train_df_st)
summary(price_lm5) #Adjusted R-squared is 0.26.
# day_of_week - in typical markets there are review dates, and at times this can be a higher price 
# Bedrooms - the size of the home for this model will use bedrooms to determine price 
# Bathrooms - again, this will be used for size not sqft to not have milticollinearity 
# sqft_lot - the size of the lot will factor into price, variable to home size 
# Waterfront - this variable has a large determining factor on price 
# Condition - if the home is in good condition will factor into price 
# zipcode - price reflected in zip codes may show neighborhood differences 
# yr_built - variable is important to price, expectation is that newer the home, higher price   

# Zipcode as well as lat/long could be correlated
price_lm6 <-lm(price ~ Year + bedrooms + bathrooms + waterfront + view + grade + sqft_above + yr_built + lat + long , data = train_df_st) 
summary(price_lm6) #R-squared goes down to .6657. Zipcode is less influential than the latitude and longitude 
=======


# Getting Started------------------------------------------------------------------------------------------------------------------------------------------

#Setting the Working Directory...................................................................................................................................
setwd("C:/Users/chan/Documents/R/Buan 4310 Data mining")
#Load in the relevant libraries.......................................................................................................................................

#Reading and Loading in the Data...........................................................................................................................................
mydata <- read.csv("house_4.csv",header = TRUE) #Reads and loads in the data set
#Add calculated columns
mydata$Age_of_Home <- mydata$Year - mydata$yr_built
mydata$old <- ifelse(mydata$Age_of_Home > 25,1,0)
mydata$Has_Basement <- ifelse(mydata$sqft_basement != 0,1,0)
# 47.590627, -122.253413  Point of reference for computing geographic location
mydata$Eastside <- ifelse(mydata$long > -122.253413,1,0) #Long coordinate used -122.253413
mydata$Northern_KC <- ifelse(mydata$lat > 47.590627,1,0) # Lat coordinate used # 47.590627
mydata$Avg_construction_design <- ifelse(mydata$grade == 7| mydata$grade ==8 |mydata$grade ==9| mydata$grade ==10, 1,0)
mydata$poor_construction_design <- ifelse(mydata$grade ==6| mydata$grade ==5 | mydata$grade == 4,1,0)
mydata$unconstructed_design <- ifelse(mydata$grade ==3| mydata$grade == 2| mydata$grade == 1,1,0) #the one dummy variable left out is ecxcellent quality construction design
mydata$badview <-ifelse(mydata$view ==0| mydata$view ==1,1,0)
mydata$acceptableview <- ifelse(mydata$view == 2| mydata$view == 3,1,0) #Leaving out the variable excellent view with a score of 4
mydata$renovated <-ifelse(mydata$yr_renovated > 0, 1, 0)


nrow(mydata) #12,093 rows
colnames(mydata)
boxplot(mydata$bedrooms, plot=FALSE)$out
outliers <- boxplot(mydata$bedrooms, plot=FALSE)$out
x<-mydata
x<- x[-which(x$bedrooms %in% outliers),]
mydata <- x
nrow(mydata) #After removing outliers in bedrooms, we now are left with 11,794 rows

boxplot(mydata$bathrooms, plot=FALSE)$out
outliers <- boxplot(mydata$bathrooms, plot=FALSE)$out
x<-mydata
x<- x[-which(x$bathrooms %in% outliers),]
mydata <- x
nrow(mydata) # After removing bathroom outliers, we are left with 11,550 rows

boxplot(mydata$sqft_living, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_living, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_living %in% outliers),]
mydata <- x
nrow(mydata) # After removing sqft_living outliers, we are left with 11,346 rows

boxplot(mydata$sqft_lot, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_lot, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_lot %in% outliers),]
mydata <- x
nrow(mydata) # After removing sqft_lot outliers, we are left with 10,157 rows


boxplot(mydata$sqft_above, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_above, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_above %in% outliers),]
mydata <- x
nrow(mydata) #After removing outliers for sqft_above, there are 9,912 rows

boxplot(mydata$sqft_basement, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_basement, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_basement %in% outliers),]
mydata <- x
nrow(mydata) #After removing outliers for sqft_basement, there are 9,750


boxplot(mydata$sqft_living15, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_living15, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_living15 %in% outliers),]
mydata <- x
nrow(mydata) #After removing outliers for yr_renovated, there are 9,627 rows

boxplot(mydata$sqft_lot15, plot=FALSE)$out
outliers <- boxplot(mydata$sqft_lot15, plot=FALSE)$out
x<-mydata
x<- x[-which(x$sqft_lot15 %in% outliers),]
mydata <- x
nrow(mydata) #After removing outliers for sqft_lot15, there are 9,390

# We decided to only remove outliers for things specific to an average house in King County. We did not remove outliers for the day of the week, condition(1-5 scale), grade (1-5 scale), view (1-4) or dummy variables. We also did not remove outliers for zipcode, lat, or long since all of these houses are located in King County and satisfy that requirement 

# Part 2 Training and Validation Split -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#set the seed using our fav number
set.seed(666)
# create the indices for the split. This samples the row indices to split the data into training (60%) and validation (40%)
train_index <- sample(1:nrow(mydata), 0.6 * nrow(mydata))
valid_index <- setdiff(1:nrow(mydata), train_index)
# using the indices, create the training and validation sets, in essence, splitting a data frame by row
train_df_st <- mydata[train_index,]
valid_df_st <-mydata[valid_index,]
#Check the rows after splitting
nrow(train_df_st) #5,634 rows of data
nrow(valid_df_st) #3,756 rows of data

#PART 3: Building the model ONLY on the training set-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
price_lm <- lm(price ~ ., data = train_df_st) #The original model has an adjusted R-squared of 0.6954, p-value: < 0.00000000000000022
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(price_lm)

#Use BACKWARD elimination for reducing the predictors in housing price.....................................................................................................................................................................
price_lm1 <- step(price_lm, direction = "backward")
summary(price_lm1) #The adjusted R-squared is 0.6994. p-value: < 0.00000000000000022. 

#Use forward elimation for reducing the predictors in the housing price 
#use step() to run forward selection 
#price_lm_step_f <- step(price_lm, scope = list(lower = price_lm, upper = price_lm), direction = "forward") 
#summary(price_lm_step_f) # The adjusted R-squared is 0.6992. p-value: < 0.00000000000000022


#Use both stepwise regression for reducing predictors in the housing price 
#price_lm_step_bf <- step(price_lm, direction = "both")
#summary(price_lm) # The adjusted R-squared is 0.6992. p-value: < 0.00000000000000022

#We will utilize the bacward elimination since it has the highest adjusted R-squared. We need to evaluate the soundness of the model for multi-colineraity, and if it logically makes sense
# First we will focus on the variables that are significant at the 5% level. 

# Using information we found from the backwards regression, at the five percent significance level, we will include all of those variables before narrowing it down further
price_lm2 <- lm(price~ Year + bathrooms +sqft_living +sqft_lot + waterfront + view + condition + grade + sqft_above +yr_built + yr_renovated + zipcode + lat + sqft_living15 +sqft_lot15 + Has_Basement + Northern_KC + Avg_construction_design +poor_construction_design + acceptableview + renovated, data = train_df_st)
summary(price_lm2) # In this first model, we were only acessing for significance at the 5% level. In our next model, we will need to remove variables that obviously have multi-collinearity. Adjusted R-squared is 0.6985

price_lm3 <- lm(price~ Year + bathrooms +sqft_living +sqft_lot + waterfront + view + condition + grade + sqft_above +yr_built + yr_renovated + zipcode + lat + sqft_living15 +sqft_lot15 + Has_Basement + Northern_KC + Avg_construction_design +poor_construction_design +renovated, data = train_df_st)
summary(price_lm3) #We deleted acceptable view, since it was no longer significant at the 5% level. There was also multi-collinearity between the variables view, and acceptable view. So even though our adjusted R-squared is now a bit lower (0.698) it makes more logical sense

price_lm4 <- lm(price~ Year + yr_built + bathrooms +sqft_living +sqft_lot + waterfront + view + condition + grade + sqft_above + zipcode + lat + sqft_living15 +sqft_lot15 + Has_Basement + Northern_KC + Avg_construction_design +poor_construction_design +renovated, data = train_df_st)
summary(price_lm4) #Looking at year and yr_built. Keeping them in the model adjusted r-squared is .698. Deleting them, and substiuting it with old makes the adjusted r-squared .6727. This means that the actual age of the house (rather than a proxy like "OLD") offers value to the model. 
# We will also test yr_renovated, and renovated to see which one is more appropriate for the model. Including only the dummy variable yields an adjusted R squared of .6958. Only including yr_renovated provides an adjusted r-squared of .6958. This means that being renovated or not is a bigger deal, than as to when the house got renovated. We will keep renovated the dummy variable in the model. 

price_lm5 <- lm(price~ Year + yr_built + bathrooms  +sqft_lot + waterfront + view + condition + grade + sqft_above + lat + sqft_living15 +sqft_lot15 + Has_Basement + Northern_KC + Avg_construction_design +poor_construction_design +renovated, data = train_df_st)
summary(price_lm5) # We are going to remove zipcode since its coded incorrectly (integer rather than categorical). We are removing sqft_living and keeping with sqft_above. There is multi-collinearity between sqft_living and has_basement. our new adjusted r-squared is .6921

price_lm6 <- lm(price~ Year + yr_built + bathrooms  +sqft_lot  + waterfront + view + condition + grade + sqft_above + sqft_living15 +sqft_lot15 + Has_Basement +lat + Avg_construction_design +poor_construction_design +renovated + Eastside, data = train_df_st)
summary(price_lm6) # We need to decide whether to keep lat, long, or North or East.
# First we will remove lat and see how removing it affects the model( Removing negatively affects it -0.6698) . Then we will try replacing it with North and see if it improves the accuracy (it doesnt). So we will leave Lat in (.6893). Now we decide whether we want long or East.Adding East, caused the adjusted R-squared to go up to .6894

price_lm7 <- lm(price~ Year + yr_built + bathrooms  + sqft_lot  + waterfront + view + condition  + sqft_above + sqft_living15 +sqft_lot15 + Has_Basement +lat + grade +renovated + Eastside, data = train_df_st)
summary(price_lm7) #There is multi-collinearity with grade, avg_construction_design, poor_construction_design. We need to get rid of some variables
# First we will delete grade and see if it improves the fit of our model. Deleting grade made the adjusted r-squared go down to .6253. 
# We will add grade back in, and delete avg_construction_design, and poor _construction_design. Deleting them, and adding grade in, caused the adjusted R squared to go up to .6804

price_lm8 <- lm(price~ Year + yr_built + bathrooms  + waterfront + view + condition  + sqft_above + sqft_living15 +sqft_lot15 + Has_Basement +lat + grade +renovated + Eastside, data = train_df_st)
summary(price_lm8) #sqft above gives us the amount of space the house takes up, sqft_lot incorporates the space of the house. It kept the adjusted r-sqaured at .6798

price_lm9<- lm(price~ Year + yr_built + bathrooms + bedrooms + sqft_above + Has_Basement  + waterfront + view  +  condition + grade + renovated + Eastside + lat + sqft_living15 +sqft_lot15, data = train_df_st)
summary(price_lm9) #adding in bedrooms caused the model's r squared to go slightly up to .6803.

#This is the best model we have. If we were to include sqft_living, we would have had multi-collinearity with sqft_above, and sqft_lot. To still capture the necessary information, we dropped sqft_living, 
# and include sqft_above (capture the space the home actually takes up), Has_basement to capture extra lot space not being used by the house. We included sqft_living15 and sqft_lot15 since those are the measurements of k-15 nearest neighbors. We were not provided with basement (but thats ok since its included in the sqft_living15 metric)

#We first took a stab at the problem by trying to use logic. While it may not have yielded the best model, it was worth a try! See appendix for those models and calculations...

# Before we test the models on the testing set, let's consider what metrics we will evaluate against 

summary(price_lm9) #The p-value for our model is p-value: < 0.00000000000000022. Our entire model except for three variables were significant at the *** level

# Part 5 Diagnostics -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Test Multicollinearity for model price_lm9
# If no factors are correlated, the VIFs will all be 1.
# Rule of thumb: If VIF > 10, mullticollinearity is high.

library(car)
vif(price_lm9) #  sqft_above 4.052272, there is a concern between these variables about mullticollinearity. so we remove sqft_above and then retest the mullticollinearity 

price_lm10<- lm(price~ Year + yr_built + bathrooms + bedrooms  + Has_Basement  + waterfront + view  +  condition + grade + renovated + Eastside + lat + sqft_living15 + sqft_lot15, data = train_df_st)
summary(price_lm10) # Adjusted R-squared:  0.6632 ,  p-value: < 0.00000000000000022

vif(price_lm10) # Now there is no concern for multicollinearity anymore. We think the best model is price_lm10

# Heteroskedasticity test
# Perform a Breusch-Pagan Test to test for heteroskedasticity/homoskedasticity
library(lmtest)
bptest(price_lm10)

plot(price_lm10, 1)
# Heteroskedasticity increases the likelihood that the coefficient estimates are further from the correct population value. Heteroscedasticity tends to produce p-values that are smaller than they should be

# log transformation.
# Here, the predictors are transformed using a log transformation.
price_lm10_log <- lm(log(price) ~ Year + yr_built + log(bathrooms) + log(bedrooms)  + Has_Basement + waterfront + view  +log(condition) + log(grade)  + renovated + Eastside + log(lat) + log(sqft_living15) + log(sqft_lot15), data = train_df_st)
summary(price_lm10_log)
# Adjusted R-squared:  0.7132 , p-value: < 0.00000000000000022

bptest(price_lm10_log)
plot(price_lm10_log, 1)
# From this test we assume that the error terms are normally distribute

# Part 4 Predicting -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Predict the outcome (i.e. price) of the validation set using the model from the training set.

library(forecast)
price_lm10_log_pred_train <- predict(price_lm10_log,
                                      train_df_st)
price_lm10_log_pred_valid <- predict(price_lm10_log,
                                valid_df_st)

# Compare the errors between the training and validation sets.
# perform diagnostics on the model. This includes the various assumptions on a regression.

accuracy(price_lm10_log_pred_train, train_df_st$price) # RMSE 511191.3
accuracy(price_lm10_log_pred_valid, valid_df_st$price) # RMSE 525992.1
max(mydata$price) - min(mydata$price) # 2219000
sd(mydata$price) #  226097.4

price_valid_lm10_log <- lm(price ~ Year + yr_built + log(bathrooms) + log(bedrooms)  + Has_Basement + waterfront + view  +log(condition) + log(grade)  + renovated + Eastside + log(lat) + log(sqft_living15) + log(sqft_lot15), data = valid_df_st)
summary(price_valid_lm10_log) # Adjusted R-squared:  0.7012 ,  p-value: < 0.00000000000000022


# Import new customer data set 

housenew <- read.csv("house_test_4.csv", header = TRUE)

# We need to add some variables to the new dataset

housenew$Age_of_Home <- housenew$Year - housenew$yr_built
housenew$Has_Basement <- ifelse(housenew$sqft_basement != 0,1,0)
# 47.590627, -122.253413  Point of reference for computing geographic location
housenew$Eastside <- ifelse(housenew$long > -122.253413,1,0) #Long coordinate used -122.253413
housenew$Northern_KC <- ifelse(housenew$lat > 47.590627,1,0) # Lat coordinate used # 47.590627
housenew$Avg_construction_design <- ifelse(housenew$grade == 7| housenew$grade ==8 |housenew$grade ==9| housenew$grade ==10, 1,0)
housenew$renovated <-ifelse(housenew$yr_renovated > 0, 1, 0)

#Predict new price using DataFrame1
housenew_predict <- predict(price_lm10_log, newdata = housenew)
housenew_predict









##Appendix................................................................................................................
#Models made off of logic from prior-housing experience.......................................................................................
price_lm3 <- lm(price ~ Year + bedrooms + bathrooms + sqft_lot + waterfront + view + condition + zipcode, data = train_df_st) 
summary(price_lm3) #Adjusted R-squared: 0.244 ,p-value: < 0.00000000000000022

# Bedrooms - the size of the home for this model will use bedrooms to determine price 
# Bathrooms - again, this will be used for size not sqft to not have milticollinearity 
# sqft_lot - the size of the lot will factor into price, variable to home size 
# Waterfront - this variable has a large determining factor on price 
# Condition - if the home is in good condition will factor into price 
# zipcode - price reflected in zip codes may show neighborhood differences 


price_lm4 <-lm(price ~ Year + sqft_living + sqft_lot + waterfront + view + grade + zipcode, data = train_df_st) 
summary(price_lm4) #Adjusted R-squared: 0.4589 ,p-value: < 0.00000000000000022

#Month - Real Estate markets eb and flow over the year and have busy seasons 
# sqft_living - this will be the size determinate for this model 
# sqft_lot - the size of the lot will factor into price, variable to home size 
# view - this variable has a large determining factor on price 
# grade - for this model, instead of condition we will look at overall grade 
# yr_renovated - if a renovation was done this will impact home price 
# zipcode - price reflected in zip codes may show neighborhood differences 



price_lm5 <-lm(price ~ bedrooms + bathrooms + sqft_lot + waterfront + condition + zipcode + yr_built, data = train_df_st)
summary(price_lm5) #Adjusted R-squared is 0.26.
# day_of_week - in typical markets there are review dates, and at times this can be a higher price 
# Bedrooms - the size of the home for this model will use bedrooms to determine price 
# Bathrooms - again, this will be used for size not sqft to not have milticollinearity 
# sqft_lot - the size of the lot will factor into price, variable to home size 
# Waterfront - this variable has a large determining factor on price 
# Condition - if the home is in good condition will factor into price 
# zipcode - price reflected in zip codes may show neighborhood differences 
# yr_built - variable is important to price, expectation is that newer the home, higher price   

# Zipcode as well as lat/long could be correlated
price_lm6 <-lm(price ~ Year + bedrooms + bathrooms + waterfront + view + grade + sqft_above + yr_built + lat + long , data = train_df_st) 
summary(price_lm6) #R-squared goes down to .6657. Zipcode is less influential than the latitude and longitude 
>>>>>>> main
