#King County Housing Prices Modeling Project 

#The dataset (houses4.csv) includes home prices in King County. 
# The corresponding documentation (Real Estate in King County documentation.docx) 
  # provides the variable definitions.  
# Using various data mining techniques, we will explore the data and build a 
  #suitable model to help Jacob determine what predicts home prices in King County.

# We will build and select an appropriate model for the company and predict the 
# price (i.e. the target variable) of the new houses given in the sample set. 

#Model 1 
  # Variables to consider: 
    # Month - Real Estate markets eb and flow over the year and have busy seasons 
    # Bedrooms - the size of the home for this model will use bedrooms to determine price 
    # Bathrooms - again, this will be used for size not sqft to not have milticollinearity 
    # sqft_lot - the size of the lot will factor into price, variable to home size 
    # Waterfront - this variable has a large determing factor on price 
    # Condition - if the home is in good condition will factor into price 
    # zipcode - price reflected in zip codes may show neighborhodd differences 

#Model 2 
  # Variables to consider: 
    # Month - Real Estate markets eb and flow over the year and have busy seasons 
    # sqft_living - this will be the size determinatne for this model 
    # sqft_lot - the size of the lot will factor into price, variable to home size 
    # Waterfront - this variable has a large determing factor on price 
    # grade - for this model, instead of condition we will look at overall grade 
    # yr_renovated - if a renovation was done this will impact home price 
    # HasBeenViews - the popularity of the home

#Model 3
  # Variables to consider: 
    # day_of_week - in typical markets there are review dates, and at times this can be a higher price 
    # Bedrooms - the size of the home for this model will use bedrooms to determine price 
    # Bathrooms - again, this will be used for size not sqft to not have milticollinearity 
    # sqft_lot - the size of the lot will factor into price, variable to home size 
    # Waterfront - this variable has a large determing factor on price 
    # Condition - if the home is in good condition will factor into price 
    # yr_built - variable is important to price, expectation is that newer the home, higher price 



