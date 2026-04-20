In /questionnaire/codebook.csv, check how variables are defined, in particular sociodemographic variables.

Create code_robustness/mrp.R and work in it.

Using the package tidycensus, find the joint distribution of default predictor variables: age,  gender, family income, highest diploma (3 categories), race, US region, and size of agglomeration. 

Define a function that takes a dataset (with US as default), a variable of interest, a list of predictor variables (with the above list as the default value), and population distributions (with distribution from the US Census as the default), and returns the mean of the variable of interest estimated through Multilevel Regression and Poststratification.

Also find an extended list of predictor variables by adding to the default ones sociodemographic variables that are present both in the codebook and the Census/PUMS. Find the joint distribution for this extended list.

Apply the above function to:
- gcs_support
- gcs_support_control
- gcs_support with the extended list of predictor variables
- gcs_support_control with the extended list of predictor variables

NB: when you compute a joint distribution, weigh individuals using PWGTP from PUMS.



When you are done with this, download official (generally census) data and compute joint distribution for the default list of predictor variables for the following countries: FR, DE, IT, PL, SP, CH, GB, RU, SA, JP. Apply the MRP function to gcs_support and gcs_support_control on respective dataframes: FR, DE, IT, PL, SP, CH, GB, RU, SA, JP.

