###############################################################################
######################### MIXEDPOWER PIPELINE ################################
###############################################################################


# ----------------------------------------------------------------------------#
# This is a pipeline that helps you to set up your own power analysis. Have fun!


####### How to use this file: ##########

# 1. Save this script in a folder
# --> call it powersimulation or something similar ;)

# 2. Also save your pilot data (or a copy of it) in this exact folder 

# 3. Follow the steps in this Script and enter all requested information

# 4. Run the whole script! (And wait... maybe get a coffe)

# ----------------------------------------------------------------------------#




###############################################################################
# 1. SETUP
# --> Nothing to do for you here ;) 

rm(list = ls()) # clear environment
library(mixedpower) # get package
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set wd to folder

###############################################################################


# Okay, lets start...

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# STEP 1: Pilot data analysis and model fitting 

# --> insert your pilot data analysis here!
# --> Just do the same stuff you did anyways.At the end, we need a preprocessed
# data set and a corresponding fitted mixed model. 
# --> The power analysis is based on this model. So make sure this is your 
# final optimal model or one you are interested in. 


# ---------------------------- # 
## --> insert analysis here:




# Now I need to know how your model and data is called in your pilot data analyis
# --> just enter their names after the "<-" 
model_emp <- 
data_emp <- 

# ---------------------------- #


  
# Alright! Now... 
  
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# STEP 2: Prepare simulation and set all relevant parameters
  
  

# A: significance level to test against:
  # --> enter t or z value you want to test against 
  # --> t if lmer/ z if glmer
  # --> default value is 2, so you can just work with this
critical_value <- 2

# B: sample sizes 
  # --> for which sample sizes do you want to simulate power? 
  # --> enter a range of plausible sample sizes for your confirmatory study
  # --> e.g. c(10,20,30,40)
sample_sizes <- c(10, 20, 30, 40)


# C: confidence interval for safeguard power 
  # --> safeguard power computes a more conservative power to protect against
  # unprecise effect estimates in your pilot data 
  # --> Just tell me how conservative you want to be (0.68 == 1 SD)
confidence_level <- 0.68


# D: subject number variable
  # How is the "subject number" variable called in your pilot data set?
  # --> this needs to be a character element 
subvar = "subvar"


# E: fixed effects 
  # Which variables in your pilot data set are fixed effects in your model?
  # --> this needs to be a vector of character elements
  # --> just replace >> fixed effect 1 << with your variable name 
fixed_effects <- c(">> fixed effect 1 <<", ">> fixed effect 2 <<", "...")



# ----------------------------------------------------------------------------#
# --------------------------------------------------------------------------- #
# STEP 3: SIMULATE POWER, STORE AND SAVE OUTPUT

# That's it for now! This script does the work from here on..

# so..RUN the whole script! This will take about one hour (..or more.. or less)
# --> the output (plots, data frame with results) will be saved in your 
# powersimulation folder

power_output <- mixedpower(model_emp, data_emp, subvar, fixed_effects,
                   critical_value, sample_sizes, n_sim = 1000,
                   databased = T, safeguard = T, rnorm = T)


multiplotPower(power_output)

#######
# visit ?mixedpower to learn more about the databased, safeguard and rnorm 
# option
######
