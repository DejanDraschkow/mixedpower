######## utility functions


#-------------------------------------------------------------------------------------------------------#

#' Function to prepare a mixed model with varied fixed effect estimantes
#'
#' \code{prepare_rnorm_model()} changes the fixed effect estimates of the
#' specified model. It randomly selects "new" fixed effect estimates out of
#' a normal distribution around the "old" estimates. Non significant effects
#' in the given model are ignored and no rnorm-value is computet for them.
#'
#' @param model mixed model of interest
#' @param data_emp pilot data that fits the mixed model of interest
#' @param simvar name of the variable that contains the subject??s number
#' in data_emp
#' @param critical_value z/t value to test if a given fixed effect
#' is significant
#'
#' @return A modified mixed model
#'
#' @export
prepare_rnorm_model <- function(model_emp, data_emp, simvar, critical_value){

  # --------------------------------------------------------- #

  # 1. get fixed effects and their distribution

  # [-1, ] removes intercept --> we dont need it
  fixefs <- summary(model_emp)$coefficients[-1,]
  fixef_values <- fixefs[, "Estimate"]

  # SD = SE* sqrt(n)
  # get n to compute SD
  n <- get_n(data_emp, simvar)
  SD <- fixefs[, "Std. Error"] * sqrt(n)

  ### prepare ###
  # --> this is a TRUE FALSE vector
  significant_effects <- check_significance(model_emp, critical_value)



  # ---------- change model_emp ------------------------ #
  # 1. look at every effect
  for (i in 1:length(significant_effects)){

    # check if effect is significant:
    if(significant_effects[i] == TRUE){

      # change parameter to rnorm value
      # 1. get SD of current effect
      sd <- SD[[i]]

      # get current effect (value of it)
      effect <- fixef_values[i]

      # 2. construct normal distribution and randomly choose one observation
      # 3. change model to this random effect parameter
      # i +1 : because intercept is still in this @beta thing
      model_emp@beta[i + 1] <- rnorm(1, mean = effect, sd = sd)

    } else if(significant_effects[i] == FALSE){

      # inform user that old paramter is used to compute power
      non_significant_effect <- row.names(summary(model_emp)$coefficients)[i+1]
      cat(paste("WARNING:", non_significant_effect, "is not significant in model_emp. \n  rNorm Power cannot be computed for this effect\n"))

    } else {

      cat("WARNING: Something went wrong with the rnorm model")
    } # end if else
  } # end for

  ### return modified model == rnorm model
  model_emp
} # end function



#-------------------------------------------------------------------------------------------------------#
# PREPARE SAFEGUARD POWER


#' Function that computes confidence intervals for fixed effects and
#' sets them to the lower boundary of this CI
#'
#' \code{prepare_safeguard_model()} changes the fixed effect estimates of the
#' specified model. It computes confidence Intervals with the "Wald"- method
#' and sets the fixed effect estimates to the lower boundary of this confidence
#' interval. Non significant effects in the given model are ignored and
#' no safeguard-value is computet for them.
#'
#' @param model_emp mixed model of interest
#' @param confidence_level level of confidence that the parameter lies
#' in the interval
#' @param critical_value z/t value to test if a given fixed effect
#' is significant
#'
#' @return A modified mixed model
#'
#' @export
prepare_safeguard_model <- function(model_emp, confidence_level, critical_value){

  # ---------- prepare ------------------------ #
  # 1. check which effects are significant

  # --> this is a TRUE FALSE vector
  significant_effects <- check_significance(model_emp, critical_value)

  # 2. compute confidence intervals

  # things we want intervals for: (kick out those .sig01 stuff)
  effects <- row.names(summary(model_emp)$coefficients)[-1]
  confints <- confint(model_emp, method = "Wald",  level = confidence_level, parm = effects)


  # ---------- change model_emp ------------------------ #
  # 1. look at every effect
  for (i in 1:length(significant_effects)){

    # check if effect is significant:
    if(significant_effects[i] == TRUE){

      # change paramter to safeguard value

      # A. get safeguard effect: needs to be the one closer to 0
      #--> depends on effect of interest: negativ or positiv ?
      row <- i +1 # because there also is the intercept in coefficients that we want to skip
      safeguard_effect <- ifelse(summary(model_emp)$coefficients[row,1] > 0,
                                 confints[i,1],
                                 confints[i,2])

      # B. change model parameter of effect of interest to safeguard effect
      model_emp@beta[row] <- safeguard_effect

    } else if(significant_effects[i] == FALSE){

      # inform user that old paramter is used to compute power
      non_significant_effect <- row.names(summary(model_emp)$coefficients)[i+1]
      cat(paste("WARNING:", non_significant_effect, "is not significant in model_emp. \n  Safeguard Power cannot be computed for this effect\n"))

    } else {

      cat("WARNING: Something went wrong with the safeguard model")
    } # end if else
  } # end for


  # ---------- return safeguard model ------------------------ #
  model_emp
} # end function

#-------------------------------------------------------------------------------------------------------#
# RESET CONTRASTS FUNCTION

# simulateSamplesize loses contrasts --> rbind
# --> this function resets all contrasts from data_emp


#' Resets specified contrasts in the simulated dataset
#'
#' \code{reset_contrasts()} makes sure that contrasts in the simulated
#' dataset equals the contrasts in the pilotdata used for simulation.
#' Because the simulate_sanplesize function loses specified contrasts,
#' they need to be re-specified.
#' \code{reset_contrasts()} checks all contrasts on the fixed-effect-variables
#' in the pilot dataset and copies them to the simulated dataset.
#'
#' @param simulated_data simulated data set (= output of simulate_samplesize)
#' @param data_emp pilot data used for power simulation
#' @param model_emp mixed model used for power simulation.
#' Needs to fit data_emp.
#' @param fixed_effects names of variables in data_emp that are used as
#' fixed effects in model_emp
#'
#' @return Date frame
#'
#' @export
reset_contrasts <- function(simulated_data, data_emp, model_emp, fixed_effects ) {

  # --> 1. get names from all variables in model
  variable_names <- fixed_effects


  # --> 2. loop through names and reset contrasts for all involved variables
  for (variable in variable_names){

    # check if variable is a factor (contrasts only work with factors)
    if (is.factor(data_emp[[variable]]) == T){
      # get original contrast
      data_emp_contr <- contrasts(data_emp[[variable]])


      # apply original contrast to simulated data set
      contrasts(simulated_data[[variable]]) <- data_emp_contr
    } # end if
  } # end for loop

  # return simulated data set
  simulated_data
} # end function

#-------------------------------------------------------------------------------------------------------#

# CHECK SIGNIFICANCE

#' Gets t/z value out of model_emp and compares it to a ctritical value
#'
#' \code{check_significance()} checks the t/z value of all fixed effects and
#' compares them to the specified critical value. Returns a logical vector
#' indicating whether one effect is significant or not. TRUE == significant,
#' FALSE == not significant.
#'
#' @param model mixed model you want to check
#' @param critical_value t/z value you want to check against
#'
#' @return logical vector
#'
#' @export
check_significance <- function(model, critical_value){

  store_significance <- c()

  for (i in 1:length(fixef(model)[-1])){
    # -- get specified critical_value for current effect --- #
    if (length(critical_value) == 1){
      crit_val <- critical_value
    } else{
      crit_val <- critical_value[i]
    }


    # --------------------------------- get t/z value ---------------------- #
    # 1. ROW
    # -->  get row of effect of interest
    row <- i + 1

    # 2. COLUMN which column is the right one??
    # --> check if model is lmer or glmer

    # what happens if model_emp is a lmer?
    if (lme4::getME(model, "devcomp")$dims[["GLMM"]] == 0) {


      #... this: we need "t value"
      t_value <- abs(coef(summary(model))[row, "t value"])


      # ------------ test aganist significance level ------- #

      # --> is either TRUE or FALSE (= 1 or 0)
      is_significant <- (t_value >= crit_val)


      # store significance
      store_significance <- c(store_significance, is_significant)


      # what happens if model_emp is a glmer?
    } else if (lme4::getME(model, "devcomp")$dims[["GLMM"]] == 1) {

      #... this: we need "z value"
      z_value <- abs(coef(summary(model))[row, "z value"])

      # ------------ test aganist significance level ------- #

      # --> is either TRUE or FALSE (= 1 or 0)
      is_significant <- (z_value >= crit_val)

      # store significance
      store_significance <- c(store_significance, is_significant)
    } # end if
  } # end for
  store_significance
} # end function


#-------------------------------------------------------------------------------------------------------#
# GET N FROM DATA EMP

#' Gets number of participants in a dataset
#'
#' \code{get_n()} checks the column indicating the subject number and returns
#' the number of unique occurences.
#'
#' @param data data set you want to get n for
#' @param simvar name of variable in data that contains the subject number
#'
#' @export
get_n <- function(data, simvar){

  # --> first: check if simvar is numeric and convert to numeric if not
  # --> max() needs numeric vector
  # 1. check if simvar is numeric
  #if( is.numeric(data[[simvar]]) == F) {

    # 2. convert  to numeric
   # data[[simvar]] <- as.numeric(as.character(data[[simvar]]))
  #} # end if

  # --> now get n_now ;)
  n <- length(unique(data[[simvar]], na.rm = T))

  # return nnow
  n
}


#-------------------------------------------------------------------------------------------------------#
# GET DEPVAR FUNCTION

#' Gets dependent variable of a mixed model
#'
#' \code{get_depvar()} checks the formula of a given mixed model and return
#' the variable name of the dependent variable.
#'
#' @param model mixed model you want to know the dependent variable of
#' @export
get_depvar <- function(model){

  # 1. extract model formula and convert it to character
  # --> easier to extract depvar and we need it as character anyways
  formula <- as.character(model@call$formula)

  # 2. extrcat depvar --> second element. No clue why 2... but who cares!
  depvar <- formula[2]

  # return depvar
  depvar

} # end function depvar

