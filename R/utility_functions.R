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
#' @param data pilot data that fits the mixed model of interest
#' @param simvar name of the variable that contains the subject??s number
#' in data
#' @param critical_value z/t value to test if a given fixed effect
#' is significant
#'
#' @return A modified mixed model
#'

prepare_rnorm_model <- function(model, data, simvar, critical_value){

  # --------------------------------------------------------- #

  # 1. get fixed effects and their distribution

  # [-1, ] removes intercept --> we dont need it
  fixefs <- summary(model)$coefficients[-1,]
  fixef_values <- fixefs[, "Estimate"]

  # SD = SE* sqrt(n)
  # get n to compute SD
  n <- get_n(data, simvar)
  SD <- fixefs[, "Std. Error"] * sqrt(n)

  ### prepare ###
  # --> this is a TRUE FALSE vector
  significant_effects <- check_significance(model, critical_value)



  # ---------- change model ------------------------ #
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
      model@beta[i + 1] <- rnorm(1, mean = effect, sd = sd)

    } else if(significant_effects[i] == FALSE){

      # inform user that old paramter is used to compute power
      non_significant_effect <- row.names(summary(model)$coefficients)[i+1]
      cat(paste("WARNING:", non_significant_effect, "is not significant in model. \n  rNorm Power cannot be computed for this effect\n"))

    } else {

      cat("WARNING: Something went wrong with the rnorm model")
    } # end if else
  } # end for

  ### return modified model == rnorm model
  model
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
#' @param model mixed model of interest
#' @param confidence_level level of confidence that the parameter lies
#' in the interval
#' @param critical_value z/t value to test if a given fixed effect
#' is significant
#'
#' @return A modified mixed model
#'

prepare_safeguard_model <- function(model, confidence_level, critical_value){

  # ---------- prepare ------------------------ #
  # 1. check which effects are significant

  # --> this is a TRUE FALSE vector
  significant_effects <- check_significance(model, critical_value)

  # 2. compute confidence intervals

  # things we want intervals for: (kick out those .sig01 stuff)
  effects <- row.names(summary(model)$coefficients)[-1]
  confints <- confint(model, method = "Wald",  level = confidence_level, parm = effects)


  # ---------- change model ------------------------ #
  # 1. look at every effect
  for (i in 1:length(significant_effects)){

    # check if effect is significant:
    if(significant_effects[i] == TRUE){

      # change paramter to safeguard value

      # A. get safeguard effect: needs to be the one closer to 0
      #--> depends on effect of interest: negativ or positiv ?
      row <- i +1 # because there also is the intercept in coefficients that we want to skip
      safeguard_effect <- ifelse(summary(model)$coefficients[row,1] > 0,
                                 confints[i,1],
                                 confints[i,2])

      # B. change model parameter of effect of interest to safeguard effect
      model@beta[row] <- safeguard_effect

    } else if(significant_effects[i] == FALSE){

      # inform user that old paramter is used to compute power
      non_significant_effect <- row.names(summary(model)$coefficients)[i+1]
      cat(paste("WARNING:", non_significant_effect, "is not significant in model. \n  Safeguard Power cannot be computed for this effect\n"))

    } else {

      cat("WARNING: Something went wrong with the safeguard model")
    } # end if else
  } # end for


  # ---------- return safeguard model ------------------------ #
  model
} # end function

#-------------------------------------------------------------------------------------------------------#
# RESET CONTRASTS FUNCTION

# simulateDataset loses contrasts --> rbind
# --> this function resets all contrasts from data


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
#' @param data pilot data used for power simulation
#' @param model mixed model used for power simulation.
#' Needs to fit data.
#' @param fixed_effects names of variables in data that are used as
#' fixed effects in model
#'
#' @return Date frame
#'

reset_contrasts <- function(simulated_data, data, model, fixed_effects ) {

  # --> 1. get names from all variables in model
  variable_names <- fixed_effects


  # --> 2. loop through names and reset contrasts for all involved variables
  for (variable in variable_names){

    # check if variable is a factor (contrasts only work with factors)
    if (is.factor(data[[variable]]) == T){
      # get original contrast
      data_contr <- contrasts(data[[variable]])


      # apply original contrast to simulated data set
      contrasts(simulated_data[[variable]]) <- data_contr
    } # end if
  } # end for loop

  # return simulated data set
  simulated_data
} # end function

#-------------------------------------------------------------------------------------------------------#

# CHECK SIGNIFICANCE

#' Gets t/z value out of model and compares it to a ctritical value
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

    # what happens if model is a lmer?
    if (lme4::getME(model, "devcomp")$dims[["GLMM"]] == 0) {


      #... this: we need "t value"
      t_value <- abs(coef(summary(model))[row, "t value"])


      # ------------ test aganist significance level ------- #

      # --> is either TRUE or FALSE (= 1 or 0)
      is_significant <- (t_value >= crit_val)


      # store significance
      store_significance <- c(store_significance, is_significant)


      # what happens if model is a glmer?
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

get_depvar <- function(model){

  # 1. extract model formula and convert it to character
  # --> easier to extract depvar and we need it as character anyways
  formula <- as.character(model@call$formula)

  # 2. extrcat depvar --> second element. No clue why 2... but who cares!
  depvar <- formula[2]

  # return depvar
  depvar

} # end function depvar

#-------------------------------------------------------------------------------------------------------#
# GET DEPVAR FUNCTION

#' keeps balance of groups in designs with between-subject variables
#'
#'
#' @param final_data simulated data
#' @param simvar variable that is changed in simulation process
#' @param fixed_effects fixed effects in model used to simulate data
#' @param n_want levels of simvar the data should have

keep_balance <- function(final_data, simvar, fixed_effects, n_want){

  # --------------------------------- #
  # 1. FIND BETWEEN SUBJECT VARIABLES
  between <- c()
  for(column in fixed_effects){

    # go through subjects/simvar and check if subjects: only one value per subject per condition --> between
    for (sub in unique(final_data[[simvar]])){
      add <- ifelse(length(unique(final_data[final_data[[simvar]] == sub,][[column]])) == 1, T, F)}

    # store between variables
    if(add== T){
      final_data[[column]] <- as.factor(final_data[[column]]) # make sure all between variables ar factor
      between <- c(between, column)
    }

  }

  # rewrite all between-variables into factor:


  # ------------------------- #
  # BETWEEN DESIGNS
  # ------------------------- #

  if(length(between) > 0){

    # --------------------------------- #
    # 2. GET ALL COMBINATIONS OF BETWEEN SUBJECT VARIABLES
    # --> get overview which subject is in which combination

    # get all combinations of all levels for all between subject variables
    combinations <- expand.grid(sapply(subset(final_data, select = between), levels))

    # --------------------------------- #
    # 3. CHECK WHICH SUBJECTS ARE IN WHICH COMBINATION
    # --> get overview which subject is in which combination
    sub_groups <- list()
    # loop through combinations of combinations
    for (cond_comb in 1:nrow(combinations)){

      # A. create nested list to store subjects in them
      sub_groups[cond_comb] <- c(NaN) # doesn't work with empty vector

      # B. loop through all subjects and store the ones that are in current combination
      for (sub in unique(final_data[[simvar]])){
        group <- unique(subset(final_data[final_data[[simvar]] == sub,], select = between))

        # C. check if subject is in current  combination
        if(length(between) == 1){
          if(sum(combinations[cond_comb,] == group[[1]]) == length(between)){
            sub_groups[[cond_comb]] <- c(sub_groups[[cond_comb]], sub)}
        } else {
          if(sum(combinations[cond_comb,] == group) == length(between)){
            sub_groups[[cond_comb]] <- c(sub_groups[[cond_comb]], sub)}
        }


      }
    }
    # remove Nans
    sub_groups <- lapply(sub_groups, function(x) x[!is.na(x)])


    # --------------------------------- #
    # 4. GET RATIOS AND NEW Ns
    combinations$ratio <- sapply(sub_groups,function(x) length(x)/get_n(final_data, simvar))
    combinations$new_n <- sapply(combinations$ratio, function(x) n_want*x)

    # round to integer while keeping sum:
    y <- floor(combinations$new_n)
    indices <- tail(order(combinations$new_n-y), round(n_want - sum(y)))
    y[indices] <- y[indices] + 1
    combinations$new_n <- y

    # --------------------------------- #
    # 5. RANDOMLY SELECT SUBJECTS FROM EVERY GROUP
    keep <- c()
    for (group in 1:length(sub_groups)){
      keep <- c(keep, sample(sub_groups[[group]], combinations$new_n[group]))
    }


  } else {

    # ------------------------- #
    # WITHIN DESIGNS
    # ------------------------- #

    # if no between variables: just select random ones! Wish everything would be this easy
    keep <- sample(unique(final_data[[simvar]]), n_want)

  } # end if else

  keep # return value
} # end function


# ------------------------------- check_input ---------------------------------#
#' Function trunning the actual power simulation
#'
#' \code{check_input()} checks if inout handed to simulation is correct.
#' Returns error messages.
#'
#' @param model lme4 model: mixed model of interest
#' @param data data frame: pilot data that fits the mixed model of interest
#' @param simvar charackter element: name of the variable that contains the
#' subject??s number
#' in data
#' @param fixed_effects vector of character elements: names of variables that
#'  are used as fixed effects in
#' model emp
#' @param critical_value integer: z/t value to test if a given fixed effect
#' is significant
#' @param sampe_sizes vector of integers: sample sizes you want to test power
#'of
#' @param n_sim integer: number of simulations to run
#' @param SESOI Smallest effect of interst
#' @param R2 logical value: indicating if a R2 simulation should be run
#' @param R2var character: name of second random effect we want to vary
#' @param R2level integer: number of levels for R2var. Right now, the second

check_input <- function(model, data, fixed_effects, simvar,
                        steps, critical_value, n_sim,
                        SESOI, R2, R2var, R2level){


  # -------- simvar ---------- #
  if (is.numeric(data[[simvar]]) == F){
    stop('"simvar" needs to be numeric. Consider creating a numeric dummy variable.')
  }

  # make sure that simvar is also continous
  data[[simvar]] <- as.numeric(as.factor(data[[simvar]]))

  # -------- steps ---------- #
  if(is.numeric(steps) == F | length(steps) < 1){
    stop('"steps" needs to be numeric and contain at least one value')
  }

  # --------- fixed effects ------- #
  if (is.character(fixed_effects) == F) {
    stop('"fixed_effects" need to be handed to the simulation in character format')
  }

  for(col in fixed_effects){
    if(col %in% colnames(data) == F){
      stop(paste('The fixed effect "',col, '"', " does not exist in the data frame handed to the simulation", sep = ""))
    }
  }

  # --------- critical value ----- #
  if (is.numeric(critical_value) == F){ stop('"critical_value" needs to be numeric.')}

  if (length(critical_value) != 1
      & length(critical_value) != length(row.names(summary(model)$coefficients)[-1])){
    stop('"critical_value" needs to be of length 1 or contain as many values as effects in the model (including interactions).')
  }

  # --------- critical value ----- #
  if(SESOI != F
     & length(SESOI) != length(row.names(summary(model)$coefficients)[-1])){
    stop('"SESOI" needs to contain as many values as effects in the model (including interactions).')
  }

  # --------- nsim ----------- #
  if(n_sim < 2 | is.numeric(n_sim) == F){
    stop('"n_sim" needs to be numeric and be greater than 1.')
  }

  # --------- R2 ----------- #
  if(R2 == T){
    if (is.numeric(data[[R2var]]) == F){
      stop('"R2var" needs to be numeric. Consider creating a numeric dummy variable.')}

    data[[R2var]] <- as.numeric(as.factor(data[[R2var]]))

    if(is.numeric(R2level) == F | length(R2level) != 1){
      stop('"R2level" needs to be numeric and of length 1. Simulations for multiple R2levels are currently not supported.')
    }
  }

  data # return modified data
}




