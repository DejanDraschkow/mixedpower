#' Function thatr runs the whole mixedpower power process for one random effect
#'
#' \code{mixedpower()} combines all three power options (databased, safeguard,
#' rnorm). It runs them all by default and returns a data frame with results
#' for all three options.
#'
#' @param model lme4 model: mixed model of interest
#' @param data data frame: pilot data that fits the mixed model of interest
#' @param fixed_effects vector of character elements: names of variables that
#'  are used as fixed effects in
#' model emp
#' @param simvar charackter element: name of the variable that contains the
#' random effect we want to simulate along (e.g. subject number or stimuli number)
#' in data
#' @param steps vector of integers: steps for "simvar" you want to test power
#' of
#' @param critical_value integer: z/t value to test if a given fixed effect
#' is significant
#' @param n_sim integer: number of simulations to run
#' @param confidence_level float: value between 0-1 indicating the width of the
#' confidence interval used for the safeguard option
#' @param databased logical value: indicates whether databased power simulation
#' shoul be run
#' @param safeguard logical value: indicates whether safeguard power simulation
#' shoul be run
#' @param rnorm logical value: indicates whether rnorm power simulation
#' shoul be run
#' @return A modified mixed model
#'
#' @export
mixedpower <- function(model, data, fixed_effects, simvar,
                       steps, critical_value, n_sim = 1000,
                       SESOI = F, databased = T){

  # keep this so power_simulation function is compatible with mixedpower1 and mixedpower
  confidence_level <- 0.68

  #### This function combines the whole power simulation process ###

  # prepare storing output
  output <- list()
  i <- 1

  # ------------------------ #
  # 1. databased
  if (databased == T){
    databased_power_values <- power_simulation(model, data, simvar, fixed_effects,
                                               critical_value, steps, n_sim, confidence_level,
                                               safeguard = F, rnorm = F)

    # store output
    databased_power_values["mode"] <- "databased"
    #databased_power_values["effect"] <- row.names(databased_power_values)

    output[[i]] <- databased_power_values
    i <- i + 1 # increment i so the next output gets stored at the right position

  } # end if databased

  # 2. SESOI
  if (SESOI != F){

    # change beta coeficients to SESOI values
    model@beta <- SESOI

    # run SESOI power analysis
    SESOI_power_values <- power_simulation(model, data, simvar, fixed_effects,
                                           critical_value, steps, n_sim, confidence_level,
                                           safeguard = F, rnorm = F)

    SESOI_power_values["mode"] <- "SESOI"
    #SESOI_power_values["effect"] <- row.names(SESOI_power_values)
    output[[i]] <- SESOI_power_values

  } # end if rnorm
  # ------------------------ #

  # prepare storing final results
  results <- data.frame()

  # combine results
  for (ii in 1:length(output)){
    results <- rbind(results, output[[ii]])
  }# end for loop

  # return and save stuff
  save(results, file = "output_powersismulation.Rda")
  results

} # end function


# ------------------------------------------------------------------------------- #

#' Function thatr runs the whole mixedpower power process for two random effect
#'
#' \code{R2power()} combines all three power options (databased, safeguard,
#' rnorm). It runs them all by default and returns a data frame with results
#' for all three options.
#'
#' @param model lme4 model: mixed model of interest
#' @param data data frame: pilot data that fits the mixed model of interest
#' @param fixed_effects vector of character elements: names of variables that
#'  are used as fixed effects in
#' model emp
#' @param simvar charackter element: name of the variable that contains the
#' random effect we want to simulate along (e.g. subject number or stimuli number)
#' in data
#' @param steps vector of integers: steps for "simvar" you want to test power
#' of
#' @param critical_value integer: z/t value to test if a given fixed effect
#' is significant
#' @param n_sim integer: number of simulations to run
#' @param confidence_level float: value between 0-1 indicating the width of the
#' confidence interval used for the safeguard option
#' @param databased logical value: indicates whether databased power simulation
#' shoul be run
#' @param safeguard logical value: indicates whether safeguard power simulation
#' shoul be run
#' @param rnorm logical value: indicates whether rnorm power simulation
#' shoul be run
#' @return A modified mixed model
#'
#' @export
R2power <- function(model, data, fixed_effects, simvar,
                    steps, R2var, R2level, critical_value,
                    n_sim = 1000, SESOI = F, databased = T){

  # keep this so power_simulation function is compatible with mixedpower1 and mixedpower
  confidence_level <- 0.68

  #### This function combines the whole power simulation process ###

  # prepare storing output
  output <- list()
  i <- 1

  # ------------------------ #
  # 1. databased
  if (databased == T){
    databased_power_values <- power_simulation(model, data, simvar, fixed_effects,
                                               critical_value, steps, n_sim, confidence_level,
                                               safeguard = F, rnorm = F)

    # store output
    databased_power_values["mode"] <- "databased"
    #databased_power_values["effect"] <- row.names(databased_power_values)

    output[[i]] <- databased_power_values
    i <- i + 1 # increment i so the next output gets stored at the right position

  } # end if databased

  # 2. SESOI
  if (SESOI != F){

    # change beta coeficients to SESOI values
    model@beta <- SESOI

    # run SESOI power analysis
    SESOI_power_values <- power_simulation(model, data, simvar, fixed_effects,
                                           critical_value, steps, n_sim, confidence_level,
                                           safeguard = F, rnorm = F)

    SESOI_power_values["mode"] <- "SESOI"
    #SESOI_power_values["effect"] <- row.names(SESOI_power_values)
    output[[i]] <- SESOI_power_values

  } # end if rnorm
  # ------------------------ #

  # prepare storing final results
  results <- data.frame()

  # combine results
  for (ii in 1:length(output)){
    results <- rbind(results, output[[ii]])
  }# end for loop

  # return and save stuff
  save(results, file = "output_powersismulation.Rda")
  results

} # end function

#-----------------------------------------------------------------------------#



#' Function thatr runs the whole mixedpower power process
#'
#' \code{mixedpower()} combines all three power options (databased, safeguard,
#' rnorm). It runs them all by default and returns a data frame with results
#' for all three options.
#'
#' @param model lme4 model: mixed model of interest
#' @param data data frame: pilot data that fits the mixed model of interest
#' @param fixed_effects vector of character elements: names of variables that
#'  are used as fixed effects in
#' model emp
#' @param simvar charackter element: name of the variable that contains the
#' random effect we want to simulate along (e.g. subject number or stimuli number)
#' in data
#' @param steps vector of integers: steps for "simvar" you want to test power
#' of
#' @param critical_value integer: z/t value to test if a given fixed effect
#' is significant
#' @param n_sim integer: number of simulations to run
#' @param confidence_level float: value between 0-1 indicating the width of the
#' confidence interval used for the safeguard option
#' @param databased logical value: indicates whether databased power simulation
#' shoul be run
#' @param safeguard logical value: indicates whether safeguard power simulation
#' shoul be run
#' @param rnorm logical value: indicates whether rnorm power simulation
#' shoul be run
#' @return A modified mixed model
#'
#' @export
mixedpower1 <- function(model, data, fixed_effects, simvar,
                        steps, critical_value, n_sim = 1000, confidence_level= 0.68,
                        databased = T, safeguard = F, rnorm = F){



  #### This function combines the whole power simulation process ###

  # prepare storing output
  output <- list()
  i <- 1

  # ------------------------ #
  # 1. databased
  if (databased == T){
    databased_power_values <- power_simulation(model, data, simvar, fixed_effects,
                                               critical_value, steps, n_sim, confidence_level,
                                               safeguard = F, rnorm = F)

    # store output
    databased_power_values["mode"] <- "databased"
    databased_power_values["effect"] <- row.names(databased_power_values)

    output[[i]] <- databased_power_values
    i <- i + 1 # increment i so the next output gets stored at the right position

  } # end if databased

  # 2. safeguard
  if (safeguard == T){
    safeguard_power_values <- power_simulation(model, data, simvar, fixed_effects,
                                               critical_value, steps, n_sim, confidence_level,
                                               safeguard = T, rnorm = F)

    safeguard_power_values["mode"] <- "safeguard"
    safeguard_power_values["effect"] <- row.names(safeguard_power_values)
    output[[i]] <- safeguard_power_values
    i <- i + 1 # increment i so the next output gets stored at the right position

  } # end if safeguard


  # 3. rnorm
  if (rnorm == T){
    rnorm_power_values <- power_simulation(model, data, simvar, fixed_effects,
                                           critical_value, steps, n_sim, confidence_level,
                                           safeguard = F, rnorm = T)

    rnorm_power_values["mode"] <- "rnorm"
    rnorm_power_values["effect"] <- row.names(rnorm_power_values)
    output[[i]] <- rnorm_power_values

  } # end if rnorm
  # ------------------------ #

  # prepare storing final results
  results <- data.frame()

  # combine results
  for (ii in 1:length(output)){
    results <- rbind(results, output[[ii]])
  }# end for loop

  # return and save stuff
  save(results, file = "output_powersismulation.Rda")
  results

} # end function
