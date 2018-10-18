#' Function thatr runs the whole mixedpower power process
#'
#' \code{mixedpower()} combines all three power options (databased, safeguard,
#' rnorm). It runs them all by default and returns a data frame with results
#' for all three options.
#'
#' @param model_emp lme4 model: mixed model of interest
#' @param data_emp data frame: pilot data that fits the mixed model of interest
#' @param subvar charackter element: name of the variable that contains the
#' subject??s number
#' in data_emp
#' @param fixed_effects vector of character elements: names of variables that
#'  are used as fixed effects in
#' model emp
#' @param critical_value integer: z/t value to test if a given fixed effect
#' is significant
#' @param sampe_sizes vector of integers: sample sizes you want to test power
#'of
#' @param n_sim integer: number of simulations to run
#' @param databased logical value: indicates whether databased power simulation
#' shoul be run
#' @param safeguard logical value: indicates whether safeguard power simulation
#' shoul be run
#' @param rnorm logical value: indicates whether rnorm power simulation
#' shoul be run
#' @return A modified mixed model
#'
#' @export
mixedpower <- function(model_emp, data_emp, subvar, fixed_effects,
                      sample_sizes, n_sim, critical_value, confidence_level,
                      databased = T, safeguard = T, rnorm = F){

  #### This function combines the whole power simulation process ###

  # prepare storing output
  output <- list()
  i <- 1

  # ------------------------ #
  # 1. databased
  if (databased == T){
    databased_power_values <- power_simulation(model_emp, data_emp, subvar, fixed_effects,
                                               critical_value, sample_sizes, n_sim, confidence_level,
                                               safeguard = F, rnorm = F)

    # store output
    databased_power_values["mode"] <- "databased"
    databased_power_values["effect"] <- row.names(databased_power_values)

    output[[i]] <- databased_power_values
    i <- i + 1 # increment i so the next output gets stored at the right position

  } # end if databased

  # 2. safeguard
  if (safeguard == T){
    safeguard_power_values <- power_simulation(model_emp, data_emp, subvar, fixed_effects,
                                               critical_value, sample_sizes, n_sim, confidence_level,
                                               safeguard = T, rnorm = F)

    safeguard_power_values["mode"] <- "safeguard"
    safeguard_power_values["effect"] <- row.names(safeguard_power_values)
    output[[i]] <- safeguard_power_values
    i <- i + 1 # increment i so the next output gets stored at the right position

  } # end if safeguard


  # 3. rnorm
  if (rnorm == T){
    rnorm_power_values <- power_simulation(model_emp, data_emp, subvar, fixed_effects,
                                           critical_value, sample_sizes, n_sim, confidence_level,
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

#-----------------------------------------------------------------------------#



#' Function that runs the power simulation
#'
#' \code{power_simulation()} runs the actual simulation process for either the
#' databased, safeguard or rnorm power option. Returns a data frame with results
#' for all fixed effects and all tested sample sizes.
#'
#' @param model_emp lme4 model: mixed model of interest
#' @param data_emp data frame: pilot data that fits the mixed model of interest
#' @param subvar charackter element: name of the variable that contains the
#' subject??s number
#' in data_emp
#' @param fixed_effects vector of character elements: names of variables that
#'  are used as fixed effects in
#' model emp
#' @param critical_value integer: z/t value to test if a given fixed effect
#' is significant
#' @param sampe_sizes vector of integers: sample sizes you want to test power
#'of
#' @param n_sim integer: number of simulations to run
#' @param safeguard logical value: indicates whether safeguard power simulation
#' shoul be run
#' @param rnorm logical value: indicates whether rnorm power simulation
#' shoul be run
#' @return A modified mixed model
#'
#' @export
power_simulation <- function(model_emp, data_emp, subvar, fixed_effects,
                             critical_value, sample_sizes, n_sim, confidence_level,
                             safeguard = F, rnorm = F){


  # PREPARE:
  # get depvar from model_emp to hand it siimulateSamplesize() later
  depvar <- get_depvar(model_emp)


  # PREPARE TO RUN IN PARALLEL
  cores= parallel::detectCores()
  cl <- parallel::makeCluster(cores[1]-1) #not to overload your computer
  doParallel::registerDoParallel(cl)

  #------------------------------------#
  #------------------------------------#
  # IF SAFEGUARD POWER:
  if (safeguard == T){
    print("Hey its safeguard")
    model_for_simulation <- prepare_safeguard_model(model_emp,
                                                    confidence_level,
                                                    critical_value)
  } else {
    print("no safeguard")
    model_for_simulation <- model_emp
  }

  #------------------------------------#
  #------------------------------------#



  #------------------------------------#
  # prepare storing power values

  ## 1. create an empty data frame witg the right dimensions and names
  # dimension = ncol, nrow
  # [-1] removes the Intercept
  n_row <- length(fixef(model_emp)[-1])
  # n col
  n_col <- length(sample_sizes)

  # row_names = names of effects (again remove intercept)
  row_names <- row.names(summary(model_emp)$coefficients)[-1]

  # empty data frame
  power_values_all <- data.frame(matrix(ncol = n_col, nrow = n_row),
                                 row.names = row_names)
  # name stuff (header)
  names(power_values_all) <- sample_sizes

  index_n <- 0 # index to store power value in Power_value later

  # loop through different sample sizes
  for (n in sample_sizes){

    # inform which sample sizes we are computing power right now
    print(n)

    index_n <- index_n + 1

    # prepare simulation for current n
    ## 1. create object that can store simulations
    #--> data frame with effects as collumns and nsim rows
    #store_simulations <- data.frame(matrix(ncol = n_row, nrow = n_sim))
    #names(store_simulations) <- names(fixef(model_emp)[-1])

    # repeat simulation n_sim times
    # store outcome in store_simulations
    # --> this is a list of vectors!!
    
    # magic cheating
    `%dopar%` <- foreach::`%dopar%`
    #okay now continue
    store_simulations <- foreach::foreach(icount(n_sim), .combine = "cbind",
                                          .export=ls(envir=globalenv()),
                                          .packages = c("lme4")) %dopar% {


                                   #------------------------------------#
                                   #------------------------------------#
                                   # IF RNORM MODEL:
                                   if (rnorm == T){
                                     model_for_simulation <- prepare_rnorm_model(model_emp,
                                                                                 data_emp,
                                                                                 subvar,
                                                                                 critical_value)
                                   }
                                   #------------------------------------#
                                   #------------------------------------#

                                   #-------------------------------------#
                                   # 1. simulate data set with n subjects
                                   simulated_data <- simulateSamplesize(n_want = n,
                                                                        data_emp = data_emp,
                                                                        model_emp = model_for_simulation,
                                                                        subvar = subvar)

                                   #------------------------------------#
                                   #2. code contrasts for simulated data set
                                   final_dataset <- reset_contrasts(simulated_data,
                                                                    data_emp,
                                                                    model_emp,
                                                                    fixed_effects)


                                   #-------------------------------------#
                                   # 3. refit model_emp to current data set (final_dataset)
                                   # --> update model emp with new data set
                                   model_sim <- update(model_emp,
                                                       data = final_dataset)

                                   #-------------------------------------#
                                   # 4. analyze final_data set and store result



                                   # check significance
                                   # --> check_significance() returns 1 if effect is significant, 0 if not
                                   # --> store significance in specified vector
                                   to.store_simulations <- check_significance(model_sim,
                                                                              critical_value)

                                 }# end for loop (n_sim)

    # -------------------------------------#
    # 5. compute power
    ## compute power!
    # margin = 2 --> apply FUN on columns
    # --> vector withs names
    power_values_n <- apply(store_simulations, MARGIN = 1,
                            FUN = mean, na.rm = T)

    # -------------------------------------#
    # 6. store power value
    ## store it!
    column_name <- as.character(n)
    power_values_all[column_name] <- power_values_n

  } # end for loop (samplesizes)

  ## END PARALLEL PROCESSING
  parallel::stopCluster(cl)

  # return data based power values
  power_values_all
  

} # end power simulation function



#-----------------------------------------------------------------------------#


#' Simulate a new data set
#'
#' \code{simulateSamplesize()} builds a new data set with a specified number of
#' subjects. It uses the \code{lme4::simulate()} function to create new response
#' values based on the mixed model fittet to the pilot data.
#'
#' @param n_want integer: how many subjects should the new data set include?
#' @param data_emp data frame: pilot data that fits the mixed model of interest
#' @param model_emp lme4 model: mixed model of interest
#' @param subvar character element: name of the varaible containing the subject
#' number in data_emp
#'
#' @return A modified mixed model
#'
#' @export

simulateSamplesize <- function(n_want, data_emp, model_emp, subvar){
  # ---------------------------------------------------------------------------- #
  # STEP 1: set relevant paramaters

  # whats the dependent variable?
  depvar <- get_depvar(model_emp)

  # how many subjects are in the pilot data? --> n_now
  n_now <- get_n(data_emp, subvar)

  # number of dublicates we need from the original dataset # --> ceiling()
  # --> floor gets next lower integer (we already have one multiplication with exp4)
  mult_factor <- ceiling(n_want/n_now)

  # --> how many subjects do we need to remove if we use this multiplication factor?
  too_much <- (n_now*mult_factor) - n_want

  # --------------------------------------------------------------------------- #
  # STEP 2: sumulate data set

  # simulate (mult_factor) data sets
  for (i in 1:mult_factor){

    # simuate new data --> variable of interest
    new_subject_data <- lme4:::simulate.merMod(model_emp, nsim = 1)

    ###--- create new data set: rename vp variable and replace variable of interest with simulated data--- ##
    # copy old data set
    new_part <- data_emp



    # first iteration: only change variable of interest
    if (i==1){
      new_part[[depvar]] <- new_subject_data[,1]
      final_data <- new_part
      # from second iteration on: change subject names and rbind new data to existing data
    } else {



      # 1. increment vp variable wih current n and replce the old names
      # --> change subjects names only from second iteration on

      # check if it is a factor
      if( is.numeric(final_data[[subvar]]) == F) {
        # do step 1.

        new_names <- as.numeric(new_part[[subvar]]) + (i-1)*n_now
        new_part[[subvar]] <- new_names

        # re-convert subvar to a factor
        new_part[[subvar]] <- factor(new_part[[subvar]])
      } else {

        # do step 1.
        new_names <- new_part[[subvar]] + (i-1)*n_now
        new_part[[subvar]] <- new_names

      } # end inner if else


      # 2. replace variable of interest
      new_part[[depvar]] <- new_subject_data[,1]

      # 3. combine new and old data set
      # --> if first iteration: nothing to rbind, so just new_part it is
      # --> second and more iteration: rbind new simulated subjects to already simulated ones
      final_data <- rbind(final_data, new_part)
    } # end outer if else
  } # end for-loop

  # --------------------------------------------------------------------------- #
  # STEP 3: delete participants to get the exact n  and return it

  #--> subvar needs to be numeric for that, so it needs to be converted temporarly

  # store old version
  #store_old <- final_data[[subvar]]

  # check if it is a factor
  if( is.numeric(final_data[[subvar]]) == F) {
    # 2. convert  to numeric
    final_data[[subvar]] <- as.numeric(final_data[[subvar]])


    # now do STEP 3:
    final_data <- final_data[final_data[[subvar]] <= n_want,]

    # re-convert to factor
    final_data[[subvar]] <- factor(final_data[[subvar]])

    # if not: just subset final data to correct n
  } else {
    final_data <- final_data[final_data[[subvar]] <= n_want,]
  }# end if

  # now do STEP 3:
  #final_data <- final_data[final_data[[subvar]] <= n_want,]

  # get back to old version
  #final_data[[subvar]] <- store_old[1:nrow(final_data)]

  # return final_data
  final_data
}
