# This function runs multiple iterations of a pls model. When creating the model_df in step 2, the 
# script chooses 1, 2, or 3 indicator for each latent variable. You can then run many iterations
# of randomly sampled indicators. The script will then save models that have reliability measures greater
# than 0.7 and r^2 values indicated in the input (no learning or methodological sampling, can have repeats)
# Measurement model contains TOPO, HYDRO, LANDCOVER, DOC

pls_models_no_nutrients <- function(filtered_df, indicators, n_iter = 10, r2_value= 0.4) {
  results <- list()
  
  for (iter in 1:n_iter) {
    cat("\n--- Iteration:", iter, "---\n")
    
    # Step 1: Randomly sample between 1 and 3 variables for each indicator, ensuring at least the available variables
    sampled_indicators <- lapply(indicators, function(vars) {
      max_vars <- min(3, length(vars))  # Cap the maximum number of variables to sample
      num_vars <- sample(1:max_vars, 1)  # Randomly select between 1 and the maximum available
      selected_vars <- sample(vars, num_vars)  # Sample 'num_vars' variables
      cat("Sampled variables:", paste(selected_vars, collapse = ", "), "\n")
      selected_vars
    })
    
    # Step 2: Prepare the model_df
    for (indicator in names(sampled_indicators)) {
      variables <- sampled_indicators[[indicator]]
      for (i in seq_along(variables)) {
        old_name <- variables[i]
        new_name <- paste0(indicator, "_", i)
        model_df[[new_name]] <- filtered_df[[old_name]]
      }
    }
    
    # Step 3: Standardize the data
    model_df_standardized <- as.data.frame(scale(model_df, center = TRUE, scale = TRUE))  # Keep all columns
 
    #Step 4: Define measurement model
    measurement_model <- constructs(
      composite("TOPO", multi_items("topo_", 1:length(sampled_indicators$topo))),
      composite("HYDRO", multi_items("hydro_", 1:length(sampled_indicators$hydro))),# multi_items("hydro_", 1:2)),,      composite("LANDCOVER", multi_items("landcover_", 1:length(sampled_indicators$landcover))),
      composite("DOC",  multi_items("doc_", 1:length(sampled_indicators$doc)))# single_item('dom_1'))#,
    )
    
    #Step 5: Define structural model
    structural_model <- relationships(
      paths(from = "TOPO", to = c("HYDRO", "LANDCOVER", "DOC")),
      paths(from = "LANDCOVER", to = c("HYDRO", "DOC")),
      paths(from = "HYDRO", to = "DOC")
    )
    
    # Step 6: Estimate the model
    pls_model <- estimate_pls(
      data = model_df_standardized,
      measurement_model = measurement_model,
      structural_model = structural_model,
      inner_weights = path_weighting,
      missing = mean_replacement,
      missing_value = "NA"
    )
    
    # Step 7. Evaluate and save summaries with sufficiently high reliability
    model_summary <- summary(pls_model)
    reliability <- model_summary$reliability
    r2_DOC <- model_summary$paths["R^2", "DOC"]
    
    if (all(reliability > 0.7, na.rm = TRUE) && abs(r2_DOC) > r2_value) {
      results[[iter]] <- list(
        summary = model_summary,
        sampled_indicators = sampled_indicators
      )
      cat("PLS model summary saved for iteration", iter, "\n")
    } else {
      cat('NOT saved for iteration',iter,'low reliability')
    }
  }
  
  return(results)  # Return the list of summaries
}



###########################################################################################
# Same as above except measurement model contains TOPO, HYDRO, LANDCOVER, DOC

pls_models_nutrients <- function(filtered_df, indicators, n_iter = 10, r2_value= 0.4) {
  results <- list()
  
  for (iter in 1:n_iter) {
    cat("\n--- Iteration:", iter, "---\n")
    
    # Step 1: Randomly sample between 1 and 3 variables for each indicator, ensuring at least the available variables
    sampled_indicators <- lapply(indicators, function(vars) {
      max_vars <- min(3, length(vars))  # Cap the maximum number of variables to sample
      num_vars <- sample(1:max_vars, 1)  # Randomly select between 1 and the maximum available
      selected_vars <- sample(vars, num_vars)  # Sample 'num_vars' variables
      cat("Sampled variables:", paste(selected_vars, collapse = ", "), "\n")
      selected_vars
    })
    
    # Step 2: Prepare the model_df
    for (indicator in names(sampled_indicators)) {
      variables <- sampled_indicators[[indicator]]
      for (i in seq_along(variables)) {
        old_name <- variables[i]
        new_name <- paste0(indicator, "_", i)
        model_df[[new_name]] <- filtered_df[[old_name]]
      }
    }
    str(model_df)
    
    # Step 3: Standardize the data
    model_df_standardized <- as.data.frame(scale(model_df, center = TRUE, scale = TRUE))  # Keep all columns
    
    #Step 4: Define measurement model
    measurement_model <- constructs(
      composite("TOPO", multi_items("topo_", 1:length(sampled_indicators$topo))),
      composite("HYDRO", multi_items("hydro_", 1:length(sampled_indicators$hydro))),# multi_items("hydro_", 1:2)),, 
      composite("LANDCOVER", multi_items("landcover_", 1:length(sampled_indicators$landcover))),
      composite("DOC",  multi_items("doc_", 1:length(sampled_indicators$doc))),# single_item('dom_1'))#,
      composite("NUTRIENT", multi_items("nutr_", 1:length(sampled_indicators$nutr)))
    )
    
    #Step 5: Define structural model
    # Create structural model
    structural_model <- relationships(
      paths(from = c("TOPO"), to = c("HYDRO", "LANDCOVER",  "NUTRIENT")), #"F_IONS",
      paths(from = c("LANDCOVER"), to = c("HYDRO", "DOC", "NUTRIENT")), #"F_IONS",
      paths(from = c("HYDRO"), to = c("DOC")),
      paths(from =c("NUTRIENT"), to = c("DOC"))
    )
     
    # Step 6: Estimate the model
    pls_model <- estimate_pls(
      data = model_df_standardized,
      measurement_model = measurement_model,
      structural_model = structural_model,
      inner_weights = path_weighting,
      missing = mean_replacement,
      missing_value = "NA"
    )
    
    # Step 7. Evaluate and save summaries with sufficiently high reliability
    model_summary <- summary(pls_model)
    reliability <- model_summary$reliability
    r2_DOC <- model_summary$paths["R^2", "DOC"]
    
    if (all(reliability > 0.7, na.rm = TRUE) && abs(r2_DOC) > r2_value) {
      results[[iter]] <- list(
        summary = model_summary,
        sampled_indicators = sampled_indicators
      )
      cat("PLS model summary saved for iteration", iter, "\n")
    } else {
      cat('NOT saved for iteration',iter,'low reliability')
    }
  }
  
  return(results)  # Return the list of summaries
}


