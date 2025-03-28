### Heddesheimer: PanelMatch Function
# vincent.heddesheimer@princeton.edu

# This is a custom function to run PanelMatch on a dataset.

# Load packages
library(tidyverse)
library(data.table)
library(PanelMatch)
library(stringr)


# Theme
theme_vincent <- function (fontsize = 15, facet_alt = F) {
  th <- ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = element_blank(), 
                                             panel.grid.minor = element_blank(), text = element_text(size = fontsize))
  if (facet_alt) {
    th <- th + theme(strip.background = element_blank()) + 
      theme(strip. = element_text(colour = "black"))
  }
  th
}

# Function
run_panelmatch <- function(data,
                           treatment,
                           outcome,
                           covariates = NULL,
                           lag = 2,
                           lead = 0:1,
                           unit_id = "id",
                           time_id = "t",
                           refinement.method = "CBPS.weight",
                           figure_path = "output/figures/panelmatch/",
                           dataframes_path = "output/dataframes/panelmatch/",
                           results_width = 5,
                           results_height = 5.5,
                           covariate_balance_width = 7,
                           covariate_balance_height = 8,
                           forbid_treatment_reversal = FALSE,
                           placebo_iterations = 100,
                           se_method = "conditional",
                           cb_plots = FALSE
) {
  # Force refinement.method to "none" if covariates is NULL
  if (is.null(covariates)) {
    refinement.method <- "none"
  }
  
  tryCatch({
    # Create scatterplot if requested
    if (cb_plots == TRUE && !is.null(covariates)) {  # Only do covariate balance plots if covariates exist
      # Create matched set for covariate balance
      message(paste("Create matched set for covariate balance with lagged outcome"))
      
      match <- PanelMatch(
        lag = lag,
        time.id = time_id,
        unit.id = unit_id,
        covs.formula = as.formula(paste("~", paste(c(covariates, outcome), collapse = " + "))),
        treatment = treatment,
        refinement.method = refinement.method,
        data = data,
        match.missing = TRUE,
        qoi = "att",
        outcome.var = outcome,
        lead = lead,
        forbid.treatment.reversal = forbid_treatment_reversal,
        placebo.test = FALSE
      )
      
      # Create a scatter plot to analyze covariate balance
      message(paste("Create a scatter plot to analyze covariate balance"))
      
      pdf(paste0(figure_path,
                 treatment,
                 "_",
                 outcome,
                 "_cb_scat.pdf"))
      
      balance_scatter(
        matched_set_list = list(match$att),
        data = data,
        covariates = if (!is.null(covariates)) c(covariates, outcome) else outcome
      )
      # Close the pdf file
      dev.off()
      
      # Create covariate balance before treatment
      message(paste("Analyze covariate balance before treatment"))
      
      
      # Note that this includes covariates as well as outcome
      cb <- as_tibble(
        get_covariate_balance(
          match$att,
          data = data,
          covariates = c(covariates, outcome),
          plot = FALSE
        ),
        rownames = "t"
      ) |>
        pivot_longer(
          cols = c(covariates, outcome),
          names_to = "covariate",
          values_to = "covbal"
        ) |>
        mutate(
          t = as.integer(str_replace(t, "t_", "-")),
          treatment = treatment,
          outcome = outcome
        )
      
      # cb df
      covariate_balance_df <- rbind(covariate_balance_df, cb)
      
      # Create Covariance Balance Plot
      # Create a named color vector where the outcome is "black" and all other covariates are "grey"
      all_vars <- c(covariates, outcome)
      color_mapping <-
        setNames(rep("grey", length(all_vars)), all_vars)
      color_mapping[outcome] <- "black"
      
      # Plot
      cb_plot <- cb |>
        ggplot(aes(x = t, y = covbal, color = covariate)) +
        geom_line() +
        scale_color_manual(values = color_mapping) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_vline(xintercept = -1, linetype = "dashed") +
        scale_x_continuous(breaks = scales::breaks_extended(n = lag)) +
        scale_y_continuous(limits = c(-.5, .5)) +
        theme_vincent() +
        theme(
          legend.position = "none"
        ) +
        labs(y = "Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")
      
      # Save
      ggsave(
        filename = paste0(treatment, "_", outcome, "_cb_pre.pdf"),
        plot = cb_plot,
        path = figure_path,
        height = covariate_balance_height,
        width = covariate_balance_width
      )
      
      
    }
    
    # Create matched set for ATT & placebo
    message(paste("Create matched set for ATT & placebo"))
    
    # Check if outcome is in covariates
    outcome_in_covariates <- !is.null(covariates) && outcome %in% covariates
    
    if (!is.null(covariates) && length(covariates) > 0) {
      match <- PanelMatch(
        lag = lag,
        time.id = time_id,
        unit.id = unit_id,
        covs.formula = as.formula(paste("~", paste(covariates, collapse = " + "))),
        treatment = treatment,
        refinement.method = refinement.method,
        data = data,
        match.missing = TRUE,
        qoi = "att",
        outcome.var = outcome,
        lead = lead,
        forbid.treatment.reversal = forbid_treatment_reversal,
        placebo.test = !outcome_in_covariates  # Set to FALSE if outcome is in covariates
      )
    } else {
      match <- PanelMatch(
        lag = lag,
        time.id = time_id,
        unit.id = unit_id,
        treatment = treatment,
        refinement.method = refinement.method,
        data = data,
        match.missing = TRUE,
        qoi = "att",
        outcome.var = outcome,
        lead = lead,
        forbid.treatment.reversal = forbid_treatment_reversal,
        placebo.test = TRUE
      )
    }
    
    
    # Estimate ATT
    message(paste("Estimate ATT"))
    
    est <-
      PanelEstimate(sets = match,
                    data = data,
                    se.method = se_method)
    
    # Extract relevant portions of the summary data frames for each lead
    summary_est <- summary(est)$summary
    
    lead_rows <-
      lead + 1  # Assuming leads start from 0 and the summary is indexed starting from 1
    tmp_results <- data.frame(
      treatment = rep(treatment, length(lead)),
      outcome = rep(outcome, length(lead)),
      t = rep(lead, each = 1),
      # Now dynamic
      estimate = summary_est[lead_rows, 1],
      conf.low = summary_est[lead_rows, 3],
      conf.high = summary_est[lead_rows, 4],
      # calculate 90% confidence intervals
      conf.low90 = summary_est[lead_rows, 1] - summary_est[lead_rows, 2] * qnorm(0.95),
      conf.high90 = summary_est[lead_rows, 1] + summary_est[lead_rows, 2] * qnorm(0.95),
      stringsAsFactors = FALSE
    )
    
    # Estimate placebo ATT only if outcome is not in covariates
    if (!outcome_in_covariates) {
      message(paste("Estimate placebo ATT"))
      
      placebo_df <- placebo_test(
        pm.obj = match, data = data, 
        se.method = se_method,
        number.iterations = placebo_iterations, 
        plot = FALSE)
      
      # Create data.frame for binding
      placebo_df <- data.frame(
        t = c(-1, as.numeric(str_remove(names(placebo_df$estimates), "t"))),
        estimate = c(0, placebo_df$estimates),
        se = c(0, placebo_df$standard.errors),
        conf.low = c(0, placebo_df$estimates - placebo_df$standard.errors * qnorm(0.975)),
        conf.high = c(0, placebo_df$estimates + placebo_df$standard.errors * qnorm(0.975)),
        conf.low90 = c(0, placebo_df$estimates - placebo_df$standard.errors * qnorm(0.95)),
        conf.high90 = c(0, placebo_df$estimates + placebo_df$standard.errors * qnorm(0.95)),
        stringsAsFactors = FALSE
      ) |>
        select(-se)
      rownames(placebo_df) <- NULL
      
      # Bind with tmp_results
      tmp_results <- rbind(tmp_results, placebo_df |> mutate(treatment = treatment, outcome = outcome))
    }
    
    # Add these results to the overall dataframe
    results_df <- rbind(results_df, tmp_results)
    
    # Plot results
    message(paste("Plot results"))
    
    plot_data <- data.frame(
      t = lead,
      estimate = summary_est[lead_rows, 1],
      conf.low = summary_est[lead_rows, 3],
      conf.high = summary_est[lead_rows, 4],
      conf.low90 = summary_est[lead_rows, 1] - summary_est[lead_rows, 2] * qnorm(0.95),
      conf.high90 = summary_est[lead_rows, 1] + summary_est[lead_rows, 2] * qnorm(0.95),
      stringsAsFactors = FALSE
    ) 
    
    # Bind with placebo only if outcome is not in covariates
    if (!outcome_in_covariates) {
      plot_data <- rbind(plot_data, placebo_df)
    }
    
    # Plotting...
    plot <- plot_data |>
      ggplot(aes(x = t, y = estimate)) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "red",
        linewidth = .25,
        alpha = 0.75
      ) +
      geom_vline(
        xintercept = -0.5,
        linetype = "dashed",
        color = "red",
        linewidth = .25,
        alpha = 0.75
      ) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                    width = 0,
                    linewidth = .5) +
      geom_errorbar(aes(ymin = conf.low90, ymax = conf.high90),
                    width = 0,
                    linewidth = 1.25) +
      labs(y = "ATT", x = "Relative Time") +
      geom_point(aes(x = t, y = estimate), size = 2, shape = 21, fill = "white") +
      # scale_x_continuous(breaks = lead) +
      theme_vincent()
    
    # Save
    ggsave(
      filename = paste0(treatment, "_", outcome, "_att.pdf"),
      plot = plot,
      path = figure_path,
      width = 5,
      height = 5.5
    )
    
  }, error = function(e) {
    warning(paste("Caught an error:", e))
    return(NULL)
  })
  
  # Return the results data frames
  return(list(results_df = results_df, covariate_balance_df = covariate_balance_df))
}