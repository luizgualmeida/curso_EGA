########################################################
# Função para extrair os dados de invariância configural
########################################################

config_invariance <- function(invariance){
  if(invariance$configural.results$configural_flag == TRUE) {
    print("Configural invariance is supported.")
  } else {
    print("Configural invariance is not supported.")
  }
}





########################################################
# Função para extrair invariância aproximada
########################################################

approx_invariance <- function(invariance){
  total_tests <- sum(unlist(lapply(invariance$results, function(x) {
    if (is.data.frame(x)) {
      nrow(x)
    } else {
      0
    }
  })))
  total_significant_BH <- sum(unlist(lapply(invariance$results, function(x) {
    if (is.data.frame(x)) {
      sum(x$p_BH < 0.05)
    } else {
      0
    }
  })))
  prop_significant_BH <- round(total_significant_BH / total_tests, 2)
  if (prop_significant_BH < 0.25) {
    cat("Approximate invariance is supported. ", prop_significant_BH*100, " % of the tests are significant which is less than 25%.", "\n")
  } else {
    cat("Approximate invariance is not supported. ", prop_significant_BH*100, " % of the tests are significant which is more than 25%.", "\n")
  }
}








########################################################
# Função para extrair invariância métrica aproximada
########################################################

get_significant_results_2_4_trust <- function(results) {
  result_df <- data.frame(
    Comparison = character(),
    Item = character(),
    Membership = character(),
    Difference = numeric(),
    P_Value = numeric(),
    P_Value_BH = numeric(),
    Direction = character(),
    stringsAsFactors = FALSE
  )
  
  if(is.null(results) || length(results) == 0) {
    return(result_df)
  }
  
  for(comparison_name in names(results)) {
    comparison_data <- results[[comparison_name]]
    
    if(!is.data.frame(comparison_data)) {
      next
    }
    
    for(i in 1:nrow(comparison_data)) {
      row_data <- comparison_data[i,]
      
      if(!"p_BH" %in% names(row_data)) {
        next
      }
      
      # Extract item number more safely
      item_str <- rownames(comparison_data)[i]
      item_label <- suppressWarnings({
        if (grepl("^TRUST_SCI_", item_str)) {
          sub("^TRUST_SCI_", "", item_str)
        } else {
          NA
        }
      })
      
      if(!is.na(item_label)) {
        result_df <- rbind(result_df, data.frame(
          Comparison = comparison_name,
          Item = item_label,
          Membership = as.character(row_data$Membership),
          Difference = as.numeric(row_data$Difference),
          P_Value = as.numeric(row_data$p),
          P_Value_BH = as.numeric(row_data$p_BH),
          Direction = as.character(row_data$Direction),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if(nrow(result_df) > 0) {
    result_df <- result_df[order(result_df$P_Value),]
  }
  
  return(result_df)
}




item_invariance_summary_4_trust <- function(invariance, ega, alpha = 0.06, total_comparisons = 2278) {
  # Get significant results
  sig_results <- get_significant_results_2_4_trust(invariance$results)
  sig_results_BH <- sig_results[sig_results$P_Value_BH < alpha, ]
  
  # Get unique item names from EGA (assuming these are the column names or item names used)
  item_names <- names(ega$wc)
  
  # Create summary dataframe
  item_summary <- data.frame(
    Item = item_names,
    Non_Invariant_Count = sapply(item_names, function(item) {
      sum(sig_results_BH$Item == item)
    }),
    stringsAsFactors = FALSE
  )
  
  # Add percentages
  item_summary$Percentage <- round(item_summary$Non_Invariant_Count / total_comparisons * 100, 1)
  
  # Sort by frequency
  item_summary <- item_summary[order(-item_summary$Non_Invariant_Count), ]
  
  # Add community information
  item_summary$Community <- ega$wc[item_summary$Item]
  
  # Create formatted table
  formatted_table <- knitr::kable(item_summary,
                                  col.names = c("Item", "Non-Invariant Comparisons", 
                                                "% of Comparisons", "Group"),
                                  caption = "Frequency of Non-Invariance by Item")
  
  # Print table
  print(formatted_table)
  
  # Return invisible summary
  invisible(item_summary)
}

partial_invariance_2_4_trust <- function(invariance, alpha = 0.05) {
  # Get significant results
  sig_results <- get_significant_results_2_4_trust(invariance$results)
  sig_results_BH <- sig_results[sig_results$P_Value_BH < alpha, ]
  
  # Create vectors of all items by membership
  all_items <- list(
    Group1 = names(invariance$membership[invariance$membership == 1]),
    Group2 = names(invariance$membership[invariance$membership == 2]),
    Group3 = names(invariance$membership[invariance$membership == 3])
  )
  
  # For each group, find items that never show significant differences
  invariant_items <- list()
  for(group in 1:3) {
    group_items <- all_items[[group]]
    # Find which items from this group appear in significant results
    significant_items <- unique(paste0("TRUST_SCI_", sig_results_BH$Item))
    # Items that never show significant differences
    invariant_items[[group]] <- setdiff(group_items, significant_items)
  }
  
  # Check if each group has at least 2 invariant items
  invariant_counts <- sapply(invariant_items, length)
  partial_invariance_supported <- all(invariant_counts >= 2)
  
  # Print results
  cat("\nPARTIAL INVARIANCE CHECK BY GROUP\n")
  cat("================================\n")
  for(group in 1:3) {
    cat(sprintf("\nGroup %d:\n", group))
    cat(sprintf("- Number of invariant items: %d\n", invariant_counts[group]))
    cat("- Invariant items:", paste(invariant_items[[group]], collapse=", "), "\n")
  }
  
  cat(sprintf("\nPartial invariance is %s (criterion: ≥2 invariant items per group)\n",
              ifelse(partial_invariance_supported, "supported", "not supported")))
  
  list(
    supported = partial_invariance_supported,
    invariant_items = invariant_items,
    invariant_counts = invariant_counts
  )
}
