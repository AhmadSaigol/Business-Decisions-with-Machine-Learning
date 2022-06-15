
assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) {
  
  attrition_col_expr <- enquo(attrition_col)
  
  data %>%
    
    # Use parenthesis () to give tidy eval evaluation priority
    filter((!! attrition_col_expr) %in% attrition_value) %>%
    arrange(desc(pct)) %>%
    mutate(
      # Function inputs in numeric format (e.g. baseline_pct = 0.088 don't require tidy eval)
      above_industry_avg = case_when(
        pct > baseline_pct ~ "Yes",
        TRUE ~ "No"
      )
    )
  
}