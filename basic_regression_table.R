logistic_regression_table <- function(data_frame, variables, outcome, control_for = NULL){
  #variables is a character vector of variable names present in data_frame. Important: all numeric values should be in numeric format and binary or categorical variables should be factors. If you prefer, you can alter the function to convert character variables to factors in the script
  #outcome is a character vector of the outcome of interest (a column in data_frame) - the outcome variable should be BINARY with 0-1 or factor coding
  #control_for is an optional character vector of 1 or more columns (in the dataframe) to control for.
  #make sure "outcome" and "control_for" are not included in variables you pass to the function
  
  
  print(paste("Logistic regression of ", outcome))
  
  regression.df <- data.frame()
  
  for(variable in variables){
    print(variable)
    variable_string <- ifelse(is.null(control_for), variable, paste0(variable, " + ", paste0(control_for, collapse = " + ")))
    formula <- as.formula(paste0(outcome, " ~ ", variable_string))
    variable2 <- data_frame[,variable, drop = TRUE]
    if("Hmisc" %in% (.packages()) & !is.null(label(variable2))){ #If variables have a label (see Hmisc package) this will be used for the variable names in the table, otherwise the original variable name will be used 
      var_name <- label(variable2)
    } else {
      var_name <- variable
    }
    my.glm <- glm(formula, data = data_frame, family = "binomial")
    OR <- exp(coef(my.glm))[2:length(coef(my.glm))]
    CI_2.5 <- exp(confint(my.glm))[,1][2:length(coef(my.glm))]
    CI_97.5 <- exp(confint(my.glm))[,2][2:length(coef(my.glm))]
    signif <- ifelse((CI_2.5 > 1 & CI_97.5 > 1) | (CI_2.5 < 1 & CI_97.5 < 1), "*", "")
    effect <- paste0(round(OR,2), " (95% CI: ", round(CI_2.5, 2), "-", round(CI_97.5,2), ")", signif)
    if(!(is.null(control_for))){ #The last rows corresponds the the variabe being controled for - remove these
      N_unique_control_groups <- sum(sapply(data_frame[, control_for],
                                          function(x) length(na.omit(unique(x))))) - length(control_for) #Find total number of unique control_for comparison groups (that's the sum of (number of groups in each control_for variable - 1)
      effect <- effect[-length(effect):-(length(effect) - N_unique_control_groups + 1)] #exclude the values corresponding to control_variables (we add 1 because subsettign is inclusive)
    }
    
    if(is.factor(variable2)){
      total <- sum(!is.na(variable2))
      count <- table(variable2)
      percent <- paste0(round(100*count/total, 1), "%")
      proportion <- paste0(count, "/", total, " = ", percent)
      
      count1 <- table(data_frame[, c(variable, outcome), drop = TRUE])[,2]
      count0 <- table(data_frame[, c(variable, outcome), drop = TRUE])[,1]
      odds1 <- round(count1/count0, 2)
      odds <- paste0(count1, "/", count0, " = ", odds1)
      
      regression.row <- data.frame(variable = c(var_name, rep("", length(levels(variable2))-1)),
                                   values = levels(variable2),
                                   mean_or_proportion = proportion,
                                   odds_of_1 = odds,
                                   OR = rep("-", length(levels(variable2))))
      effect <- append("Ref", effect)
      
    } else {
      mean <- round(mean(variable2, na.rm = TRUE), 2)
      
      regression.row <- data.frame(variable = var_name,
                                   values = "-",
                                   mean_or_proportion = mean,
                                   odds_of_1 = "-", 
                                   OR = "-")
    }
    
    regression.row$OR <- effect
    regression.df <- rbind(regression.df, regression.row)
  }
  
  return(regression.df)
}
