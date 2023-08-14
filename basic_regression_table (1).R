library(labeled)

#IMPORTANT: spot-check ALL math and ORs in table! Results may get skewed if, for example, a non-binary variable is included as an adjustment variable in the model
#If controlling for another variable, the OR will not be hand-calculable from the odds for each group, therefore, I recommend running without the control variable to verify accuracy
#This code accepts numeric and factor variables. Errors should result for all other types of variables
#The outcome is expected to be binary (factor or numeric). This can be fairly easily adapted for continuous outcomes, however

logistic_regression_table <- function(data_frame, variables, outcome, control_for = NULL){
  #variables is a character vector of variable names present in data_frame. Important: all numeric values shoudl be in numeric format and binary or categorical variables should be factors. If you prefer, you can alter the function to convert character variables to factors in the script
  #outcome is a character vector of the outcome of interest (a column in data_frame) - the variable should be BINARY with 0-1 or factor coding
  #control_for is an optional character vector of a BINARY variable to control for (a column in data_frame). Currently, only one variable is accepted, but this can be easily adapted to include more
  #make sure "outcome" and "control_for" are not included in variables you pass to the function
  
  
  print(paste("Logistic regression of ", outcome))
  
  regression.df <- data.frame()
  
  for(variable in variables){
    print(variable)
    if(is.null(control_for)){
      formula <- as.formula(paste0(outcome, " ~ ", variable))
    } else {
      formula <- as.formula(paste0(outcome, " ~ ", variable, " + ", control_for))
    }
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
    if(!(is.null(control_for))){ #The last row corresponds the the variabe being controled for - remove this
      effect <- effect[-length(effect)] #exclude the value corresponding to region
    }
    
    if(is.factor(variable2)){
      total <- sum(!is.na(variable2))
      count <- table(variable2)
      percent <- paste0(round(100*count/total, 1), "%")
      proportion <- paste0(count, "/", total, " = ", percent)
      
      count1 <- table(data_frame[,variable, drop = TRUE], data_frame[,outcome])[,2]
      count2 <- table(data_frame[,variable, drop = TRUE], data_frame[, outcome])[,1]
      odds1 <- round(count1/count2, 2)
      odds <- paste0(count1, "/", count2, " = ", odds1)
      
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




# EXAMPLE
#Generate dummy data
Iris <- get("iris")
is.factor(Iris$Species) #Verify all variables are numeric or factors
Iris$big_sepal <- as.factor(sample(c(0,1), nrow(Iris), replace = TRUE)) #Create a fake outcome variable
table(Iris$big_sepal, Iris$Sepal.Length)
table(Iris$big_sepal, Iris$Species)
Iris_vars <- colnames(Iris)[!(colnames(Iris) %in% c("big_sepal", "Petal.Length"))] #include all variables except outcome and control_variable in variable list

Iris_regression <- logistic_regression_table(data_frame = Iris, variables = Iris_vars, outcome= "big_sepal")
print(Iris_regression)
#Note: odds of 1 is whatever outcome category was assigned value 1 in the 0-1 coding
#The output allows you to hand-check some of the ORs (you can check counts against table1)

Iris_regression_controled_for_petal_length <- logistic_regression_table(data_frame = Iris, variables = Iris_vars, outcome= "big_sepal", control_for = "Petal.Length")
print(Iris_regression_controled_for_petal_length)
#You can't hand check the ORs here because we've added a control variable; however, by running first without the control variable, we should be able to spot any problems.