library(ggplot2)
library(dplyr)
library(gridExtra)
library(broom)
library(patchwork)



X <- cityhomes  # Definere X som variable for datakilden


# Plot for StorGrund
Storgrund <- ggplot(data = X, aes(x = as.factor(StorGrund))) +
  geom_bar(colour = "black", fill = "gray") +
  xlab("Big Ground") +
  ylab("Count") +
  ggtitle("Ground")

# Plot for Areal_Grund
X <- X %>%
  mutate(Areal_Grund_Cat = case_when(
    is.na(Areal_Grund) ~ "Missing",
    Areal_Grund < 200 ~ "< 200",
    Areal_Grund >= 200 & Areal_Grund < 400 ~ "200-399",
    Areal_Grund >= 400 & Areal_Grund < 600 ~ "400-599",
    Areal_Grund >= 600 ~ ">= 600"
  ))

# Plot for Areal_Grund
Areal_grund_plot <- ggplot(data = X, aes(x = Areal_Grund_Cat)) +
  geom_bar(colour = "black", fill = "gray") +
  xlab("Land Area (sqm)") +
  ylab("Count") +
  ggtitle("Distribution of Land Area (with Missing Values)")



# Plot for Antal Rum
Antal_rum <- ggplot(data = X, aes(x = as.factor(Ejd_AntalRum))) +
  geom_bar(colour = "black", fill = "gray") +
  xlab("Rooms") +
  ylab("Count") +
  ggtitle("Rooms")

# Kombinere plotsene
grid.arrange(Storgrund, Areal_grund_plot, Antal_rum, ncol = 1)


#### EVT ny R-fil herfra



# Fjerner rækker, hvor der mangler værdier i Areal_Bolig og Antal rum
df_clean <- na.omit(X, cols = c("Areal_Bolig", "Ejd_AntalRum"))

# Box plot for Antal rum ift arealet af huset
ggplot(df_clean, aes(x = as.factor(Ejd_AntalRum), y = Areal_Bolig)) +
  geom_boxplot() +
  xlab("Number of Rooms") +
  ylab("Living Area (sqm)") +
  ggtitle("Box Plot of Living Area by Number of Rooms")

plot(ggplot)



# Plot for komunenavn ift prisen med en rød prik, som viser den gennemsnitsligpris for kommunen
p <- ggplot(df_clean, aes(x = as.factor(KommuneNavn), y = Pris_Salg)) +
  geom_boxplot() +
  geom_point(stat = "summary", fun = "mean", color = "red", size = 3) +  # Add average price points
  xlab("City") +
  ylab("Price") +
  ggtitle("Box Plot of Price by City")

# Print the plot
print(p)

# Calculate boxplot statistics. Dette betyder er at den printer første variabel, hvilket er pris_salg
boxplot_stats <- ggplot_build(p)$data[[1]]

# Print the boxplot statistics
print(boxplot_stats)

# Calculate and print the average price for each Kommune
average_prices <- aggregate(Pris_Salg ~ KommuneNavn, data = df_clean, FUN = mean)
print(average_prices)



#### Laver R2 værdi ift. boxplot
# Convert 'Ejd_AntalRum' to numeric if it's not already
X$Ejd_AntalRum <- as.numeric(as.character(X$Ejd_AntalRum))


# Performing linear regression
model <- lm(Areal_Bolig ~ Ejd_AntalRum, data = df_clean)

# Getting the summary of the model
model_summary <- summary(model)
print(model_summary)


# Coefficient of Determination (R-squared) for den lineær regression af Areal_bolig ift Antalrum
r_squared <- model_summary$r.squared
# Printing the R-squared value
print(r_squared)


##### Laver to mængder A og B


# Defining our dataset as "data"
data <- cityhomes

# Fjerner variablerne, som der ikke skal bruges, så dem i koden nedeunder er de eneste tilstede
variables_to_keep <- c("Pris_Salg", "Areal_Bolig", "Areal_Grund", "Dist_skole", "Dist_raadhus", "Trend", "Velhavende", "KommuneNavn","Alder")
filtered_data <- na.omit(data[variables_to_keep])


# Logarithmically scale the 'Areal_Grund' variable
filtered_data$Areal_Grund <- log(filtered_data$Areal_Grund)

# Set a seed for reproducibility (Sikre at vi får samme mængde hver gang)
set.seed(1)

# Randomly sample indices for 10% of the data
indices <- sample(1:nrow(filtered_data), size = 0.1 * nrow(filtered_data), replace = FALSE)

# Split the data into sets A and B containing 10% and 90% of the data respectively
set_A <- filtered_data[indices, ]
set_B <- filtered_data[-indices, ]

# Output the number of rows in each set to confirm the split
cat("Set A has", nrow(set_A), "rows\n")
cat("Set B has", nrow(set_B), "rows")


####Laver Lineær regression af variablerne i mængden B, hvor Pris_Salg er den afhængige
initial_model <- lm(Pris_Salg ~ ., data = set_B)
summary(initial_model)


# Finder alle uafhængige variabler i mængden B
independent_vars <- names(set_B)[names(set_B) != "Pris_Salg"]




repeat {
  # Perform linear regression
  formula <- as.formula(paste("Pris_Salg ~", 
  paste(independent_vars, collapse = " + ")))
  model <- lm(formula, data = set_B)
  
  # Perform t-tests and get p-values
  p_values <- summary(model)$coefficients[-1, 4]  # Exclude the intercept
  
  # Find the variable with the highest p-value
  max_p_value <- max(p_values)
  variable_to_remove <- names(p_values)[which.max(p_values)]
  
  print(variable_to_remove)
  
  # Check if the max p-value is greater than 0.05
  if (max_p_value > 0.05) {
    # Remove the variable with the highest p-value
    independent_vars <- setdiff(independent_vars, variable_to_remove)
  } else {
    # If all p-values are 0.05 or less, break out of the loop
    break
  }
}
# Final model after removing variables with p-values greater than 0.05
final_model <- model
summary(final_model)



### Laver reset test
# Assuming set_B is your dataframe

# Step 1: Prepare the Model Formula
# Exclude 'Dist_skole', "Pris_Salg" and "Alder" from the independent variables
independent_var <- setdiff(names(set_B), c("Pris_Salg", "Dist_skole","Alder"))
formula <- as.formula(paste("Pris_Salg ~", paste(independent_var, collapse = " + ")))

# Step 2: Fit the Linear Model
model <- lm(formula, data = set_B)  # Definere den nye model
summary(model)      



# Step 3: Perform RESET Test Using lmtest Package
reset_test <- resettest(model, power = 2:3)  # Again, ensure consistency in model reference
print(reset_test)


### Vi laver en ny regression

is_numeric_variable <- function(variable) {
  return(is.numeric(variable))
}

is_binary_variable <- function(variable) {
  unique_values <- unique(variable)
  return(all(unique_values %in% c(0, 1)) && length(unique_values) == 2)
}

# Identify independent variables
independent_vars <- names(set_B)[names(set_B) != "Pris_Salg"]

# Initialize lists for different types of variables
binary_vars <- character(0)
non_binary_numeric_vars <- character(0)

# Iterate over independent variables
for (var_name in independent_vars) {
  if (is_binary_variable(set_B[[var_name]])) {
    binary_vars <- c(binary_vars, var_name)
  } else if (is_numeric_variable(set_B[[var_name]])) {
    non_binary_numeric_vars <- c(non_binary_numeric_vars, var_name)
  }
}

for (var_name in non_binary_numeric_vars) {
  squared_name <- paste0(var_name, "_squared")
  cubed_name <- paste0(var_name, "_cubed")
  
  set_B[[squared_name]] <- set_B[[var_name]]^2
  set_B[[cubed_name]] <- set_B[[var_name]]^3
}


# Construct the formula string for the initial model
formula_str <- "Pris_Salg ~ KommuneNavn + Velhavende + "  # Add non-transformed variables
formula_str <- paste(formula_str, paste(binary_vars, collapse = " + "), " + ")
formula_str <- paste(formula_str, paste(non_binary_numeric_vars, collapse = " + "), " + ")
formula_str <- paste(formula_str, paste(paste0(non_binary_numeric_vars, "_squared"), collapse = " + "), " + ")
formula_str <- paste(formula_str, paste(paste0(non_binary_numeric_vars, "_cubed"), collapse = " + "))




# Fit the initial model


Poly_model <- lm(as.formula(formula_str), data = set_B)
summary(Poly_model)

# Begin backward elimination process
repeat {
  # Calculate the summary of the model
  summary_model <- summary(Poly_model)
  
  # Get the p-values of the predictors, excluding the intercept
  p_values <- summary_model$coefficients[-1, "Pr(>|t|)"]
  
  # Find the predictor with the highest p-value
  max_p_value <- max(p_values)
  predictor_to_remove <- names(p_values)[which.max(p_values)]
  
  # Check if the max p-value is greater than 0.05
  if (max_p_value > 0.05) {
    # Print the predictor being removed
    cat("Removing variable:", predictor_to_remove, "with p-value:", max_p_value, "\n")
    
    # Rebuild the model formula without the predictor
    current_formula <- formula(Poly_model)
    terms_to_keep <- setdiff(all.vars(current_formula), predictor_to_remove)
    new_formula <- as.formula(paste("Pris_Salg ~", paste(terms_to_keep[-1], collapse = " + ")))
    
    # Refit the model without the predictor
    Poly_model <- lm(new_formula, data = set_B)
  } else {
    # Exit the loop if no p-values are greater than 0.05
    break
  }
}

# Print the summary of the final model
summary(Poly_model)
final_model <- Poly_model


# Step 3: Perform RESET Test Using lmtest Package
reset_test <- resettest(Poly_model, power = 2:3)  # Again, ensure consistency in model reference
print(reset_test)


##### Sektion for Normalitet af fejlledene
# Assuming you have already calculated the quantiles and the slo
final_model_res <- rstandard(final_model)
y <- quantile(final_model_res, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y) / diff(x)
int <- y[1] - slope * x[1]

# Create the QQ plot using ggplot2
QQplot_standard <- ggplot() +
  geom_qq(aes(sample = final_model_res)) +
  xlab("Theoretical quantiles") +
  ylab("Standardized Residuals") +
  geom_abline(intercept = as.numeric(int), slope = as.numeric(slope), color = "maroon", size = 0.5) +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 20)
  )

# Display the QQ plot
print(QQplot_standard)




### QQplot, hvor det er log af salgsprisen
# Extract the formula used in the model
model_formula <- formula(final_model)

# Create a new data frame with only the variables used in the model
# Note: We need to include the dependent variable 'Pris_Salg' as it's not part of the formula
set_B_log <- set_B[, c("Pris_Salg", all.vars(model_formula))]

# Apply logarithmic transformation to Pris_Salg in the new data frame
set_B_log$Pris_Salg_log <- log(set_B_log$Pris_Salg)

# Optionally, if you don't need the original Pris_Salg, you can remove it
set_B_log$Pris_Salg <- NULL


# Fit the linear model using the log-transformed Pris_Salg
final_model_log <- lm(Pris_Salg_log ~ ., data = set_B_log)

# Extract the residuals from the model (standardlized residuals)
residuals_log <- rstandard(final_model_log)


y <- quantile(residuals_log, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y) / diff(x)
int <- y[1] - slope * x[1]

# Create the QQ plot(LOG) using ggplot2
p1 <- ggplot() +
  geom_qq(aes(sample = residuals_log)) +
  xlab("Theoretical quantiles") +
  ylab("Log of the Standardized Residuals") +
  geom_abline(intercept = as.numeric(int), slope = as.numeric(slope), color = "maroon", size = 0.5) +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 20)
  )
print(p1)


### Laver JB test for log og  normal.
jb_test <- jarque.bera.test(final_model$residuals)
print(jb_test)

jb_test_log <- jarque.bera.test(final_model_log$residuals)
print(jb_test_log)


### Historgrammer med reisudals, hvor vi kigger på salsprisen. 


# Calculate mean and standard deviation of residuals
residuals <- rstandard(final_model)
mean_residuals <- mean(residuals)
sd_residuals <- sd(residuals)


# Create the Histogram plot using ggplot2 with automatic bin width
p2 <- ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), fill = "blue", color = "black", alpha = 0.7,binwidth = 0.4) + # Set y to density for the histogram
  stat_function(fun = dnorm, args = list(mean = mean_residuals, sd = sd_residuals), color = "red", size = 1) + # Add normal distribution curve
  xlab("Standardlized Residuals") +
  ylab("Density") +
  ggtitle("Histogram of Residuals with Normal Distribution PDF") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma, limits = c(-5, 5)) + # Adjust x-axis limits to -10 to 10
  scale_y_continuous(expand = expansion(mult = c(0, .1))) # This adds a little space at the top of the bars

# Display the Histogram plot
print(p2)


### Laver log-histogram af residualerne
 
 # Calculate mean and standard deviation of the residuals
 mean_res <- mean(residuals_log)
 sd_res <- sd(residuals_log)
 
 # Create the Histogram plot using ggplot2
 p2_log <- ggplot(data.frame(residuals_log), aes(x = residuals_log)) +
   geom_histogram(aes(y = ..density..), fill = "blue", color = "black", alpha = 0.7,binwidth = 0.4) + # Automatic bin width
   stat_function(fun = dnorm, args = list(mean = mean_res, sd = sd_res), color = "red", size = 1) + # Add normal distribution curve
   xlab("Standardlized Residuals(log10)") +
   ylab("Density") +
   ggtitle("Histogram of log Residuals with Normal Distribution PDF") +
   theme_minimal() + xlim(-5, 5) # Setting x-axis limits
 
 print(p2_log)
 
 
## Kombinere scatterplotsene i et billede
scatter_plot <- p2 + theme(plot.title = element_blank())
scatter_plot1 <- p2_log + theme(plot.title = element_blank())

# Combine the plots
combined_plot <- scatter_plot | scatter_plot1

# Add a shared title
combined_plot <- combined_plot & plot_annotation(title = "Histogram with Standardlized residiuals vs density")

# Display the combined plot
combined_plot






### Laver t-tet for logaritme:
# I am assuming that set_B_log has been created properly earlier in your R session
# and it includes Pris_Salg_log as the dependent variable and all other predictors.

# Identify independent variables in set_B_log
independent_vars <- names(set_B_log)[!names(set_B_log) %in% c("Pris_Salg_log", "Pris_Salg.1")]

# Initialize formula with only independent variables
formula_text <- paste("Pris_Salg_log ~", paste(independent_vars, collapse = " + "))


formula <- as.formula(formula_text)
model <- lm(formula,data = set_B_log)
summary(model)




repeat {
  formula <- as.formula(formula_text)
  model <- lm(formula, data = set_B_log)
  
  # Perform t-tests and get p-values
  p_values <- summary(model)$coefficients[-1, 4]  # Exclude the intercept
  
  # Check if any variable (including squared and cubed) has p-value > 0.05
  if (all(p_values <= 0.05, na.rm = TRUE)) {
    break  # Exit loop if all variables' p-values are ≤ 0.05
  }
  
  # Identify variable with highest p-value
  variable_to_remove <- names(p_values)[which.max(p_values)]
  cat("Removed variable:", variable_to_remove, "\n")
  
  # Remove the identified variable from the formula
  formula_text <- gsub(paste0("\\+?\\s*", variable_to_remove, "\\s*"), "", formula_text)
  
}

# Final model after removing variables with p-values greater than 0.05
final_model_log <- lm(formula, data = set_B_log)
summary(final_model_log)


## BP TEST
bp_test <- bptest(final_model)
print(bp_test)



# Calculate standardized residuals
# Ensure final_model_log is your linear model object

residuals_final_model <- residuals(final_model_log)

### Standardlized residuals af final_model_log
std_residuals <- rstandard(final_model_log)

# Get fitted values
fitted_values <- fitted(final_model_log)




# Create a data frame for plotting
plot_data <- data.frame(Fitted = fitted_values, residuals = residuals)

# Create a scatter plot with ggplot2
scatter_plot1 <- ggplot(plot_data, aes(x = Fitted, y = residuals)) +
  geom_point() +
  geom_smooth((aes(y = residuals)), method = "lm", se = FALSE) +  # Added linear regressuib
  theme_minimal() +
  labs(x = "Fitted Values", y = "Residuals", title = "Scatterplot of Standardized Residuals vs Fitted Values")
# Display the scatter plot
print(scatter_plot1)



# Scatterplot for squaroot of the absolout value of standardlized residuals

std_residuals_1 <- sqrt(abs(std_residuals))

plot_data <- data.frame(Fitted = fitted_values, residuals = std_residuals_1)

# Create a scatter plot with ggplot2
scatter_plot <- ggplot(plot_data, aes(x = Fitted, y = residuals)) +
  geom_point() +
  geom_smooth((aes(y = residuals)), method = "lm", se = FALSE) +  # Added geom_smooth
  theme_minimal() +
  labs(x = "Fitted Values", y = "Square root of the absolout value of Standardlized Residuals", title = "Scatterplot of Standardized Residuals vs Fitted Values")
# Display the scatter plot
print(scatter_plot)




# Assuming scatter_plot and scatter_plot1 are your ggplot objects
# Remove individual titles
scatter_plot <- scatter_plot + theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"))
scatter_plot1 <- scatter_plot1 + theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"))

combined_plot <- scatter_plot | scatter_plot1
combined_plot


# Combine the plots
combined_plot <- scatter_plot | scatter_plot1

# Add a shared title
combined_plot <- combined_plot & plot_annotation(title = "Shared Title for Both Plots")

# Display the combined plot
combined_plot



summary(final_model_log)


#### PREDICTION


set_A$Dist_raadhus_squared <- set_A$Dist_raadhus^2
set_A$Areal_Bolig_cubed <- set_A$Areal_Bolig^3
set_A$Alder_squared <- set_A$Alder^2
set_A$Areal_Grund_cubed <- set_A$Areal_Grund^3
set_A$Dist_raadhus_cubed <- set_A$Dist_raadhus^3
set_A$Alder_cubed <- set_A$Alder^3
set_A$Areal_Bolig_squared <- set_A$Areal_Bolig^2
set_A$Areal_Grund_squared <- set_A$Areal_Grund^2
set_A$Dist_skole_squared <- set_A$Dist_skole^2
set_A$Dist_skole_cubed <- set_A$Dist_skole^3
set_A$Trend_cubed <- set_A$Trend^3

# Assuming you've done a log transformation during model training,
# and now you want to transform the predictions back to the original scale.
set_A$predictions <- exp(predict(final_model_log, newdata = set_A))

# Sort set_A by predicted prices on the original scale to align with your plot.
set_A_sorted <- set_A[order(set_A$predictions),]


# Create the plot, ensuring to use the unlogged 'Pris_Salg' and 'predictions'
gg <- ggplot(set_A_sorted, aes(x = seq_along(predictions))) + 
  geom_point(aes(y = Pris_Salg), colour = "black", size = 2) +
  geom_smooth(aes(y = predictions), colour = "red",method = "lm", formula = y ~ poly(x, 3), size = 1,se=FALSE) +
  labs(x = "Houses sorted by increasing predicted prices",
       y = "House prices",
       title = "Comparison of Actual and Predicted House Prices") +
  theme_minimal() +
  theme(legend.position = "none") # Remove the legend if it's not needed

# Print the plot
print(gg)


# Calculate the differences
set_A$diff <- set_A$Pris_Salg - set_A$predictions

# Calculate the average difference
average_diff <- mean(set_A$diff)

# If you want to consider only the absolute difference
average_abs_diff <- mean(abs(set_A$diff))

# Print the average difference
average_abs_diff




# Calculate the percentage differences
set_A$percentage_diff <- (set_A$Pris_Salg - set_A$predictions) / set_A$Pris_Salg * 100

# Calculate the average percentage difference
average_percentage_diff <- mean(set_A$percentage_diff)

# If you want to consider only the absolute percentage difference
average_abs_percentage_diff <- mean(abs(set_A$percentage_diff))

# Print the average percentage difference
average_abs_percentage_diff




### Prediction med confidence interval


# Create the plot, ensuring to use the unlogged 'Pris_Salg' and 'predictions'
gg <- ggplot(set_A_sorted, aes(x = seq_along(predictions))) + 
  geom_point(aes(y = Pris_Salg), colour = "black", size = 2) +
  geom_smooth(aes(y = predictions), colour = "red",method = "lm", formula = y ~ poly(x, 3), size = 1,se=TRUE) +
  labs(x = "Houses sorted by increasing predicted prices",
       y = "House prices",
       title = "Comparison of Actual and Predicted House Prices") +
  theme_minimal() +
  theme(legend.position = "none") # Remove the legend if it's not needed

# Print the plot
print(gg)



### Prediction with interval
# Make predictions with a 95% prediction interval
predictions_with_prediction <- predict(final_model_log, newdata = set_A, interval = "prediction", level = 0.95)




# Add the prediction interval to set_A
set_A$pred_lwr <- exp(predictions_with_prediction[, "lwr"])
set_A$pred_upr <- exp(predictions_with_prediction[, "upr"])
set_A$fit <- exp(predictions_with_prediction[, "fit"])

# Sort set_A by the fit values (or any other sorting criteria you have)
set_A_sorted <- set_A[order(set_A$fit),]


# Create the plot with confidence intervals and prediction intervals
gg <- ggplot(set_A_sorted, aes(x = seq_along(fit))) + 
  geom_ribbon(aes(ymin = pred_lwr, ymax = pred_upr), alpha = 0.1, fill = "darkgreen", color = NA) +  # Prediction interval
  geom_point(aes(y = Pris_Salg), colour = "black", size = 2) +
  geom_smooth(aes(y = fit), colour = "red",method = "lm", formula = y ~ poly(x, 3), size = 1,se = TRUE) +
  labs(x = "Houses sorted by increasing predicted prices",
       y = "House prices",
       title = "Comparison of Actual and Predicted House Prices with 95% CI and PI") +
  theme_minimal() +
  theme(legend.position = "none")

# Print the plot
print(gg)

# Calculate the average lower and upper bounds in percentage
set_A$pred_lwr_perc <- 100 * (set_A$fit - set_A$pred_lwr) / set_A$fit
set_A$pred_upr_perc <- 100 * (set_A$pred_upr - set_A$fit) / set_A$fit

average_pred_lwr_perc <- mean(set_A$pred_lwr_perc, na.rm = TRUE)
average_pred_upr_perc <- mean(set_A$pred_upr_perc, na.rm = TRUE)


cat("Average lower bound percentage: ", average_pred_lwr_perc, "%\n")
cat("Average upper bound percentage: ", average_pred_upr_perc, "%\n")









### Removing Outliers for the set_B 
#### Funktion til at sætte alle udtrykkene i anden(Ikke binary)
### Rember to run the code for the set_B again here 


is_numeric_variable <- function(variable) {
  return(is.numeric(variable))
}

is_binary_variable <- function(variable) {
  unique_values <- unique(variable)
  return(all(unique_values %in% c(0, 1)) && length(unique_values) == 2)
}

# Identify independent variables
independent_vars <- names(set_B)[names(set_B) != "Pris_Salg"]

# Initialize lists for different types of variables
binary_vars <- character(0)
non_binary_numeric_vars <- character(0)

# Iterate over independent variables
for (var_name in independent_vars) {
  if (is_binary_variable(set_B[[var_name]])) {
    binary_vars <- c(binary_vars, var_name)
  } else if (is_numeric_variable(set_B[[var_name]])) {
    non_binary_numeric_vars <- c(non_binary_numeric_vars, var_name)
  }
}

for (var_name in non_binary_numeric_vars) {
  squared_name <- paste0(var_name, "_squared")
  cubed_name <- paste0(var_name, "_cubed")
  
  set_B[[squared_name]] <- set_B[[var_name]]^2
  set_B[[cubed_name]] <- set_B[[var_name]]^3
}


# Construct the formula string for the initial model
formula_str <- "log(Pris_Salg) ~ KommuneNavn + Velhavende + "  # Add non-transformed variables
formula_str <- paste(formula_str, paste(binary_vars, collapse = " + "), " + ")
formula_str <- paste(formula_str, paste(non_binary_numeric_vars, collapse = " + "), " + ")
formula_str <- paste(formula_str, paste(paste0(non_binary_numeric_vars, "_squared"), collapse = " + "), " + ")
formula_str <- paste(formula_str, paste(paste0(non_binary_numeric_vars, "_cubed"), collapse = " + "))
print(non_binary_numeric_vars)



# Fit the initial model
Outliers_model <- lm(as.formula(formula_str), data = set_B)
summary(Outliers_model)

# Begin backward elimination process
repeat {
  # Calculate the summary of the model
  summary_model <- summary(Outliers_model)
  
  # Get the p-values of the predictors, excluding the intercept
  p_values <- summary_model$coefficients[-1, "Pr(>|t|)"]
  
  # Find the predictor with the highest p-value
  max_p_value <- max(p_values)
  predictor_to_remove <- names(p_values)[which.max(p_values)]
  
  # Check if the max p-value is greater than 0.05
  if (max_p_value > 0.05) {
    # Print the predictor being removed
    cat("Removing variable:", predictor_to_remove, "with p-value:", max_p_value, "\n")
    
    # Rebuild the model formula without the predictor
    current_formula <- formula(Outliers_model)
    terms_to_keep <- setdiff(all.vars(current_formula), predictor_to_remove)
    new_formula <- as.formula(paste("log10(Pris_Salg) ~", paste(terms_to_keep[-1], collapse = " + ")))
    
    # Refit the model without the predictor
    Outliers_model <- lm(new_formula, data = set_B)
  } else {
    # Exit the loop if no p-values are greater than 0.05
    break
  }
}

# Print the summary of the final model
summary(Outliers_model)


residual_outliers <- residuals(Outliers_model)
jarque.bera.test(outliers) #JB-test

#################


cooks_dist <- cooks.distance(final_model) #Cooks på model
threshold <- 4 / length(cooks_dist) # Definere threshold
outliers <- which(cooks_dist > threshold)
plot(cooks_dist, pch=19, main="Cook's Distance", ylab="Cook's Distance", xlab="Index")
abline(h=threshold, col="red")
points(outliers, cooks_dist[outliers], pch=19, col="blue")


# Assuming 'set_B" is the datafram and 'Pris_Salg' is a column in that dataframe
print(set_B$Pris_Salg[outliers])




# Extract variable names from the final model formula
model_formula <- formula(final_model)
model_vars <- all.vars(model_formula)

# Ensure only original variables are selected 
original_vars <- model_vars[model_vars %in% names(set_B)]

# Select only the original variables from set_B and remove outliers
set_B_clean <- set_B[-outliers, original_vars]


# Create a new model without the outliers
new_model = lm(log(Pris_Salg) ~ ., data = set_B_clean)

new_model_res = rstandard(new_model)

jarque.bera.test(new_model_res)



y <- quantile(new_model_res, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y) / diff(x)
int <- y[1] - slope * x[1]

# Create the QQ plot using ggplot2
p1 <- ggplot() +
  geom_qq(aes(sample = new_model_res)) +
  xlab("Theoretical quantiles") +
  ylab("Standardized Residuals") +
  geom_abline(intercept = as.numeric(int), slope = as.numeric(slope), color = "maroon", size = 0.5) +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 20)
  )

# Display the QQ plot
print(p1)

## BP test for den nye model
bptest(new_model)





# Calculate the standard deviation of residuals
sd_res <- sd(new_model_res)

# Calculate the mean of residuals
mean_res <- mean(new_model_res)

# Plot histogram of residuals
Hist_Cooks <- ggplot(data.frame(Residuals = new_model_res), aes(x = new_model_res)) +
  geom_histogram(aes(y = ..density..), fill = "blue", color = "black", alpha = 0.7, bins = 30) + 
  stat_function(fun = dnorm, args = list(mean = mean_res, sd = sd_res), color = "red", size = 1) +
  xlab("Standardized Residuals") +
  ylab("Density") +
  ggtitle("Histogram of log Residuals with Normal Distribution PDF") +
  theme_minimal() +
  xlim(-4, 4) # Setting x-axis limits

print(Hist_Cooks)



grid.arrange(p1, Hist_Cooks, ncol = 2)



# Polynomial features to the set A
set_A$Dist_raadhus_squared <- set_A$Dist_raadhus^2
set_A$Areal_Bolig_cubed <- set_A$Areal_Bolig^3
set_A$Alder_squared <- set_A$Alder^2
set_A$Areal_Grund_cubed <- set_A$Areal_Grund^3
set_A$Dist_raadhus_cubed <- set_A$Dist_raadhus^3
set_A$Alder_cubed <- set_A$Alder^3
set_A$Areal_Bolig_squared <- set_A$Areal_Bolig^2
set_A$Areal_Grund_squared <- set_A$Areal_Grund^2
set_A$Dist_skole_squared <- set_A$Dist_skole^2
set_A$Dist_skole_cubed <- set_A$Dist_skole^3


summary(new_model)

# Assuming set_A is your new data frame where you want to make predictions

# Assuming set_A is your new data frame where you want to make predictions

# Assuming `new_model` is a model object that was fit using a log-transformed outcome variable.

# Generate predictions on the original scale (if the model was fit to log(y))
set_A$predictions <- exp(predict(new_model, newdata = set_A))

# Sort set_A by predicted prices on the original scale
set_A_sorted <- set_A[order(set_A$predictions), ]

# Generate prediction intervals on the log scale and then transform back to the original scale
log_pred_intervals <- predict(new_model, newdata = set_A_sorted, interval="prediction", level=0.95)
set_A_sorted$lower <- exp(log_pred_intervals[,"lwr"])
set_A_sorted$upper <- exp(log_pred_intervals[,"upr"])

# Create the plot using set_A_sorted
gg <- ggplot(set_A_sorted, aes(x = seq_along(predictions), y = Pris_Salg)) +
  geom_point(colour = "black", size = 2) +
  geom_smooth(aes(y = predictions), method = "lm", formula = y ~ poly(x, 3), colour = "red", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,fill = "limegreen",color = NA) + # Use the same x aesthetic for geom_ribbon
  labs(x = "Houses sorted by increasing predicted prices",
       y = "House prices",
       title = "Comparison of Actual and Predicted House Prices with Prediction Intervals") +
  theme_minimal() +
  theme(legend.position = "none")

print(gg)



# Assuming `new_model` is a model object and other initializations have been made as per your script

# Calculate absolute and percentage differences
set_A_sorted$difference = abs(set_A_sorted$Pris_Salg - set_A_sorted$predictions)
set_A_sorted$percent_difference = (set_A_sorted$difference / set_A_sorted$Pris_Salg) * 100

# Calculate average difference in number and percentage
average_difference = mean(set_A_sorted$difference)
average_percent_difference = mean(set_A_sorted$percent_difference)

# Calculate average bounds as percent of predicted value
average_upper_bound_percent = (mean(set_A_sorted$upper) / mean(set_A_sorted$predictions)) * 100
average_lower_bound_percent = (mean(set_A_sorted$lower) / mean(set_A_sorted$predictions)) * 100

# Output the results
cat("Average Difference: ", average_difference, "\n")
cat("Average Percentage Difference: ", average_percent_difference, "%\n")
cat("Average Upper Bound (Percent of Predicted): ", average_upper_bound_percent, "%\n")
cat("Average Lower Bound (Percent of Predicted): ", average_lower_bound_percent, "%\n")

summary(new_model)



#### Plots for at tjekke om homosecidasity er opfyldt

residuals_final_model <- residuals(new_model)


std_residuals <- rstandard(new_model)

# Get fitted values
fitted_values <- fitted(new_model)

residuals = residuals(new_model)


# Create a data frame for plotting
plot_data <- data.frame(Fitted = fitted_values, residuals = residuals)

# Create a scatter plot with ggplot2
scatter_plot1 <- ggplot(plot_data, aes(x = Fitted, y = residuals)) +
  geom_point() +
  geom_smooth((aes(y = residuals)), method = "lm", se = FALSE) +  # Added geom_smooth
  theme_minimal() +
  labs(x = "Fitted Values", y = "Residuals", title = "Scatterplot of Residuals vs Fitted Values")
# Display the scatter plot
print(scatter_plot1)




# Create a data frame for plotting

std_residuals <- rstandard(new_model)

std_residuals_1 <- sqrt(abs(std_residuals))

plot_data <- data.frame(Fitted = fitted_values, residuals = std_residuals_1)

# Create a scatter plot with ggplot2
scatter_plot <- ggplot(plot_data, aes(x = Fitted, y = residuals)) +
  geom_point() +
  geom_smooth((aes(y = residuals)), method = "lm", se = FALSE) +  # Added geom_smooth
  theme_minimal() +
  labs(x = "Fitted Values", y = "Square root of the absolout value of Standardlized Residuals", title = "Scatterplot of Standardized Residuals vs Fitted Values")
# Display the scatter plot
print(scatter_plot)




# Assuming scatter_plot and scatter_plot1 are your ggplot objects
# Remove individual titles
scatter_plot <- scatter_plot + theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"))
scatter_plot1 <- scatter_plot1 + theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"))

combined_plot <- scatter_plot | scatter_plot1
combined_plot


# Combine the plots
combined_plot <- scatter_plot | scatter_plot1

# Add a shared title
combined_plot <- combined_plot & plot_annotation(title = "Shared Title for Both Plots")

# Display the combined plot
combined_plot








