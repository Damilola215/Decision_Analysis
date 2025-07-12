# input_estimates <- read.csv("data/input_estimates.csv")
library(decisionSupport)



communication_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost", "Management_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season", 
                                              "Management costs in a normal season"))



decide_function <- function(){
  income <- Yield * Market_price
  overall_costs <- Labor_cost + Management_cost
  final_result <- income - overall_costs
  return(list(final_result = final_result))
}

seminar_mc_simulation <- decisionSupport::mcSimulation(estimate = as.estimate(communication_estimates),
                                                       model_function = decide_function,
                                                       numberOfModelRuns = 10000,
                                                       functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = seminar_mc_simulation,
                   vars = "final_result",
                   method = "smooth_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

quantile(seminar_mc_simulation$y$final_result)
# Plotting the cashflow:

# Create the estimate object (for multiple options):

variable = c("revenue_option1", "costs_option1", "n_years", 
             "revenue_option2", "costs_option2")
distribution = c("norm", "norm", "const", "norm", "norm")
lower = c(10000,  5000, 20, 8000,  500)
upper = c(100000, 50000, 20, 80000,  30000)

costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)

# Define the model function without name for the return value:

profit1 <- function(x) {
  
  cashflow_option1 <- vv(revenue_option1 - costs_option1, n = n_years, var_CV = 100)
  cashflow_option2 <- vv(revenue_option2 - costs_option2, n = n_years, var_CV = 100)
  
  return(list(Revenues_option1 = revenue_option1,
              Revenues_option2 = revenue_option2,
              Costs_option1 = costs_option1,
              Costs_option2 = costs_option2,
              Cashflow_option_one = cashflow_option1,
              Cashflow_option_two = cashflow_option2))
}

# Perform the Monte Carlo simulation:

predictionProfit1 <- mcSimulation(estimate = costBenefitEstimate,
                                  model_function = profit1,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


# Plot the cashflow distribution over time

plot_cashflow(mcSimulation_object = predictionProfit1, cashflow_var_name = "Cashflow_option_one",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "green4", color_5_95 = "green1",
              color_median = "red")
# Plotting the cashflow:

# Create the estimate object (for multiple options):

variable = c("revenue_option1", "costs_option1", "n_years", 
             "revenue_option2", "costs_option2")
distribution = c("norm", "norm", "const", "norm", "norm")
lower = c(10000,  5000, 10, 8000,  500)
upper = c(100000, 50000, 10, 80000,  30000)

costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)

# Define the model function without name for the return value:

profit1 <- function(x) {
  
  cashflow_option1 <- vv(revenue_option1 - costs_option1, n = n_years, var_CV = 100)
  cashflow_option2 <- vv(revenue_option2 - costs_option2, n = n_years, var_CV = 100)
  
  return(list(Revenues_option1 = revenue_option1,
              Revenues_option2 = revenue_option2,
              Costs_option1 = costs_option1,
              Costs_option2 = costs_option2,
              Cashflow_option_one = cashflow_option1,
              Cashflow_option_two = cashflow_option2))
}

# Perform the Monte Carlo simulation:

predictionProfit1 <- mcSimulation(estimate = costBenefitEstimate,
                                  model_function = profit1,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")

plot_cashflow(mcSimulation_object = predictionProfit1, 
              cashflow_var_name = c("Cashflow_option_one", "Cashflow_option_two"),
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "green4", color_5_95 = "green1",
              color_median = "red", 
              facet_labels = c("Option 1", "Option 2"))
# Create the estimate object:

cost_benefit_table <- data.frame(label = c("Revenue", "Costs"),
                                 variable = c("revenue", "costs"),
                                 distribution = c("norm", "norm"),
                                 lower = c(100,  500),
                                 median = c(NA, NA),
                                 upper = c(10000, 5000))

# (a) Define the model function without name for the return value:

profit1 <- function() {
  Decision <- revenue - costs
  cashflow <- rnorm(rep(revenue, 20))
  return(list(Revenues = revenue,
              Costs = costs, 
              cashflow = cashflow, 
              Decision = Decision))
}

compound_figure(model = profit1, 
                input_table = cost_benefit_table, 
                decision_var_name = "Decision",
                cashflow_var_name = "cashflow",
                model_runs = 1e2, 
                distribution_method = 'smooth_simple_overlay')
