library(decisionSupport)

input_estimates <- data.frame(variable = c("Maggot_cost", "Chicken_Cost", "egg_yield", "Chicken_yield", "Market_price_egg", "Market_price_meat"),
  lower = c(1000, 300, 500, 400, 20, 800),
  median = NA,
  upper = c(3000, 500, 1000, 900, 35, 1500),
  distribution = c("posnorm", "posnorm", "posnorm", "posnorm", "posnorm", "posnorm"),
  label  = c("Maggot_cost (USD /ha)", 
             "Chicken_Cost (USD/ha)", 
             "egg_yield (pieces/batch)", 
             "Chicken_yield (kg/batch)",
             "Market price per egg (USD)",
             "Market price of meat (USD/kg)"),
  Description = c("Maggot_cost under normal conditions",
                  "chicken cost in a normal season",
                  "egg_yield per batch in a normal season",
                  "Yield of Chicken per batch in a normal season",
                  "Market price of eggs",
                  "Market price of chicken meat")
)


model_function <- function(){
  Egg_income <- egg_yield / 10 * Market_price_egg
  Chicken_income <- Chicken_yield * Market_price_meat
  Total_income <- Egg_income + Chicken_income
  
  overall_cost <- Maggot_cost + Chicken_Cost
  
  final_result <- Total_income - overall_cost
  
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 10000,
                                      functionSyntax = "plainNames")


example_mc_simulation


plot_distributions(mcSimulation_object = example_mc_simulation,
                    vars = "final_result",
                    method = "boxplot_density",
                    old_names = "final_result",
                    new_names = "Outcome distribution for profits")
