library(decisionSupport)
library(ggplot2)

hail_estimates <- data.frame(variable = c("yield", 
                                          "var_CV", 
                                          "initial_investment", 
                                          "price",
                                          "p_hail"),
                             lower = c(6000, 20, 500, 5,0.02),
                             median = NA,
                             upper = c(14000, 20, 1000, 80, 0.2),
                             distribution = c("posnorm", 
                                              "const", 
                                              "posnorm", 
                                              "posnorm",
                                              "posnorm"),
                             label = c("Yield (kg/ha)", 
                                       "Coefficient of variation", 
                                       "Investment cost (USD)", 
                                       "Market price (EUR/kg)",
                                       "% chance hail"),
                             Description = c("Yield under normal conditions",
                                             "Coefficient of variation (measure of relative variability)",
                                             "Investment cost", 
                                             "Market price achieved for yields (EUR/kg)",
                                             "Probability of a hail storm"))

hail_estimates

hail_function <- function(){
  
  # use vv() to add variability to the 
  # random draws of yield and of  price 
  # over a 20 year simulation 
  yields <- vv(var_mean = yield, 
               var_CV = var_CV, 
               n = 20)
  
  prices <- vv(var_mean = price, 
               var_CV = var_CV, 
               n = 20)
  
  # use rep() to simulate the initial_investment 
  # only in the first year (assuming the net lasts 20 years)
  invest_costs <- c(initial_investment, rep(0, 19))
  
  # use p_hail in the chance_event() 
  # to adjust yield for probability of hail
  # assuming no yield at all in the event of hail
  hail_adjusted_yield <- chance_event(chance = p_hail, 
                                      value_if = 0,
                                      value_if_not = yield,
                                      n = 20)
  
  # calculate profit without net
  profit_no_net <- hail_adjusted_yield*prices
  
  # calculate profit with the net
  profit_with_net <- (yields*prices)-invest_costs
  
  # use 'discount' to calculate net present value 
  # 'discount_rate' is expressed in percent
  NPV_no_net <- discount(profit_no_net, discount_rate = 5, calculate_NPV = TRUE)
  NPV_net <- discount(profit_with_net, discount_rate = 5, calculate_NPV = TRUE)
  
  # calculate the overall NPV of the decision (do - don't do)
  NPV_decision <- NPV_net - NPV_no_net
  
  return(list(NPV_no_net =  NPV_no_net,
              NPV_net =  NPV_net, 
              NPV_decision = NPV_decision))
}
  # Run the Monte Carlo simulation using the model function
  hail_mc_simulation <- mcSimulation(estimate = as.estimate(hail_estimates),
                                     model_function = hail_function,
                                     numberOfModelRuns = 200,
                                     functionSyntax = "plainNames")
  
  hail_mc_simulation
  
  plot_distributions(mcSimulation_object = hail_mc_simulation, 
                     vars = c("NPV_no_net", "NPV_net"),
                     method = 'smooth_simple_overlay', 
                     base_size = 7)  

  # subset the outputs from the mcSimulation function (y) 
  # to run the multi_EVPI only on the variables that the we want 
  # (i.e. the NPV_decision)
  mcSimulation_table_hail <- data.frame(hail_mc_simulation$x, 
                                        hail_mc_simulation$y[3])
  
  evpi_hail <- multi_EVPI(mc = mcSimulation_table_hail, 
                          first_out_var = "NPV_decision")  
  plot_evpi(evpi_hail, decision_vars = "NPV_decision")  

  install.packages("igraph")
  library(igraph)
  hail_path <- graph.formula(HailNet -+ Yield,
                             HailNet -+ Cost,
                             HailEvent -+ Yield,
                             Yield -+ MarketPrice,
                             MarketPrice -+ NPV,
                             Cost -+ NPV,
                             Discount -+ NPV)
  
  plot(hail_path)  
  