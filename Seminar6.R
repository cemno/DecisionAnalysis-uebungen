library(decisionSupport)
library(tidyverse)
library(DiagrammeR)

# Adding Management costs, adjusting linkStyle to 4 (for new link)
mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        ML(Management cost)-->F; linkStyle 4 stroke: red, stroke-width:1.5px")

# Changing Plot direction by replacing LR (Left to Right) to TD (top to down)
mermaid("graph TD
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1px
        CM(Management cost)-->F; linkStyle 4 stroke: red, stroke-width:1px")


# Input table for model function, adding Management_cost
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost", "Management_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season", 
                                              "Management costs in a normal season"))

## Changing some columns as they are not in the right format (addition, maybe because R-base 3.6.3?)
input_estimates$distribution <- as.character(input_estimates$distribution)
input_estimates$variable <- as.character(input_estimates$variable)

# Show input_estimates table
input_estimates

# Creating model function to describe the graphical impact pathway (Diagram done earlier)
model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  # Estimate the overall cost in a normal season
  overall_cost <- Labor_cost + Management_cost
  
  # Estimate the final results from the model
  final_result <- income - overall_cost
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}
# Run the Monte Carlo simulation using the model function
chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                    model_function = model_function,
                                    numberOfModelRuns = 10000,
                                    functionSyntax = "plainNames")

# Print MonteCarlo simulation output
chile_mc_simulation

# Plot distributions to show outcome
plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "hist_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

# Function for creating a random variable for a column from the input_estimates table 
# and put it in the global Env. for test purposes (faster that running whole model)
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

# Creating random global variable from input_estimates
make_variables(as.estimate(input_estimates))

# Call function (without parameters as they are in the global environment)
model_function()
