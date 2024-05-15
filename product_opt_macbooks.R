#### Loading main libraries #### 
library(ROI)
library(ROI.plugin.glpk)

# Tidy Optimization
library(ompr)
library(ompr.roi)

# Visualization
library(plotly)

# Core
library(tidyverse)
library(readxl)

# Timing
library(tictoc)


#### Data ####
path <- "excel_optimization/Excel Optimization - 2.0 Macbook Product Mix.xlsx"

labor_cost_tbl <- read_excel(path = path, sheet = 2, range = "A3:B5", col_names = c("type", "cost")) 
labor_cost_tbl

process_inputs_tbl <- read_excel(path = path, sheet = 2, range = "A8:I13") 
process_inputs_tbl

labor_constraints_tbl <- read_excel(path = path, sheet = 2, range = "A25:D28") %>%
    select(1, 4)
labor_constraints_tbl

unit_sales_constraints_tbl <- read_excel(path = path, sheet = 2, range = "A23:I23", col_names = c("key", str_c("Model ", 1:8))) %>%
    gather()
unit_sales_constraints_tbl


#### Preparing Data ####
process_inputs_tidy_tbl <- process_inputs_tbl %>%
    
    # Tidy Data
    rename(type = `...1`) %>%
    gather(key = "key", value = "value", -type) %>%
    spread(key = type, value = value) %>%
    rename(model = key) %>%
    
    # Add Labor Cost
    bind_cols(
        labor_cost_tbl %>% 
            spread(key = type, value = cost) %>%
            mutate(count = 8) %>% 
            uncount(count)
    ) %>%
    
    # Clean Up Column Names
    rename_all(.funs = ~ str_replace_all(., " ", "_") %>%
                   str_to_lower() %>%
                   str_replace_all(",", "")) %>%
    
    # Calculate Unit Margin
    mutate(
        unit_margin_line_1 = selling_price - (cost_per_labor_hour_testing_line_1 * labor_hours_for_testing_line_1) - (cost_per_labor_hour_assembling * labor_hours_for_assembly) - cost_of_component_parts,
        unit_margin_line_2 = selling_price - (cost_per_labor_hour_testing_line_2 * labor_hours_for_testing_line_2) - (cost_per_labor_hour_assembling * labor_hours_for_assembly) - cost_of_component_parts
    )

process_inputs_tidy_tbl



#### Optimization Model #### 
n_models  <- 8
max_sales <- unit_sales_constraints_tbl$value
max_labor <- labor_constraints_tbl$`Hours available`

labor_hours_for_assembly       <- process_inputs_tidy_tbl$labor_hours_for_assembly
labor_hours_for_testing_line_1 <- process_inputs_tidy_tbl$labor_hours_for_testing_line_1
labor_hours_for_testing_line_2 <- process_inputs_tidy_tbl$labor_hours_for_testing_line_2

unit_margin_line_1 <- process_inputs_tidy_tbl$unit_margin_line_1
unit_margin_line_2 <- process_inputs_tidy_tbl$unit_margin_line_2

tic()
model_2 <- MIPModel() %>%
    
    add_variable(macbooks_tested_line_1[i], i = 1:n_models, type = "integer", lb = 0) %>%
    add_variable(macbooks_tested_line_2[i], i = 1:n_models, type = "integer", lb = 0) %>%
    
    add_constraint(macbooks_tested_line_1[i] + macbooks_tested_line_2[i] <= max_sales[i], i = 1:n_models) %>%
    
    add_constraint(sum_expr((macbooks_tested_line_1[i] + macbooks_tested_line_2[i]) * labor_hours_for_assembly[i], i = 1:n_models) <= max_labor[1]) %>%
    add_constraint(sum_expr((macbooks_tested_line_1[i]) * labor_hours_for_testing_line_1[i], i = 1:n_models) <= max_labor[2]) %>%
    add_constraint(sum_expr((macbooks_tested_line_2[i]) * labor_hours_for_testing_line_2[i], i = 1:n_models) <= max_labor[3]) %>%
    
    set_objective(
        sum_expr(macbooks_tested_line_1[i] * unit_margin_line_1[i] + macbooks_tested_line_2[i] * unit_margin_line_2[i], i = 1:n_models),
        sense = "max") %>%
    
    solve_model(with_ROI(solver = "glpk"))
toc()


#### Model Summary #### 
model_2

model_2$objective_value

get_solution(model_2, macbooks_tested_line_1[i]) 
get_solution(model_2, macbooks_tested_line_2[i]) 


#### Model Simulation #### 
optimize_by_max_labor <- function(max_labor_assembly = 20000, max_labor_testing_line_1 = 5000, max_labor_testing_line_2 = 6000) {
    
    model_2 <- MIPModel() %>%
        
        add_variable(macbooks_tested_line_1[i], i = 1:n_models, type = "integer", lb = 0) %>%
        add_variable(macbooks_tested_line_2[i], i = 1:n_models, type = "integer", lb = 0) %>%
        
        add_constraint(macbooks_tested_line_1[i] + macbooks_tested_line_2[i] <= max_sales[i], i = 1:n_models) %>%
        
        add_constraint(sum_expr((macbooks_tested_line_1[i] + macbooks_tested_line_2[i]) * labor_hours_for_assembly[i], i = 1:n_models) <= max_labor_assembly) %>%
        add_constraint(sum_expr((macbooks_tested_line_1[i]) * labor_hours_for_testing_line_1[i], i = 1:n_models) <= max_labor_testing_line_1) %>%
        add_constraint(sum_expr((macbooks_tested_line_2[i]) * labor_hours_for_testing_line_2[i], i = 1:n_models) <= max_labor_testing_line_2) %>%
        
        set_objective(
            sum_expr(macbooks_tested_line_1[i] * unit_margin_line_1[i] + macbooks_tested_line_2[i] * unit_margin_line_2[i], i = 1:n_models),
            sense = "max") %>%
        
        solve_model(with_ROI(solver = "glpk"))
    
    return(
        bind_rows(
            get_solution(model_2, macbooks_tested_line_1[i]), 
            get_solution(model_2, macbooks_tested_line_2[i]) 
        ) %>%
            add_column(net_profit = model_2$objective_value)
    )
}


optimize_by_max_labor(max_labor_assembly = 20000, max_labor_testing_line_1 = 5000, max_labor_testing_line_2 = 6000)

#### Iteration #### 
tic()
optimization_by_max_labor_results <- list(
    assembly_total = seq(18000, 24000, length.out = 7),
    testing_line_1_total = 5000,
    testing_line_2_total = 6000
) %>%
    cross_df() %>%
    mutate(optimization_solution = pmap(.l = list(assembly_total, testing_line_1_total, testing_line_2_total),
                                        .f = optimize_by_max_labor))
toc()


#### Visualization ####
g <- optimization_by_max_labor_results %>%
    unnest() %>%
    ggplot(aes(x = as.factor(i), y = value, group = variable)) +
    facet_wrap(~ variable, ncol = 1) +
    geom_jitter(aes(color = assembly_total), width = 0.1) +
    labs(title = "Assembly Hours Effect", x = "Macbook Pro Model", y = "Quantity Manufactured")

ggplotly(g)


#### Apendix ROI Version ####
names <- c(str_c("line_1_model_", 1:8), str_c("line_2_model_", 1:8))

model_2_roi <- OP(
    types = rep("I", 2*n_models),
    constraints = rbind(
        L_constraint(diag(2*n_models), rep(">=", 2*n_models), rep(0, 2*n_models)),
        
        L_constraint(cbind(diag(n_models), diag(n_models)), rep("<=", n_models), rhs = max_sales),
        
        L_constraint(rep(1, 2*n_models) * labor_hours_for_assembly, "<=", rhs = max_labor[1]),
        L_constraint(c(rep(1, n_models), rep(0, n_models)) * labor_hours_for_testing_line_1, "<=", max_labor[2]),
        L_constraint(c(rep(0, n_models), rep(1, n_models)) * labor_hours_for_testing_line_2, "<=", max_labor[3])
    ),
    objective = L_objective(
        c(rep(1, n_models), rep(0, n_models)) * unit_margin_line_1 + 
            c(rep(0, n_models), rep(1, n_models)) * unit_margin_line_2, names = names
    ),
    maximum = TRUE
)

sol <- ROI_solve(model_2_roi, "glpk")

sol
sol$solution

