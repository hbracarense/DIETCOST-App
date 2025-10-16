# DIETCOST — Monte Carlo Simulation of Healthy Diet Costs and Environmental Impact

**Author:** Henrique Bracarense  
**Type:** R Shiny interactive simulation app  
**Languages:** R (Shiny, ShinyDashboard)  
**Scope:** Estimation of the economic and environmental cost of healthy diets through Monte Carlo simulation

---

## Overview

**DIETCOST** is an interactive application built with **R Shiny** to calculate the **cost and environmental footprint** of a nutritionally adequate diet for different demographic groups (men, women, boys, girls).  

It simulates food combinations that meet nutritional targets using **Monte Carlo optimization**, based on food composition, price, and environmental-impact databases.  

The tool is designed for **policy analysis and dietary planning**, allowing exploration of the trade-offs between **nutritional adequacy**, **affordability**, and **sustainability**.

---

## Data Sources

- **`data.xlsx`** — Main dataset containing all food and nutrient parameters:
  - `food_data` — food composition and price per gram  
  - `food_constraints` — min/max portions per person  
  - `food_group_constraints` — aggregated constraints by food group  
  - `nutrient_targets` — recommended intakes and macronutrient limits  

- **`www/constraints_data_model.xlsx`** — structural reference for constraint definitions  
- **`www/food_data_model.xlsx`** — schema for food-composition variables  
- **`www/dietcost_manual.pdf`** — methodological documentation

---

## Functionalities

### **1. Data Loading and Initialization**
- Loads `data.xlsx` sheets into global data frames.  
- Computes **boundary values** for each nutrient and food variable (min/max grams, serves, energy, macronutrients).  
- Predefines **linked foods** and substitution sets (`linked_low_*`, `linked_high_*`) for correlated items (e.g. meat/fish swaps).  
- Identifies **red-meat IDs** and model variables (`model_foods`, `nutrient_colnames`).

### **2. Monte Carlo Simulation Engine**
- Randomly samples food combinations that satisfy nutritional constraints:
  - Energy, fat, saturated fat, carbohydrates, sugar, fiber, protein, sodium.  
- For each simulated diet, calculates:
  - **Total cost** (BRL per day).  
  - **Carbon footprint (CF)** in g CO₂-eq.  
  - **Water footprint (WF)** in liters.  
  - **Ecological footprint (EF)** in g m².  
- Repeats simulation thousands of times to approximate feasible diet distributions.

### **3. User Interface**
Implemented via `shinydashboard` and `shinyWidgets`.

- **Input controls**
  - Select demographic group (man, woman, boy, girl).  
  - Set simulation parameters: number of iterations, seed, constraint tolerance.  
  - Toggle environmental metrics and cost weighting.  

- **Output panels**
  - **Summary dashboard** — value boxes for mean diet cost, CO₂, WF, EF.  
  - **Food-group charts** — barplots of energy and cost contribution by group.  
  - **Monte Carlo distribution plots** — histograms for simulated diet costs.  
  - **Nutrient balance panel** — radar or parallel plots comparing simulated vs target nutrients.  

- **Tables and downloads**
  - `DT::datatable` for simulated results.  
  - `writexl` export of current run in `.xlsx` format.

### **4. Environmental Indicators**
Each food item carries three impact coefficients:
| Variable | Unit | Description |
|-----------|-------|-------------|
| `CF_gCO2eq` | g CO₂-eq / g | Carbon footprint |
| `WF_l` | L / g | Water footprint |
| `EF_g_m2` | g m² / g | Ecological footprint |

Totals are computed per simulated diet and compared to baseline scenarios.

### **5. Simulation Logic**
Simplified pseudocode:
```r
for (i in 1:N_sim) {
  diet <- sample_feasible_foods(constraints)
  results[i] <- summarize_diet(diet)
}
summary_stats <- aggregate(results)
```
Monte Carlo sampling ensures stochastic exploration of feasible diet space rather than deterministic optimization.

### **6. Output Files**
- `dietcost_results.xlsx` — simulated results for chosen demographic and parameters.  
- `summary_table.csv` — aggregated statistics (mean ± SD).  
- `constraints_log.txt` — records of constraint enforcement per run.

---

## Technical Components

| Functionality | R Packages |
|----------------|------------|
| UI framework | `shiny`, `shinydashboard`, `shinythemes`, `shinyWidgets` |
| Data handling | `readxl`, `dplyr`, `rlang`, `vroom` |
| Tables / Export | `DT`, `writexl`, `tools` |
| Interactivity | `shinyjs`, `shinycssloaders`, `shinyFiles` |

---

## Directory Structure

```
dietcost/
├─ app.R                     # Main Shiny app
├─ data.xlsx                 # Nutrient, price and constraint data
└─ www/
   ├─ constraints_data_model.xlsx
   ├─ food_data_model.xlsx
   └─ dietcost_manual.pdf
```

---

## How to Run Locally

1. Install **R ≥ 4.1** and required packages:
```r
install.packages(c(
  "shiny","shinythemes","shinyWidgets","shinydashboard",
  "shinycssloaders","shinyjs","shinyFiles","readxl",
  "DT","writexl","vroom","dplyr","rlang"
))
```

2. Open `app.R` in RStudio.  
3. Run the app:
```r
shiny::runApp("app.R")
```

---

## Outputs and Interpretation

- **Economic costs:** mean daily cost and distribution percentiles.  
- **Environmental impact:** aggregate CO₂-eq, water, and land use.  
- **Feasibility:** share of simulations meeting all nutrient targets.  
- **Trade-off analysis:** users can adjust cost/environment weighting to see substitution effects.

---

## Citation

> **Bracarense, Henrique** (2025).  
> *DIETCOST — Monte Carlo Simulation of Healthy Diet Costs and Environmental Impact.*  
> R Shiny application for dietary cost and sustainability analysis.  
> URL: https://github.com/ or project link (available upon release).

---

## License

Distributed under an **MIT-style license** for research and non-commercial use.  
Please cite this project when reproducing its simulations or methodology.
