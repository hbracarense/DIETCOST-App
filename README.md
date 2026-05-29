# DIETCOST — Monte Carlo Simulation of Healthy Diet Costs and Environmental Impact

**Author:** Henrique Bracarense  
**Type:** R Shiny interactive simulation app  
**Languages:** R (Shiny, ShinyDashboard)  
**Scope:** Estimation of the economic and environmental cost of diet scenarios through Monte Carlo simulation

---

## Overview

**DIETCOST** is an interactive application built with **R Shiny** to estimate the **cost, nutritional profile, and environmental footprint** of diet combinations generated under user-defined food and nutrient constraints.

The application supports two data-entry workflows:

1. assembling a food dataset from the internal database supplied with the app; or  
2. uploading user-defined Excel workbooks that follow the reference data models.

After the food database and constraints are configured, DIETCOST runs a **Monte Carlo simulation** to search for feasible diet combinations. The simulation records unique diets that satisfy the configured restrictions and exports detailed results for analysis.

The tool is designed for researchers, students, and policymakers interested in the economic accessibility and environmental implications of healthy and sustainable diets.

---

## Data Sources

- **`data.xlsx`** — Main application database, containing food, nutrient, price, environmental-impact, and constraint parameters:
  - `food_data` — food group, food name, food ID, environmental indicators, nutrient composition, and price;
  - `food_constraints` — minimum and maximum food-level intake values by demographic group;
  - `food_group_constraints` — food-group constraints in grams and serves by demographic group;
  - `nutrient_targets` — nutrient and dietary-composition targets by demographic group and diet.

- **`www/food_data_model.xlsx`** — reference model for uploading a custom food-composition and price database.

- **`www/constraints_data_model.xlsx`** — reference model for uploading custom food, food-group, nutrient, and linked-food constraints.

- **`www/dietcost_manual.pdf`** — methodological and operational manual available through the app interface.

---

## Functionalities

### **1. Introduction and Manual**

The **Introduction** tab presents the purpose of DIETCOST, the general methodological framing, institutional context, and contact information.

It also provides direct access to the PDF manual through the **Download manual** button.

### **2. Food Data Configuration**

The **Foods** tab allows the user to define the food database used in the simulation.

Two modes are available:

- **Assemble food data from the internal database**
  - Users select foods from the app's built-in food database.
  - The selected items define the universe of foods eligible for the simulation.

- **Load your own data**
  - Users upload an `.xlsx` workbook.
  - The workbook must follow the structure of `www/food_data_model.xlsx`.
  - Mandatory fields include food group, food name, and food ID.
  - Nutrient, price, and environmental-impact columns must be numeric when included.

The app validates column names and expected variable structures before allowing the user to proceed.

### **3. Constraint Configuration**

The **Constraints** tab defines the restrictions applied to the Monte Carlo simulation.

The app supports:

- food-level constraints;
- food-group constraints;
- nutrient constraints;
- linked-food constraints.

Constraints can be configured manually or uploaded through an `.xlsx` workbook following the structure of `www/constraints_data_model.xlsx`.

The uploaded constraints workbook may include:

- `food_constraints`;
- `food_group_constraints`;
- `nutrient_targets`;
- `linked_foods_pair_1`;
- `linked_foods_pair_2`.

Before proceeding, the app checks consistency between food IDs, food groups, nutrient variables, and linked-food definitions.

### **4. Monte Carlo Simulation Engine**

The **Simulation** tab runs the Monte Carlo procedure after food data and constraints have been configured.

The user defines:

- the output directory;
- the number of Monte Carlo iterations;
- the minimum serve-size difference used when updating candidate diets;
- the variables used in the simulation, depending on the food and nutrient columns available.

During the run, the app generates candidate diet combinations, evaluates the configured constraints, and logs feasible unique diets.

A diet is recorded when it satisfies the active restrictions, including nutrient targets, food-group limits, and linked-food rules when applicable.

### **5. Nutritional, Economic, and Environmental Measures**

DIETCOST can evaluate diet combinations using the following measures when the corresponding columns are present in the food database:

| Variable | Unit / Interpretation | Description |
|----------|------------------------|-------------|
| `price` | monetary value | Food or diet cost |
| `CF_gCO2eq` | g CO₂-eq | Carbon footprint |
| `WF_l` | liters | Water footprint |
| `EF_g_m2` | g m² | Ecological footprint |
| `energy_kj_g` | kJ/g | Energy |
| `fat_g` | g | Total fat |
| `sat_fat_g` | g | Saturated fat |
| `CHO_g` | g | Carbohydrates |
| `sugars_g` | g | Sugars |
| `fibre_g` | g | Fibre |
| `protein_g` | g | Protein |
| `sodium_mg` | mg | Sodium |

The app also computes percentage-based dietary measures such as fat, saturated fat, carbohydrate, sugar, protein, alcohol, discretionary foods, takeaway foods, and red meat when applicable.

### **6. Output Generation**

For each run, the app creates a timestamped results folder named:

```text
results_YYYYMMDDHHMMSS
```

Inside this folder, each feasible unique diet is saved as a CSV file:

```text
meal_plan_<iteration>.csv
```

After the simulation finishes, the user can download an Excel report through the **Download report** button.

The report is saved as:

```text
results_monte_carlo_YYYYMMDDHHMM.xlsx
```

The workbook may contain the following sheets:

- `General` — output folder path, number of iterations, and number of meals created;
- `Last meal` — final candidate meal state;
- `Nutrient constraints` — nutrient-constraint diagnostics;
- `Group constraints` — food-group constraint diagnostics;
- `Linked foods` — linked-food diagnostics;
- `Nutrients diff` — nutrient differences from target ranges;
- `Groups diff` — food-group differences from target ranges;
- `Nutrient targets` — active nutrient targets;
- `Group targets` — active food-group targets;
- `Results` — mean values and confidence-interval margins across generated meal plans, when at least one feasible diet is created.

---

## Technical Components

| Functionality | R Packages |
|----------------|------------|
| UI framework | `shiny`, `shinydashboard`, `shinythemes`, `shinyWidgets` |
| Interface helpers | `shinyjs`, `shinycssloaders`, `shinyFiles` |
| Data handling | `readxl`, `dplyr`, `rlang`, `vroom`, `tools` |
| Tables and export | `DT`, `writexl` |

---

## Directory Structure

```text
dietcost/
├─ app.R                         # Main Shiny application
├─ launcher.R                    # Launcher used by the packaged/offline version
├─ DIETCOST.bat                  # Windows batch launcher
├─ DIETCOST.exe                  # Windows executable launcher
├─ dietcost_installer.iss        # Inno Setup installer script
├─ data.xlsx                     # Main food, nutrient, price, impact, and constraint database
├─ R/                            # Embedded R runtime and package library for offline distribution
├─ Output/
│  └─ DIETCOST_Setup.exe         # Generated installer, when built
└─ www/
   ├─ constraints_data_model.xlsx
   ├─ food_data_model.xlsx
   └─ dietcost_manual.pdf
```

---

## How to Run Locally from R

1. Install **R** and the required packages:

```r
install.packages(c(
  "shiny", "shinythemes", "shinyWidgets", "shinydashboard",
  "shinycssloaders", "shinyjs", "shinyFiles", "readxl",
  "DT", "writexl", "tools", "vroom", "dplyr", "rlang"
))
```

2. Open the project folder in RStudio or set the working directory to the app folder.

3. Run:

```r
shiny::runApp("app.R")
```

---

## How to Run the Packaged Windows Version

The current project structure includes a portable/offline Windows distribution with an embedded R runtime.

To launch the app without a separate R installation, use:

```text
DIETCOST.exe
```

or, for diagnostic purposes:

```text
DIETCOST.bat
```

The launcher sets the local app directory, points `.libPaths()` to the embedded R package library under `R/library`, and starts the Shiny app on `127.0.0.1` in the default browser.

---

## Installer Build

The project includes an Inno Setup script:

```text
dietcost_installer.iss
```

The script installs DIETCOST under:

```text
{localappdata}\DIETCOST
```

and can create Start Menu and Desktop shortcuts pointing to `DIETCOST.exe`.

When compiled, the installer output is expected as:

```text
Output/DIETCOST_Setup.exe
```

---

## Outputs and Interpretation

- **Generated meal plans:** each feasible unique diet is exported as an individual CSV file.
- **Economic cost:** total diet cost is computed when the `price` column is present.
- **Environmental impact:** carbon, water, and ecological footprints are computed when the corresponding columns are present.
- **Nutrient adequacy:** generated diets are evaluated against the active nutrient constraints.
- **Food-group adequacy:** generated diets are evaluated against gram and serve restrictions by food group.
- **Confidence intervals:** after feasible diets are created, the user selects a confidence interval between 90% and 99%; the Excel report then reports mean values and margins across generated meal plans.

---

## Notes on Custom Data

Custom food and constraint files must follow the reference models distributed with the app.

The app performs validation checks for:

- required columns;
- compatible food IDs;
- compatible food groups;
- numeric variables for nutrients, prices, and environmental indicators;
- consistency between selected food data and uploaded constraints;
- linked-food definitions.

If a validation check fails, the app blocks progression and displays a warning so the input workbook can be corrected.

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
