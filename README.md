# PHCUOR-DiD-Analysis
This is an exploratory analysis conducted as part of methods learning in causal inference using MICS data.

# Method: Evaluating the Impact on Facility Birth of PHCUOR Policy in Nigeria: A Difference-in-Differences Analysis (MICS 2007–2016)

This repository contains the complete analysis and findings from a quasi-experimental Difference-in-Differences (DiD) study that assessed the impact of Nigeria's Primary Health Care Under One Roof (PHCUOR) policy on facility birth rates with data from MICS 2007, 2011, and 2016.

---

## Project Overview

The PHCUOR policy, formally launched in 2011, aimed to strengthen primary healthcare delivery in Nigeria. This study leveraged repeated cross-sectional data from the Nigeria Multiple Indicator Cluster Surveys (MICS) to evaluate the policy's effectiveness. By comparing observed changes in health indicators over time between states that adopted the policy early (treatment group) and those that adopted it late (control group), the study successfully estimated the policy's causal impact.

---

## Objectives Achieved

This project addressed the following core objectives:
**Assessed** whether the implementation of the PHCUOR policy resulted in a **increase in facility births**.

---

## Methodology

* **Design:** A Difference-in-Differences (DiD) quasi-experimental design was employed.
* **Data:** Nigeria MICS data (2007, 2011, 2016-17) was utilized.
* **Study Groups:**
    * **Treatment States:** Abia, Adamawa, Bauchi, Benue, Delta, Ebonyi, Gombe, Imo, Jigawa, Kaduna, Kano, Katsina, Kebbi, Nasarawa, Niger, Ogun, Ondo, Oyo, Plateau, Rivers, Sokoto, Taraba, Yobe (early adopters of PHCUOR).
    * **Control States:** Akwa-Ibom, Anambra, Bayelsa, Borno, Cross-Rivers, Edo, Ekiti, Enugu, Kogi, Kwara, Lagos, Osun, Zamfara, Abuja FCT (late adopters of PHCUOR).
* **Key Analytical Steps:**
    * Exploratory Data Analysis (EDA) was performed.
    * Survey-weighted generalized linear models (specifically quasibinomial for binary outcomes) were fitted for DiD estimation, accounting for complex survey designs.
    * Checks for the parallel trends assumption were conducted.
    * Data visualization was generated to illustrate trends.
* **Tools:** All analytical procedures were executed entirely in **R**, emphasizing **reproducibility** and **clean, pipelined code**.

---

## Repository Structure

* `DiD_Policy_analysis_PHCUOR.Rmd`: The main R Markdown report containing the detailed analysis, including all R code chunks (with clear comments), descriptions of methods, and integrated results and visualizations.
* `data/`: (Placeholder) This directory would contain the raw and processed MICS datasets used in the analysis. 
* `outputs/`: (Placeholder) This directory holds all figures, tables, and other generated output from the analysis.

---
```bash

PHCUOR_policy_impact_DiD_folder/
│
├── README.md             
├── DiD_Policy_analysis_PHCUOR.Rmd          
├── data/                
│   └── mics_stacked.csv
├── scripts/              
├── output/              

```
## Replicating the Analysis

This project is designed for full reproducibility. To replicate the analysis and generate the report:

1.  **Clone the Repository:**
    ```bash
    git clone [https://github.com/dotnas/PHCUOR-DiD-Analysis.git]
    
    cd dotnas
    ```
2.  **Obtain Data:** Download the relevant Nigeria MICS datasets (2007, 2011, 2016-17) from the UNICEF MICS programme website (https://mics.unicef.org/surveys?display=card) or other approved sources and place them in the `data/` directory.
3.  **Install R Packages:** Open the `DiD_Policy_analysis_PHCUOR.Rmd` file in RStudio. Ensure you have all necessary R packages installed. You can install them using the following command in your R console:
    ```R
    install.packages(c("tidyverse", "haven", "survey", "fixest", "ggplot2", "sf", "tmap", "cobalt", "margins", "modelsummary"))
    ```
4.  **Knit the R Markdown:** Once the data is in place and packages are installed, knit the `DiD_Policy_analysis_PHCUOR.Rmd` file in RStudio to generate the complete HTML report.

---

## Project Structure

- `01_data_cleaning.Rmd` – Data wrangling, variable transformation (binary and factor), merging datasets.
- `02_multiple_imputation.Rmd` – Handling missing data using multiple imputation via `mice`.
- `03_analysis_did.Rmd` – Difference-in-Differences (DiD) and parallel trends testing using `fixest`.
- `output/` – Cleaned data, imputed data, and results summaries.

## Main Research Focus

- **Treatment Variable**: `treated` (early\_adopters vs late\_adopters)
- **Time Variable**: `year` (2007, 2011, 2016)
- **Outcome**: `facility_birth_bin`
- **Covariates**: maternal education, rural/urban, maternal age, wealth index

## Key R Packages Used

- `tidyverse` – Data wrangling and visualization
- `mice` – Multiple imputation
- `fixest` – Econometric modeling including DiD
- `survey` – (Optional) Complex survey design incorporation

## Notes

- MICS data includes clustering and weighting—weights (`survey_weight`) can be used in analysis.
- Descriptive statistics and visualizations precede modeling.
- Parallel trends test uses 2007–2011; DiD model uses 2011–2016 comparison.

## File Structure

- Analyst: Nasir Umar
- Date: 2025

## Contact

Maintained by: **Nasir Umar**  
Email: *nasir.umar@raas-institute.org*
