# Problem Set 2 — Big Data & Machine Learning (Uniandes)

Solution for **Problem Set 2** from the course **Big Data and Machine Learning for Applied Economics** (Universidad de los Andes, 2022-2).

## 🎯 Objective

Build and compare **classification models** to predict whether an individual is **under the poverty line** (binary outcome) using socioeconomic and survey-based features.

---

## 📁 Repository Structure

```
├── Pre-Processing.R        # Data cleaning and feature engineering
├── descriptives.R          # Exploratory data analysis and descriptive statistics
├── Modelos.R               # Main modeling script (classification models)
├── scripts/                # (If applicable) additional helper scripts
├── views/                  # Exported tables and figures
├── Problem_Set_2.pdf       # Final report (if included)
├── Merge_n_CalculoPobreza.html  # Rendered HTML output (if included)
├── LICENSE
└── README.md
```

---

## 🧩 Project Components

### 🔹 Pre-processing
- Data cleaning  
- Handling missing values  
- Feature engineering  
- Dummy variable creation  
- Preparation of modeling dataset  

### 🔹 Descriptive Analysis
- Summary statistics  
- Poverty distribution analysis  
- Tables and visualization outputs  

### 🔹 Modeling (`Modelos.R`)
Multiple classification algorithms were trained and compared:

- **Regularized Logistic Regression** (`glmnet`)
- **Decision Tree** (`rpart`)
- **Random Forest** (`rf`)
- **Gradient Boosted Trees** (`xgbTree`)

Models were trained and evaluated using the **caret** / **tidymodels** ecosystem, with performance comparison across approaches.

---

## 📦 Packages Used

Packages are loaded via `pacman::p_load(...)` across the scripts. Consolidated list:

- caret
- classInt
- cowplot
- doParallel
- fastDummies
- glmnet
- grid
- gridExtra
- kableExtra
- labelled
- lessR
- MLmetrics
- mlr
- parallel
- randomForest
- rattle
- RColorBrewer
- rlang
- rpart
- rpart.plot
- sf
- skimr
- sp
- stargazer
- themis
- tidymodels
- tidyverse
- xgboost (via caret method `xgbTree`)

---

## 📌 Notes

- Large datasets (e.g., `.rds` files) may not be included due to GitHub file size limitations.
- See the report files for detailed methodology, variable definitions, and evaluation metrics.

---

## 📜 License

MIT License
