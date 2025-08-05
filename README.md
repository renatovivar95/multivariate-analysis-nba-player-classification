
# NBA Player Profiles Classification Project

This repository contains the full content and code for the Multivariate Analysis project titled **"A Classification Study of NBA Player Profiles"**, developed as part of our coursework at **Instituto Superior Técnico**.

The goal of this project is to explore how classification methods can be used to:

- **Predict whether an NBA player will receive a major award** based on career statistics.
- **Identify a player's nominal position** (e.g., PG, SF, etc.) using their performance metrics.

We apply dimensionality reduction (PCA), data transformations, and several classification methods to analyze and interpret real-world NBA data.

---

## Repository Structure

├── data_prepare.r                         # Script to load, clean, and transform the dataset

├── Classification methods - final.Rmd     # Application of classification models and metric analysis

├── Classmaps and Results - Final.Rmd      # Class maps and stacked mosaic visualizations for interpretation

├── DATA/

│   ├── Seasons_Stats.csv                 # Raw NBA season stats from Kaggle

│   ├── awards.csv                        # Awards data from the NBA API

│   └── notes.txt                         # Notes on the data collection process

├── nba_raw.csv                           # Raw preprocessed dataset

├── nba.csv                               # Cleaned and aggregated dataset for classification

├── nba_transformed.csv                   # Dataset after log-based transformations for normalization

├── nba_transformed_pca.csv               # Dataset after PCA dimensionality reduction

├── Presentation.pdf                      # Slide deck of the project

└── README.md                             # You're here


---

## Summary of the Project

- **Initial Dataset**: NBA player statistics per season (1950–2017) from Kaggle, enhanced with player awards, height, and weight from the NBA API.
- **Final Dataset**: Aggregated player profiles (career scope) with 19 performance and biometric variables.
- **Target Variables**:
  - `AWD`: Binary label indicating whether the player received a major career award.
  - `Pos`: Nominal player position (C, PF, PG, SF, SG).

---

## Methods Applied

- **Preprocessing**:
  - Aggregation by player (career scope).
  - Normalization and log-based transformations.
  - Feature selection and engineering (e.g., per-minute stats, MPG, MPS).
- **Dimensionality Reduction**:
  - PCA on standardized and transformed data.
- **Classification Techniques**:
  - LDA (Linear Discriminant Analysis)
  - QDA (Quadratic Discriminant Analysis)
  - k-NN (K-Nearest Neighbors)
  - Naive Bayes
- **Model Evaluation**:
  - Stratified 10-fold cross-validation
  - Accuracy, F1-Score, Recall, Confusion Matrices
  - PAC & Fartness metrics with `classmap` R package

---

## Key Insights

- LDA outperformed other classifiers, especially in the imbalanced **Award Prediction** task.
- QDA performed competitively, particularly after dimensionality reduction.
- k-NN struggled with high-dimensionality; Naive Bayes underperformed due to violated independence assumptions.
- Feature transformations improved compatibility with Gaussian-based methods.
- Classmaps revealed positional ambiguities (e.g., PF vs SF) and outlier candidates.

---

## Requirements

All the libraries required can be read at the start of the scripts or markdown files.

To reproduce the results:
```R
source("data_prepare.r")       # Load and transform the data
# Open and run the .Rmd notebooks in RStudio
```

---

## References

- NBA data: https://www.kaggle.com/datasets/drgilermo/nba-players-stats
- Awards API: https://doi.org/10.32614/CRAN.package.hoopR
- Classmap: https://cran.r-project.org/web/packages/classmap/

---
