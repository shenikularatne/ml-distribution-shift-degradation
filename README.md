# ml-distribution-shift-degradation
Statistical analysis of ML model performance degradation under distribution shift, using a Random Forest classifier on the UCI Adult Income dataset.
Model Performance Degradation Under Distribution Shift

Theory and Practices in Statistical Modelling — SLIIT BSc (Hons) IT, Data Science

Overview

This project statistically validates a critical real-world machine learning challenge:
model performance degrades when deployed in environments different from the conditions
it was trained on — a phenomenon known as distribution shift.

Hypothesis tested: Model performance degrades when deployed in environments
different from training conditions.

Dataset

UCI Adult Income Dataset (1994 US Census)
— 30,162 records after cleaning.

Methodology

A Random Forest classifier was trained on US workers aged 25–45, then evaluated
across two controlled deployment environments simulating distribution shift:


Region shift — deployment population drawn from a different geographic distribution
Age shift — deployment population outside the training age range


Three-stage analysis


Descriptive Analytics — compared distributions of key features (e.g. capital
gain: $997 baseline vs $486 region shift vs $1,897 age shift) and income class
proportions (shifted up to 10.2 percentage points)
Inferential Analytics — hypothesis testing across 10 tests; 9 significant at
p < 0.001, confirming the distributional differences were not due to chance
Predictive Analytics — Multiple Linear Regression models explaining 52.7% and
60.6% of degradation variance (both p < 0.001)


Key Findings


Age shift caused consistent degradation across all evaluation metrics
Region shift revealed hidden degradation — accuracy stayed misleadingly stable
while F1-score and AUC exposed the true performance drop
Multi-metric evaluation is essential — accuracy alone is insufficient to detect
distribution shift effects
Hours-per-week shift was the strongest predictor of degradation


Conclusion

A model that performs well in development may fail silently in production. Rigorous
statistical validation of deployment conditions is not optional — it is essential.

Tools & Technologies

R
Key packages: (fill in — e.g. randomForest, caret, dplyr, ggplot2)
