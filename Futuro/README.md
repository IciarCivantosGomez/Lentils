# README #

This repository contains the software, data and reproducibility instructions of a set of machine learning models to predict rust-resistance for candidate populations.

Please, clone the repo and follow these steps.

## Data

There are four clean datasets called:
* 'DatosPresente/fichtraining_ROYA_CAMPO_nondup.csv' Dataset with the bioclimatic variables, DSR and coordinates for the lentils evaluated for their resistance to rust in the present.
* 'DatosPrecip/ROYA_Completa_fut_pres.txt' Dataset with the coordinates for the lentils evaluated for their resistance to rust in the future.
* 'DatosPresente/SupplementaryTable3_RubioTeso_etal_Martonne_bio12.xlsx' Dataset with the bioclimatic variables, DSR and coordinates for the crop lentils in the present.
* 'DatosPrecip/Len_silvestres_futuro_B1_B12.txt' Dataset with the coordinates for the crop lentils in the future.




## Machine Learning scripts

All machine learning scripts are written in Python and located at `Lentils/Futuro` folder.

#### future_lens_resistance_all_models-Ridge.ipynb

This script builds 500 Ridge Regression predictors.

Prediction errors (RMSE) by model are stored at:
* `results/Roya_Presente_all_iter_Ridge.xlsx` 
* `results/Roya_Future_all_iter_Ridge.xlsx`
* `results/Silvestres_Presente_alliter_Ridge.xlsx`
* `results/Silvestres_Future_alliter_Ridge.xlsx`

Invocation: `future_lens_resistance_all_models-Ridge.ipynb`





## Feature importance scripts



## Post-prediction scripts


