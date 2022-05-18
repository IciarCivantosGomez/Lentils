# README #

This repository contains the software, data and reproducibility instructions of a set of machine learning models to predict rust sensitivity (DSr).

Please, clone the repo and follow these steps.

## Data

There are four clean datasets called:
* `DatosPresente/fichtraining_ROYA_CAMPO_nondup.csv` Dataset with the bioclimatic variables, DSR and coordinates for the lentils evaluated for their resistance to rust in the present.
* `DatosPrecip/ROYA_Completa_fut_pres.txt` Dataset with the coordinates for the lentils evaluated for their resistance to rust in the future.
* `DatosPresente/SupplementaryTable3_RubioTeso_etal_Martonne_bio12.xlsx` Dataset with the bioclimatic variables, DSR and coordinates for the crop lentils in the present.
* `DatosPrecip/Len_silvestres_futuro_B1_B12.txt` Dataset with the coordinates for the crop lentils in the future.



## Machine Learning scripts


TODO ESTO ES LO QUE TENIAMOS EN CRACOLES, ADAPTAR A LENTEJAS

====> DESDE AQUI =====>

All machine learning scripts are written in Python and located at `Lentils/Pyscripts` folder.

#### Future/future_lens_resistance_all_models-Ridge.ipynb

This script builds 500 Ridge Regression predictors.

DSR predictions by model are stored at:
* `results/Roya_Presente_all_iter_Ridge.xlsx` 
* `results/Roya_Future_all_iter_Ridge.xlsx`
* `results/Silvestres_Presente_alliter_Ridge.xlsx`
* `results/Silvestres_Future_alliter_Ridge.xlsx`

Median DSR predictions are also calculated and stored at:
* `results/Roya_Presente_avg_Ridge.xlsx` 
* `results/Roya_Future_avg_Ridge.xlsx`
* `results/Silvestres_Presente_avg_Ridge.xlsx`
* `results/Silvestres_Future_avg_Ridge.xlsx`

Invocation: `future_lens_resistance_all_models-Ridge.ipynb`

#### Future/future_lens_resistance_all_models-RF.ipynb

This script builds 500 Random Forest predictors.

DSR predictions by model are stored at:
* `results/Roya_Presente_all_iter_RF.xlsx` 
* `results/Roya_Future_all_iter_RF.xlsx`
* `results/Silvestres_Presente_alliter_RF.xlsx`
* `results/Silvestres_Future_alliter_RF.xlsx`

Median DSR predictions are also calculated and stored at:
* `results/Roya_Presente_avg_RF.xlsx` 
* `results/Roya_Future_avg_RF.xlsx`
* `results/Silvestres_Presente_avg_RF.xlsx`
* `results/Silvestres_Future_avg_RF.xlsx`

Invocation: `future_lens_resistance_all_models-RF.ipynb`

#### Future/future_lens_resistance_all_models-XGBOOST.ipynb

This script builds 500 XGBOOST Regression predictors.

DSR predictions by model are stored at:
* `results/Roya_Presente_all_iter_XGBOOST.xlsx` 
* `results/Roya_Future_all_iter_XGBOOST.xlsx`
* `results/Silvestres_Presente_alliter_XGBOOST.xlsx`
* `results/Silvestres_Future_alliter_XGBOOST.xlsx`

Median DSR predictions are also calculated and stored at:
* `results/Roya_Presente_avg_XGBOOST.xlsx` 
* `results/Roya_Future_avg_XGBOOST.xlsx`
* `results/Silvestres_Presente_avg_XGBOOST.xlsx`
* `results/Silvestres_Future_avg_XGBOOST.xlsx`

Invocation: `future_lens_resistance_all_models-XGBOOST.ipynb`


## Feature correlation script

Located at `future_lens_resistance_corr.ipynb`. This scripts produces two different correlation matrices. First matrix returns
correlations among the predictors. The second matrix shows correlations between the target variable and the predictors.

Invoke once: `future_lens_resistance_corr.ipynb`


====> HASTA AQUÍ ====>


====> LO QUE SIGUE ES LA PARTE R Y YA ESTÁ ACTUALIZADA =====>



## Post-prediction scripts

## Exploratory analysis

You will need R 4.0.1 or later. Go to the R_Code folder and run `merge_results.R` and the `paint_maps.R`

- Results 

  * tables/model_Errors_AVG.csv              # Average, median and cumulative RMSE for lab sample and model in the present time
    tables/RMSE_summary.csv                  # Tukey's five numbers summary of RMSE for each model
    tables/Roya_Presente_merged_ALL.csv      # DSr prediction values for each individual experiment for lab samples
    tables/model_KSdist.csv                  # Kolmogorov Smirnov distante between measured DSr and predicted Dsr in the present time
    tables/data_map_,modelprec,_,CChange_model,.csv  # Intermediate data for plotting maps
    tables/HighValueSamples_,modelprec,_,CChange_model,.csv # Most valuable samples for conservation
    tables/
  * plots/predictions_errors (both .png and .eps) # RMSE histograms
    plots/hist_preds_CChangeModel            # Histogram of predicted values for wild samples 
    plots/mapa_lens_*, mapa_greece_*, mapa_west_* # Individual and comparative maps
    
