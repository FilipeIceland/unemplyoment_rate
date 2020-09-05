setwd("C:/Users/mypa4/Documents/ITB/MSIT H6029 - MSc Research Project/00.MScProject")


source("My_functions.R") #import functions

import_libraries() #load libraries

chapter2() #run functions for plots in chapter 2

filter_test_m<-c(2018,12) #breakpoint between training set and test set for monthly data
filter_test_qt<-c(2018,4) #breakpoint between training set and test set for quarterly data


eurostat<-etl_eurostat() #load and transform eurostat data
ne_10m_admin_0_countries<-etl_ne_countries() #load and transform natural earth data
OECD<-etl_oecd(ne_10m_admin_0_countries[c('ADM0_A3','ADMIN')]) #load and transform oecd data

#plot intial maps
plot_map_save(ne_10m_admin_0_countries,unique(eurostat$COUNTRY),
         'Countries in eurostat data',"03.Chapter03/03.Images/eurostat_countries.png",c(25, 85),c(-160,150),n_cols=4)
plot_map_save(ne_10m_admin_0_countries,unique(OECD$COUNTRY),
         'Countries in OECD data',"03.Chapter03/03.Images/OECD_countries.png",c(-60, 85),n_cols=4)
plot_map_save(ne_10m_admin_0_countries,unique(ne_10m_admin_0_countries$ADMIN),
              '',"03.Chapter03/03.Images/NE_countries.png",c(-60, 85),leg_position = "none")

neigh_list<-get_ne_neighbour_countries(ne_10m_admin_0_countries) #find lists of neighbours

eurostat_DF<-pivot_byCountry_filter(eurostat,c(2000,1),c(2019,12)) #apply filter for eurostat data and reshape
finalEurostat_DF<-filter_with_neighbours(eurostat_DF,neigh_list) #filter countries with data for eurostat data

#plot final map for eurostat dataset
plot_map_save(ne_10m_admin_0_countries,names(finalEurostat_DF[-1]),'',
              "03.Chapter03/03.Images/final_eurostat_countries.png",c(35, 70),c(-15,35),leg_position ="right",n_cols=2)
save_ts_acf_pcf_ndiffs(finalEurostat_DF,"Eurostat",filter_test_m) #store acf, pacf, and ndiffs for eurostat dataset

#cluster analysis for eurostat dataset
eval_kmeans(finalEurostat_DF,"Eurostat") 
plot_map_save(ne_10m_admin_0_countries,names(finalEurostat_DF)[-1],'',
              "04.Chapter04/04.Images/Eurostat/eurostat_countries_3clusters.png",c(35, 70),c(-15,35),
              colour_by=get_clusters(finalEurostat_DF,3),n_cols=6)
plot_map_save(ne_10m_admin_0_countries,names(finalEurostat_DF)[-1], '',
              "04.Chapter04/04.Images/Eurostat/eurostat_countries_5clusters.png",c(35, 70),c(-15,35),
              colour_by=get_clusters(finalEurostat_DF,5),n_cols=6)
plot_map_save(ne_10m_admin_0_countries,names(finalEurostat_DF)[-1], '',
              "04.Chapter04/04.Images/Eurostat/eurostat_countries_9clusters.png",c(35, 70),c(-15,35),
              colour_by=get_clusters(finalEurostat_DF,9),n_cols=6)
kDTW_Eurostat<-kmeans_DTW(finalEurostat_DF,filter_test_qt,9)
plot_map_save(ne_10m_admin_0_countries,names(kDTW_Eurostat), '',
              "04.Chapter04/04.Images/Eurostat/Eurostat_countries_9dtw.png",c(35, 70),c(-15,35),
              colour_by=kDTW_Eurostat,leg_position ="right",n_cols=2)

#store cross-correlation for eurostat dataset
CCM(finalEurostat_DF,'Eurostat',0,'_lag0',0)
CCM(finalEurostat_DF,'Eurostat',c(1,2,6,12,18,24),'_lag1to24',24)

neigh_list_to_latex(neigh_list,finalEurostat_DF,'Eurostat') #store list of neighbours for eurostat dataset


OECD_DF<-pivot_byCountry_filter(OECD,c(2003,1),c(2019,12)) #apply filter for oecd data and reshape
finalOECD_DF<-filter_with_neighbours(OECD_DF,neigh_list) #filter countries with data for oecd data

#plot final map for oecd dataset
plot_map_save(ne_10m_admin_0_countries,names(finalOECD_DF)[-1], 'Countries in OECD dataset',
              "03.Chapter03/03.Images/final_OECD_countries.png",c(15, 75),c(-160,35),n_cols=5)

save_ts_acf_pcf_ndiffs(finalOECD_DF,"OECD",filter_test_qt) #store acf, pacf, and ndiffs for oecd dataset

#cluster analysis for oecd dataset
eval_kmeans(finalOECD_DF,"OECD")
plot_map_save(ne_10m_admin_0_countries,names(finalOECD_DF)[-1], '',
              "04.Chapter04/04.Images/OECD/OECD_countries_6clusters.png",c(15, 75),c(-160,35),
              colour_by=get_clusters(finalOECD_DF,6),n_cols=5)
plot_map_save(ne_10m_admin_0_countries,names(finalOECD_DF)[-1], '',
              "04.Chapter04/04.Images/OECD/OECD_countries_10clusters.png",c(15, 75),c(-160,35),
              colour_by=get_clusters(finalOECD_DF,10),n_cols=5)

kDTW_OECD<-kmeans_DTW(finalOECD_DF,filter_test_qt,10)
plot_map_save(ne_10m_admin_0_countries,names(kDTW_OECD), '',
              "04.Chapter04/04.Images/OECD/OECD_countries_10dtw.png",c(15, 75),c(-160,35),
              colour_by=kDTW_OECD,n_cols=5)

#store cross-correlation for eecd dataset
CCM(finalOECD_DF,'OECD',c(0,1,2,3,4,8),8)
#store cross-correlation for oecd dataset
neigh_list_to_latex(neigh_list,finalOECD_DF,'OECD')

#eurostat dataset - run classic models for univariate time series
resultsEU_uvm<-run_Rforecast_models(finalEurostat_DF,filter_test_m)

#eurostat dataset - run ML algorithms for univariate time series
resultsEU_uvm_ml_3_0<-run_ML_uvm_models(finalEurostat_DF,filter_test_m,3,0)
resultsEU_uvm_ml_4_0<-run_ML_uvm_models(finalEurostat_DF,filter_test_m,4,0)
resultsEU_uvm_ml_6_0<-run_ML_uvm_models(finalEurostat_DF,filter_test_m,6,0)
resultsEU_uvm_ml_3_1<-run_ML_uvm_models(finalEurostat_DF,filter_test_m,3,1)
resultsEU_uvm_ml_4_1<-run_ML_uvm_models(finalEurostat_DF,filter_test_m,4,1)
resultsEU_uvm_ml_6_1<-run_ML_uvm_models(finalEurostat_DF,filter_test_m,6,1)

#eurostat dataset - merge results of ML algorithms for univariate time series
resultsEU_uvm_ml1<-merge_by_rowsnames(list(resultsEU_uvm_ml_3_0$Scores1,resultsEU_uvm_ml_4_0$Scores1,resultsEU_uvm_ml_6_0$Scores1,resultsEU_uvm_ml_3_1$Scores1,resultsEU_uvm_ml_4_1$Scores1,resultsEU_uvm_ml_6_1$Scores1))
resultsEU_uvm_RF1<- dplyr::select(resultsEU_uvm_ml1, dplyr::starts_with("RF"))
resultsEU_uvm_ANN1<- dplyr::select(resultsEU_uvm_ml1, dplyr::starts_with("ANN"))
resultsEU_uvm_ml12<-merge_by_rowsnames(list(resultsEU_uvm_ml_3_0$ScoresH,resultsEU_uvm_ml_4_0$ScoresH,resultsEU_uvm_ml_6_0$ScoresH,resultsEU_uvm_ml_3_1$ScoresH,resultsEU_uvm_ml_4_1$ScoresH,resultsEU_uvm_ml_6_1$ScoresH))
resultsEU_uvm_RF12<- dplyr::select(resultsEU_uvm_ml12, dplyr::starts_with("RF"))
resultsEU_uvm_ANN12<- dplyr::select(resultsEU_uvm_ml12, dplyr::starts_with("ANN"))
resultsEU_uvm_mlT<-merge_by_rowsnames(list(resultsEU_uvm_ml_3_0$ScoresT,resultsEU_uvm_ml_4_0$ScoresT,resultsEU_uvm_ml_6_0$ScoresT,resultsEU_uvm_ml_3_1$ScoresT,resultsEU_uvm_ml_4_1$ScoresT,resultsEU_uvm_ml_6_1$ScoresT))
resultsEU_uvm_RFT<- dplyr::select(resultsEU_uvm_mlT, dplyr::starts_with("RF"))
resultsEU_uvm_ANNT<- dplyr::select(resultsEU_uvm_mlT, dplyr::starts_with("ANN"))

#eurostat dataset - merge results for classic models and best ML algorithms for univariate time series
resultsEU_uvm_final1<-merge_by_rowsnames(list(resultsEU_uvm$Scores1,resultsEU_uvm_ml1[,c('RF3d1','ANN3d1')]))
resultsEU_uvm_final12<-merge_by_rowsnames(list(resultsEU_uvm$ScoresH,resultsEU_uvm_ml12[,c('RF3d1','ANN3d1')]))
resultsEU_uvm_finalT<-merge_by_rowsnames(list(resultsEU_uvm$ScoresT,resultsEU_uvm_mlT[,c('RF3d1','ANN3d1')]))

#eurostat dataset - run VAR models
resultsEU_VAR<-run_VARmodel(neigh_list,finalEurostat_DF,filter_test_m)

#eurostat dataset - run ML algorithms for multivariate time series
resultsEU_MTS_ML2r<-run_recursive_MTS_ML(neigh_list,finalEurostat_DF,filter_test_m,2)
resultsEU_MTS_ML3r<-run_recursive_MTS_ML(neigh_list,finalEurostat_DF,filter_test_m,3)
resultsEU_MTS_ML4r<-run_recursive_MTS_ML(neigh_list,finalEurostat_DF,filter_test_m,4)
resultsEU_MTS_ML6r<-run_recursive_MTS_ML(neigh_list,finalEurostat_DF,filter_test_m,6)
resultsEU_MTS_ML2d1r<-run_recursive_MTS_ML(neigh_list,finalEurostat_DF,filter_test_m,2,1)
resultsEU_MTS_ML3d1r<-run_recursive_MTS_ML(neigh_list,finalEurostat_DF,filter_test_m,3,1)
resultsEU_MTS_ML4d1r<-run_recursive_MTS_ML(neigh_list,finalEurostat_DF,filter_test_m,4,1)
resultsEU_MTS_ML6d1r<-run_recursive_MTS_ML(neigh_list,finalEurostat_DF,filter_test_m,6,1,models = 'RF')

#eurostat dataset - merge results of ML algorithms for multivariate time series
resultsEU_MTS_ML_1<-merge_by_rowsnames(list(resultsEU_MTS_ML2r$RMSE1,resultsEU_MTS_ML3r$RMSE1,resultsEU_MTS_ML4r$RMSE1,resultsEU_MTS_ML6r$RMSE1,resultsEU_MTS_ML2d1r$RMSE1,resultsEU_MTS_ML3d1r$RMSE1,resultsEU_MTS_ML4d1r$RMSE1,resultsEU_MTS_ML6d1r$RMSE1))
resultsEU_MTS_ML_1_RF<- dplyr::select(resultsEU_MTS_ML_1, dplyr::starts_with("mRF"))
resultsEU_MTS_ML_1_ANN<- dplyr::select(resultsEU_MTS_ML_1, dplyr::starts_with("mANN"))
resultsEU_MTS_ML_r<-merge_by_rowsnames(list(resultsEU_MTS_ML2r$RMSEH,resultsEU_MTS_ML3r$RMSEH,resultsEU_MTS_ML4r$RMSEH,resultsEU_MTS_ML6r$RMSEH,resultsEU_MTS_ML2d1r$RMSEH,resultsEU_MTS_ML3d1r$RMSEH,resultsEU_MTS_ML4d1r$RMSEH,resultsEU_MTS_ML6d1r$RMSEH))
resultsEU_MTS_ML_r_RF<- dplyr::select(resultsEU_MTS_ML_r, dplyr::starts_with("mRF"))
resultsEU_MTS_ML_r_ANN<- dplyr::select(resultsEU_MTS_ML_r, dplyr::starts_with("mANN"))

#eurostat dataset - merge results for classic models and best ML algorithms
EUfinalScores1<-merge_by_rowsnames(list(resultsEU_uvm_final1,dplyr::select(resultsEU_VAR$RMSE1,'VAR(2)'),dplyr::select(resultsEU_MTS_ML_1_RF,'mRF3d1'),dplyr::select(resultsEU_MTS_ML_1_ANN,'mANN2d0')))
EUfinalScores12<-merge_by_rowsnames(list(resultsEU_uvm_final12,dplyr::select(resultsEU_VAR$RMSEH,'VAR(2)'),dplyr::select(resultsEU_MTS_ML_r_RF,'mRF6d1'),dplyr::select(resultsEU_MTS_ML_r_ANN,'mANN2d0')))

#eurostat dataset - store results in tex files
DF_to_Latex(resultsEU_uvm$Models_strings,'05.Chapter05/05.Latex/','EurostatUnivariateModels','Eurostat: univariate models','eurostatunimodels',rowname='COUNTRY')
DF_to_Latex(round(resultsEU_uvm_RF1,4),'05.Chapter05/05.Latex/','EurostatRF1Step','Eurostat: RF - forecast 1 step','eurostatrff1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_uvm_RF1),'05.Chapter05/05.Latex/','EurostatRF1StepMean','Eurostat: RF mean - forecast 1 step','eurostatrff1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_uvm_RF12,4),'05.Chapter05/05.Latex/','EurostatRF12Steps','Eurostat: RF - 12-steps forecast','eurostatrff12',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_uvm_RF12),'05.Chapter05/05.Latex/','EurostatRF12StepsMean','Eurostat: RF mean - 12-steps forecast','eurostatrff12mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_uvm_RFT,4),'05.Chapter05/05.Latex/','EurostatRFTraining','Eurostat: RF - training set','eurostatrfft',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_uvm_RFT),'05.Chapter05/05.Latex/','EurostatRFTrainingMean','Eurostat: RF mean - training set','eurostatrfftmean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_uvm_ANN1,4),'05.Chapter05/05.Latex/','EurostatANN1Step','Eurostat: ANN - forecast 1 step','eurostatANNf1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_uvm_ANN1),'05.Chapter05/05.Latex/','EurostatANN1StepMean','Eurostat: ANN mean - forecast 1 step','eurostatANNf1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_uvm_ANN12,4),'05.Chapter05/05.Latex/','EurostatANN12Steps','Eurostat: ANN - 12-steps forecast','eurostatANNf12',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_uvm_ANN12),'05.Chapter05/05.Latex/','EurostatANN12StepsMean','Eurostat: ANN mean - 12-steps forecast','eurostatANNf12mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_uvm_ANNT,4),'05.Chapter05/05.Latex/','EurostatANNTraining','EU: ANN - training set','eurostatannft',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_uvm_ANNT),'05.Chapter05/05.Latex/','EurostatANNTrainingMean','EU: ANN mean - training set','eurostatannftmean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_uvm_final1,4),'05.Chapter05/05.Latex/','EurostatUnivariateModelsOneStep','Eurostat: models for univariate time series - RMSE per country - 12 one-step forecasts','eurostatunimodelsf1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_uvm_final1),'05.Chapter05/05.Latex/','EurostatUnivariateModelsOneStepMean','Eurostat: models for univariate time series - global results -  12 one-step forecasts','eurostatunimodelsf1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_uvm_final12,4),'05.Chapter05/05.Latex/','EurostatUnivariateModels12Steps','Eurostat: models for univariate time series - RMSE per country - 12-steps forecast','eurostatunimodelsf12',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_uvm_final12),'05.Chapter05/05.Latex/','EurostatUnivariateModels12StepsMean','Eurostat: models for univariate time series - global results -  12-steps forecast','eurostatunimodelsf12mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_uvm_finalT,4),'05.Chapter05/05.Latex/','EurostatUnivariateModelsTraining','Eurostat: models for univariate time series - RMSE per country - training set','eurostatunimodelstraining',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_uvm_finalT),'05.Chapter05/05.Latex/','EurostatUnivariateModelsTrainingMean','Eurostat: models for univariate time series - global results -  training set','eurostatunimodelstrainingmean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_VAR$RMSE1,4),'05.Chapter05/05.Latex/','EurostatVAR1Step','Eurostat: VAR - 1-step forecast','eurostatvarf1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_VAR$RMSE1),'05.Chapter05/05.Latex/','EurostatVAR1StepMean','Eurostat: VAR mean - 1-step forecast','eurostatvarf1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_VAR$RMSEH,4),'05.Chapter05/05.Latex/','EurostatVAR12Steps','Eurostat: VAR - 12-steps forecast','eurostatvarf12',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_VAR$RMSEH),'05.Chapter05/05.Latex/','EurostatVAR12StepsMean','Eurostat: VAR mean - 12-steps forecast','eurostatvarf12mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_VAR$AIC,4),'05.Chapter05/05.Latex/','EurostatVARaic','Eurostat: VAR - AIC','eurostatvaraic',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(round(resultsEU_MTS_ML_1_RF,4),'05.Chapter05/05.Latex/','EurostatMTSRF1Step','Eurostat: mts RF - 1-step forecast','eurostatmtsrff1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_MTS_ML_1_RF),'05.Chapter05/05.Latex/','EurostatMTSRF1StepMean','Eurostat: mts RF mean - 1-step forecast','eurostatmtsrff1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_MTS_ML_r_RF,4),'05.Chapter05/05.Latex/','EurostatMTSRF12Steps','Eurostat: mts RF - 12-steps forecast','eurostatmtsrff12',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_MTS_ML_r_RF),'05.Chapter05/05.Latex/','EurostatMTSRF12StepsMean','Eurostat: mts RF mean - 12-steps forecast','eurostatmtsrff12mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_MTS_ML_1_ANN,4),'05.Chapter05/05.Latex/','EurostatMTSANN1Step','Eurostat: mts ANN - 1-step forecast','eurostatmtsannf1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_MTS_ML_1_ANN),'05.Chapter05/05.Latex/','EurostatMTSANN1StepMean','Eurostat: mts ANN mean - 1-step forecast','eurostatmtsannf1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsEU_MTS_ML_r_ANN,4),'05.Chapter05/05.Latex/','EurostatMTSANN12Steps','Eurostat: mts ANN - 12-steps forecast','eurostatmtsannf12',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsEU_MTS_ML_r_ANN),'05.Chapter05/05.Latex/','EurostatMTSANN12StepsMean','Eurostat: mts ANN mean - 12-steps forecast','eurostatmtsannf12mean',rowname='',colour=TRUE)
DF_to_Latex(round(EUfinalScores1,4),'05.Chapter05/05.Latex/','EurostatModels1StepALL','Eurostat: models for univariate and multivariate time series - RMSE per country - 1 one-step forecasts','eurostatmodelsf1all',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(EUfinalScores1),'05.Chapter05/05.Latex/','EurostatModels1StepMeanALL','Eurostat: models for univariate and multivariate time series - global results -  1 one-step forecasts','eurostatmodelsf1allmean',rowname='',colour=TRUE)
DF_to_Latex(round(EUfinalScores12,4),'05.Chapter05/05.Latex/','EurostatModels12StepsALL','Eurostat: models for univariate and multivariate time series - RMSE per country - 12 one-step forecasts','eurostatmodelsf12all',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(EUfinalScores12),'05.Chapter05/05.Latex/','EurostatModels12StepsMeanALL','Eurostat: models for univariate and multivariate time series - global results -  12 one-step forecasts','eurostatmodelsf12allmean',rowname='',colour=TRUE)

#eurostat dataset - plot best models
plot_best_models(EUfinalScores1,'Eurostat','best models for 1-step forecasts',c(-15,35),c(35, 70))
plot_best_models(EUfinalScores1[,c('SARIMA','VAR(2)')],'Eurostat','best models for 1-step forecasts - SARIMA vs VAR',c(-15,35),c(35, 70))
plot_best_models(EUfinalScores1[,c('ARFIMA','VAR(2)')],'Eurostat','best models for 1-step forecasts - ARFIMA vs VAR',c(-15,35),c(35, 70))
plot_best_models2(EUfinalScores1,'Eurostat','best models for 1-step forecasts - classic vs ML',c(-15,35),c(35, 70))
plot_best_models2(EUfinalScores1[,c('SARIMA','RF3d1')],'Eurostat','best models for 1-step forecasts - SARIMA vs RF3d1',c(-15,35),c(35, 70))
plot_best_models2(EUfinalScores1[,c('ARFIMA','RF3d1')],'Eurostat','best models for 1-step forecasts - ARFIMA vs RF3d1',c(-15,35),c(35, 70))
plot_best_models(EUfinalScores12,'Eurostat','best models for 12 recursive forecasts',c(-15,35),c(35, 70))
plot_best_models(EUfinalScores12[,c('SARIMA','ARFIMA')],'Eurostat','best models for 12 recursive forecasts - SARIMA vs ARFIMA',c(-15,35),c(35, 70))
plot_best_models(EUfinalScores12[c('SARIMA','VAR(2)')],'Eurostat','best models for 12 recursive forecasts - SARIMA vs VAR',c(-15,35),c(35, 70))
plot_best_models(EUfinalScores12[c('ARFIMA','VAR(2)')],'Eurostat','best models for 12 recursive forecasts - ARFIMA vs VAR',c(-15,35),c(35, 70))
plot_best_models(EUfinalScores12[,c('RF3d1','mRF6d1')],'Eurostat','best models for 12 recursive forecasts - RF vs mRF',c(-15,35),c(35, 70))
plot_best_models(EUfinalScores12[,c('ANN/AR','ANN3d1','mANN2d0')],'Eurostat','best models for 12 recursive forecasts - ANN vs mANN',c(-15,35),c(35, 70))
plot_best_models2(EUfinalScores12,'Eurostat','best models for 12-steps forecasts - classic vs ML',c(-15,35),c(35, 70))
plot_best_models2(EUfinalScores12[,c('SARIMA','RF3d1')],'Eurostat','best models for 12-steps forecasts - SARIMA vs RF3d1',c(-15,35),c(35, 70))
plot_best_models2(EUfinalScores12[,c('ARFIMA','RF3d1')],'Eurostat','best models for 12-steps forecasts - ARFIMA vs RF3d1',c(-15,35),c(35, 70))
plot_best_models2(EUfinalScores12[,c('VAR(2)','mRF6d1')],'Eurostat','best models for 12-steps forecasts - VAR(2) vs mRF6d1',c(-15,35),c(35, 70))
plot_best_models2(EUfinalScores12[,c('VAR(2)','mANN2d0')],'Eurostat','best models for 12-steps forecasts - VAR(2) vs mANN2d0',c(-15,35),c(35, 70))

#oecd dataset - run classic models for univariate time series
resultsOECD_uvm<-run_Rforecast_models(finalOECD_DF,filter_test_qt)

#oecd dataset - run ML algorithms for univariate time series
resultsOECD_uvm_ml_2_0<-run_ML_uvm_models(finalOECD_DF,filter_test_qt,2,0)
resultsOECD_uvm_ml_3_0<-run_ML_uvm_models(finalOECD_DF,filter_test_qt,3,0)
resultsOECD_uvm_ml_4_0<-run_ML_uvm_models(finalOECD_DF,filter_test_qt,4,0)
resultsOECD_uvm_ml_2_1<-run_ML_uvm_models(finalOECD_DF,filter_test_qt,2,1)
resultsOECD_uvm_ml_3_1<-run_ML_uvm_models(finalOECD_DF,filter_test_qt,3,1)
resultsOECD_uvm_ml_4_1<-run_ML_uvm_models(finalOECD_DF,filter_test_qt,4,1)
resultsOECD_uvm_ml_5_1<-run_ML_uvm_models(finalOECD_DF,filter_test_qt,5,1)
resultsOECD_uvm_ml_6_1<-run_ML_uvm_models(finalOECD_DF,filter_test_qt,6,1)

#oecd dataset - merge results of ML algorithms for univariate time series
resultsOECD_uvm_ml1<-merge_by_rowsnames(list(resultsOECD_uvm_ml_2_0$Scores1,resultsOECD_uvm_ml_3_0$Scores1,resultsOECD_uvm_ml_4_0$Scores1,resultsOECD_uvm_ml_2_1$Scores1,resultsOECD_uvm_ml_3_1$Scores1,resultsOECD_uvm_ml_4_1$Scores1,resultsOECD_uvm_ml_5_1$Scores1,resultsOECD_uvm_ml_6_1$Scores1))
resultsOECD_uvm_RF1<- dplyr::select(resultsOECD_uvm_ml1, dplyr::starts_with("RF"))
resultsOECD_uvm_ANN1<- dplyr::select(resultsOECD_uvm_ml1, dplyr::starts_with("ANN"))
resultsOECD_uvm_ml4<-merge_by_rowsnames(list(resultsOECD_uvm_ml_2_0$ScoresH,resultsOECD_uvm_ml_3_0$ScoresH,resultsOECD_uvm_ml_4_0$ScoresH,resultsOECD_uvm_ml_2_1$ScoresH,resultsOECD_uvm_ml_3_1$ScoresH,resultsOECD_uvm_ml_4_1$ScoresH,resultsOECD_uvm_ml_5_1$ScoresH,resultsOECD_uvm_ml_6_1$ScoresH))
resultsOECD_uvm_RF4<- dplyr::select(resultsOECD_uvm_ml4, dplyr::starts_with("RF"))
resultsOECD_uvm_ANN4<- dplyr::select(resultsOECD_uvm_ml4, dplyr::starts_with("ANN"))
resultsOECD_uvm_mlT<-merge_by_rowsnames(list(resultsOECD_uvm_ml_2_0$ScoresT,resultsOECD_uvm_ml_3_0$ScoresT,resultsOECD_uvm_ml_4_0$ScoresT,resultsOECD_uvm_ml_2_1$ScoresT,resultsOECD_uvm_ml_3_1$ScoresT,resultsOECD_uvm_ml_4_1$ScoresT,resultsOECD_uvm_ml_5_1$ScoresT,resultsOECD_uvm_ml_6_1$ScoresT))
resultsOECD_uvm_RFT<- dplyr::select(resultsOECD_uvm_mlT, dplyr::starts_with("RF"))
resultsOECD_uvm_ANNT<- dplyr::select(resultsOECD_uvm_mlT, dplyr::starts_with("ANN"))

#oecd dataset - merge results for classic models and best ML algorithms
resultsOECD_uvm_final1<-merge_by_rowsnames(list(resultsOECD_uvm$Scores1,resultsOECD_uvm_ml1[,c('RF5d1','ANN2d0')]))
resultsOECD_uvm_final4<-merge_by_rowsnames(list(resultsOECD_uvm$ScoresH,resultsOECD_uvm_ml4[,c('RF5d1','ANN2d1')]))
resultsOECD_uvm_finalT<-merge_by_rowsnames(list(resultsOECD_uvm$ScoresT,resultsOECD_uvm_mlT[,c('RF5d1','ANN2d1')]))

#oecd dataset - run VAR models
resultsOECD_VAR<-run_VARmodel(neigh_list,finalOECD_DF,filter_test_qt)

#oecd dataset - run ML algorithms for multivariate time series
resultsOECD_MTS_ML1r<-run_recursive_MTS_ML(neigh_list,finalOECD_DF,filter_test_qt,1,models='ANN',nodes=10)
resultsOECD_MTS_ML2r<-run_recursive_MTS_ML(neigh_list,finalOECD_DF,filter_test_qt,2,models='ANN',nodes=10)
resultsOECD_MTS_ML3r<-run_recursive_MTS_ML(neigh_list,finalOECD_DF,filter_test_qt,3,nodes=10)
resultsOECD_MTS_ML4r<-run_recursive_MTS_ML(neigh_list,finalOECD_DF,filter_test_qt,4,nodes=10)
resultsOECD_MTS_ML1d1r<-run_recursive_MTS_ML(neigh_list,finalOECD_DF,filter_test_qt,1,1,nodes=10)
resultsOECD_MTS_ML2d1r<-run_recursive_MTS_ML(neigh_list,finalOECD_DF,filter_test_qt,2,1,nodes=10)
resultsOECD_MTS_ML3d1r<-run_recursive_MTS_ML(neigh_list,finalOECD_DF,filter_test_qt,3,1,nodes=10)
resultsOECD_MTS_ML4d1r<-run_recursive_MTS_ML(neigh_list,finalOECD_DF,filter_test_qt,4,1,models='RF')
resultsOECD_MTS_ML5d1r<-run_recursive_MTS_ML(neigh_list,finalOECD_DF,filter_test_qt,5,1,models='RF')
resultsOECD_MTS_ML6d1r<-run_recursive_MTS_ML(neigh_list,finalOECD_DF,filter_test_qt,6,1,models='RF')

#oecd dataset - merge results of ML algorithms for multivariate time series
resultsOECD_MTS_ML_1<-merge_by_rowsnames(list(resultsOECD_MTS_ML1r$RMSE1,resultsOECD_MTS_ML2r$RMSE1,resultsOECD_MTS_ML3r$RMSE1,resultsOECD_MTS_ML4r$RMSE1,resultsOECD_MTS_ML1d1r$RMSE1,resultsOECD_MTS_ML2d1r$RMSE1,resultsOECD_MTS_ML3d1r$RMSE1,resultsOECD_MTS_ML4d1r$RMSE1,resultsOECD_MTS_ML5d1r$RMSE1,resultsOECD_MTS_ML6d1r$RMSE1))
resultsOECD_MTS_ML_1_RF<- dplyr::select(resultsOECD_MTS_ML_1, dplyr::starts_with("mRF"))
resultsOECD_MTS_ML_1_ANN<- dplyr::select(resultsOECD_MTS_ML_1, dplyr::starts_with("mANN"))
resultsOECD_MTS_ML_r<-merge_by_rowsnames(list(resultsOECD_MTS_ML1r$RMSEH,resultsOECD_MTS_ML2r$RMSEH,resultsOECD_MTS_ML3r$RMSEH,resultsOECD_MTS_ML4r$RMSEH,resultsOECD_MTS_ML1d1r$RMSEH,resultsOECD_MTS_ML2d1r$RMSEH,resultsOECD_MTS_ML3d1r$RMSEH,resultsOECD_MTS_ML4d1r$RMSEH,resultsOECD_MTS_ML5d1r$RMSEH,resultsOECD_MTS_ML6d1r$RMSEH))
resultsOECD_MTS_ML_r_RF<- dplyr::select(resultsOECD_MTS_ML_r, dplyr::starts_with("mRF"))
resultsOECD_MTS_ML_r_ANN<- dplyr::select(resultsOECD_MTS_ML_r, dplyr::starts_with("mANN"))

#oecd dataset - merge results for classic models and best ML algorithms
OECDfinalScores1<-merge_by_rowsnames(list(resultsOECD_uvm_final1,dplyr::select(resultsOECD_VAR$RMSE1,'VAR(1)'),dplyr::select(resultsOECD_MTS_ML_1_RF,'mRF5d1'),dplyr::select(resultsOECD_MTS_ML_1_ANN,'mANN1d0')))
OECDfinalScores4<-merge_by_rowsnames(list(resultsOECD_uvm_final4,dplyr::select(resultsOECD_VAR$RMSEH,'VAR(1)'),dplyr::select(resultsOECD_MTS_ML_r_RF,'mRF5d1'),dplyr::select(resultsOECD_MTS_ML_r_ANN,'mANN3d0')))

#oecd dataset - store results in tex files
DF_to_Latex(resultsOECD_uvm$Models_strings,'05.Chapter05/05.Latex/','OECDUnivariateModels','OECD: univariate models','oecdunimodels',rowname='COUNTRY')
DF_to_Latex(round(resultsOECD_uvm_RF1,4),'05.Chapter05/05.Latex/','OECDRF1Step','OECD: RF - forecast 1 step','oecdrff1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_uvm_RF1),'05.Chapter05/05.Latex/','OECDRF1StepMean','OECD: RF mean - forecast 1 step','oecdrff1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_uvm_RF4,4),'05.Chapter05/05.Latex/','OECDRF4Steps','OECD: RF - 4-steps forecast','oecdrff4',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_uvm_RF4),'05.Chapter05/05.Latex/','OECDRF4StepsMean','OECD: RF mean - 4-steps forecast','oecdrff4mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_uvm_RFT,4),'05.Chapter05/05.Latex/','OECDRFTraining','OECD: RF - training set','oecdrfft',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_uvm_RFT),'05.Chapter05/05.Latex/','OECDRFTrainingMean','OECD: RF mean - training set','oecdrfftmean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_uvm_ANN1,4),'05.Chapter05/05.Latex/','OECDANN1Step','OECD: ANN - forecast 1 step','oecdANNf1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_uvm_ANN1),'05.Chapter05/05.Latex/','OECDANN1StepMean','OECD: ANN mean - forecast 1 step','oecdANNf1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_uvm_ANN4,4),'05.Chapter05/05.Latex/','OECDANN4Steps','OECD: ANN - 4-steps forecast','oecdANNf4',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_uvm_ANN4),'05.Chapter05/05.Latex/','OECDANN4StepsMean','OECD: ANN mean - 4-steps forecast','oecdANNf4mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_uvm_ANNT,4),'05.Chapter05/05.Latex/','OECDANNTraining','OECD: ANN - training set','oecdannft',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_uvm_ANNT),'05.Chapter05/05.Latex/','OECDANNTrainingMean','OECD: ANN mean - training set','oecdannftmean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_uvm_final1,4),'05.Chapter05/05.Latex/','OECDUnivariateModelsOneStep','OECD: models for univariate time series - RMSE per country - 4 one-step forecasts','oecdunimodelsf1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_uvm_final1),'05.Chapter05/05.Latex/','OECDUnivariateModelsOneStepMean','OECD: models for univariate time series - global results -  4 one-step forecasts','oecdunimodelsf1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_uvm_final4,4),'05.Chapter05/05.Latex/','OECDUnivariateModels4Steps','OECD: models for univariate time series - RMSE per country - 4-steps forecast','oecdunimodelsf4',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_uvm_final4),'05.Chapter05/05.Latex/','OECDUnivariateModels4StepsMean','OECD: models for univariate time series - global results -  4-steps forecast','oecdunimodelsf4mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_uvm_finalT,4),'05.Chapter05/05.Latex/','OECDUnivariateModelsTraining','OECD: models for univariate time series - RMSE per country - training set','oecdunimodelstraining',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_uvm_finalT),'05.Chapter05/05.Latex/','OECDUnivariateModelsTrainingMean','OECD: models for univariate time series - global results -  training set','oecdunimodelstrainingmean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_VAR$RMSE1,4),'05.Chapter05/05.Latex/','OECDVAR1Step','OECD: VAR - 1-step forecast','oecdvarf1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_VAR$RMSE1),'05.Chapter05/05.Latex/','OECDVAR1StepMean','OECD: VAR mean - 1-step forecast','oecdvarf1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_VAR$RMSEH,4),'05.Chapter05/05.Latex/','OECDVAR4Steps','OECD: VAR - 4-steps forecast','oecdvarf4',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_VAR$RMSEH),'05.Chapter05/05.Latex/','OECDVAR4StepsMean','OECD: VAR mean - 4-steps forecast','oecdvarf4mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_VAR$AIC,4),'05.Chapter05/05.Latex/','OECDVARaic','OECD: VAR - AIC','oecdvaraic',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(round(resultsOECD_MTS_ML_1_RF,4),'05.Chapter05/05.Latex/','OECDMTSRF1Step','OECD: mts RF - 1-step forecast','oecdmtsrff1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_MTS_ML_1_RF),'05.Chapter05/05.Latex/','OECDMTSRF1StepMean','OECD: mts RF mean - 1-step forecast','oecdmtsrff1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_MTS_ML_r_RF,4),'05.Chapter05/05.Latex/','OECDMTSRF4Steps','OECD: mts RF - 4-steps forecast','oecdmtsrff4',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_MTS_ML_r_RF),'05.Chapter05/05.Latex/','OECDMTSRF4StepsMean','OECD: mts RF mean - 4-steps forecast','oecdmtsrff4mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_MTS_ML_1_ANN,4),'05.Chapter05/05.Latex/','OECDMTSANN1Step','OECD: mts ANN - 1-step forecast','oecdmtsannf1',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_MTS_ML_1_ANN),'05.Chapter05/05.Latex/','OECDMTSANN1StepMean','OECD: mts ANN mean - 1-step forecast','oecdmtsannf1mean',rowname='',colour=TRUE)
DF_to_Latex(round(resultsOECD_MTS_ML_r_ANN,4),'05.Chapter05/05.Latex/','OECDMTSANN4Steps','OECD: mts ANN - 4-steps forecast','oecdmtsannf4',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(resultsOECD_MTS_ML_r_ANN),'05.Chapter05/05.Latex/','OECDMTSANN4StepsMean','OECD: mts ANN mean - 4-steps forecast','oecdmtsannf4mean',rowname='',colour=TRUE)
DF_to_Latex(round(OECDfinalScores1,4),'05.Chapter05/05.Latex/','OECDModels1StepALL','OECD: models for univariate and multivariate time series - RMSE per country - 1 one-step forecasts','oecdmodelsf1all',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(OECDfinalScores1),'05.Chapter05/05.Latex/','OECDModels1StepMeanALL','OECD: models for univariate and multivariate time series - global results -  1 one-step forecasts','oecdmodelsf1allmean',rowname='',colour=TRUE)
DF_to_Latex(round(OECDfinalScores4,4),'05.Chapter05/05.Latex/','OECDModels4StepsALL','OECD: models for univariate and multivariate time series - RMSE per country - 4 one-step forecasts','oecdmodelsf4all',rowname='COUNTRY',colour=TRUE)
DF_to_Latex(mScores_and_rank(OECDfinalScores4),'05.Chapter05/05.Latex/','OECDModels4StepsMeanALL','OECD: models for univariate and multivariate time series - global results -  4 one-step forecasts','oecdmodelsf4allmean',rowname='',colour=TRUE)

#oecd dataset - plot best models
plot_best_models(OECDfinalScores1,'OECD','best models for 1-step forecasts',c(-160,35),c(15, 75))
plot_best_models(OECDfinalScores1[,c('RF5d1','VAR(1)')],'OECD','best models for 1-step forecasts - RF5d1 vs VAR',c(-160,35),c(15, 75))
plot_best_models2(OECDfinalScores1,'OECD','best models for 1-step forecasts - classic vs ML',c(-160,35),c(15, 75))
plot_best_models2(OECDfinalScores1[,c('SARIMA','RF5d1')],'OECD','best models for 1-step forecasts - SARIMA vs RF5d1',c(-160,35),c(15, 75))
plot_best_models(OECDfinalScores4,'OECD','best models for 4 recursive forecasts',c(-160,35),c(15, 75))
plot_best_models(OECDfinalScores4[,c('SARIMA','ARFIMA')],'OECD','best models for 4 recursive forecasts - SARIMA vs ARFIMA',c(-160,35),c(15, 75))
plot_best_models(OECDfinalScores4[c('SARIMA','VAR(1)')],'OECD','best models for 4 recursive forecasts - SARIMA vs VAR',c(-160,35),c(15, 75))
plot_best_models(OECDfinalScores4[,c('RF5d1','mRF5d1')],'OECD','best models for 4 recursive forecasts - RF vs mRF',c(-160,35),c(15, 75))
plot_best_models(OECDfinalScores4[,c('ANN2d1','mANN3d0')],'OECD','best models for 4 recursive forecasts - ANN vs mANN',c(-160,35),c(15, 75))
plot_best_models2(OECDfinalScores4,'OECD','best models for 4-steps forecasts - classic vs ML',c(-160,35),c(15, 75))
plot_best_models2(OECDfinalScores4[,c('SARIMA','RF5d1')],'OECD','best models for 4-steps forecasts - SARIMA vs RF5d1',c(-160,35),c(15, 75))
plot_best_models2(OECDfinalScores4[,c('VAR(1)','mRF5d1')],'OECD','best models for 4-steps forecasts - VAR(1) vs mRF5d1',c(-160,35),c(15, 75))
plot_best_models2(OECDfinalScores4[,c('VAR(1)','mANN3d0')],'OECD','best models for 4-steps forecasts - VAR(1) vs mANN3d0',c(-160,35),c(15, 75))













mean_dif_trainEU<-mean_dif(finalEurostat_DF,c(2000,01),c(2018,12))
mean_dif_testEU<-mean_dif(finalEurostat_DF,c(2018,12),c(2019,12))
mean_dif_testtrainEU<-scores_ratio(mean_dif_testEU,mean_dif_trainEU,row.names(resultsEU_uvm$Scores1),'dif_test_train')
variationEU<-merge_by_rowsnames(list(mean_dif_trainEU,mean_dif_testEU,mean_dif_testtrainEU))
names(variationEU)<-c('training set', 'test set', '$\\frac{\\text{test set}}{\\text{train set}}$')
DF_to_Latex(round(variationEU,3),'06.Chapter06/06.Latex/','MeanVariationEU','EU: mean absolute monthly variation','eumeanvariation',rowname='COUNTRY')

mean_dif_trainOECD<-mean_dif(finalOECD_DF,c(2003,01),c(2018,12))
mean_dif_testOECD<-mean_dif(finalOECD_DF,c(2018,12),c(2019,12))
mean_dif_testtrainOECD<-scores_ratio(mean_dif_testOECD,mean_dif_trainOECD,row.names(resultsOECD_uvm$Scores1),'dif_test_train')
variationOECD<-merge_by_rowsnames(list(mean_dif_trainOECD,mean_dif_testOECD,mean_dif_testtrainOECD))
names(variationOECD)<-c('training set', 'test set', '$\\frac{\\text{test set}}{\\text{train set}}$')
DF_to_Latex(round(variationOECD,3),'06.Chapter06/06.Latex/','MeanVariationOECD','OECD: mean absolute quarterly variation','oecdmeanvariation',rowname='COUNTRY')


nn <- number_neighbours(neigh_list,finalEurostat_DF)
arfima_var_EU1<-scores_ratio(EUfinalScores1$`VAR(2)`, EUfinalScores1$ARFIMA, row.names(resultsEU_uvm$Scores1),'ARFIMA_VAR1')
sarima_var_EU1<-scores_ratio(EUfinalScores1$`VAR(2)`, EUfinalScores1$SARIMA, row.names(resultsEU_uvm$Scores1),'SARIMA_VAR1')
rf_rfm_EU12<-scores_ratio( EUfinalScores12$mRF6d1,EUfinalScores12$RF3d1,row.names(resultsEU_uvm$Scores1),'RF12')
arfima_var_EU12<-scores_ratio( EUfinalScores12$`VAR(2)`,EUfinalScores12$ARFIMA,row.names(resultsEU_uvm$Scores1),'ARFIMA_VAR12')
sarima_var_EU12<-scores_ratio( EUfinalScores12$`VAR(2)`,EUfinalScores12$SARIMA,row.names(resultsEU_uvm$Scores1),'SARIMA_VAR12')
ann_mann_EU12<-scores_ratio( EUfinalScores12$ANN3d1,EUfinalScores12$mANN2d0,row.names(resultsEU_uvm$Scores1),'ANN12')
TS<-DF_to_TS(finalEurostat_DF)
ndif<-data.frame(ndiffs=apply(TS,2,forecast::ndiffs))
high_pacf<-n_high_pacf(finalEurostat_DF,filter_test_m)
c3<-cor(cbind(mean_dif_trainEU,mean_dif_testEU,mean_dif_testtrainEU,ndif,high_pacf,arfima_var_EU1,sarima_var_EU1,arfima_var_EU12,sarima_var_EU12,ann_mann_EU12,rf_rfm_EU12))


arfima_rf_EU1<-scores_ratio(EUfinalScores1$RF3d1, EUfinalScores1$ARFIMA, row.names(resultsEU_uvm$Scores1),'ARFIMA_RF1')
sarima_rf_EU1<-scores_ratio(EUfinalScores1$RF3d1, EUfinalScores1$SARIMA, row.names(resultsEU_uvm$Scores1),'SARIMA_RF1')
arfima_rf_EU12<-scores_ratio(EUfinalScores12$RF3d1, EUfinalScores12$ARFIMA, row.names(resultsEU_uvm$Scores1),'ARFIMA_RF12')
sarima_rf_EU12<-scores_ratio(EUfinalScores12$RF3d1, EUfinalScores12$SARIMA, row.names(resultsEU_uvm$Scores1),'SARIMA_RF12')
c4<-cor(cbind(mean_dif_trainEU,mean_dif_testEU,mean_dif_testtrainEU,ndif,high_pacf,sarima_rf_EU1,arfima_rf_EU1,sarima_rf_EU12,arfima_rf_EU12))


var_rfm_EU12<-scores_ratio( EUfinalScores12$mRF6d1,EUfinalScores12$`VAR(2)`,row.names(resultsEU_uvm$Scores1),'VAR_RF12')
c6<-cor(cbind(nn,mean_dif_trainEU,mean_dif_testEU,mean_dif_testtrainEU,ndif,high_pacf,var_rfm_EU12))



c1<-cor(cbind(nn,arfima_var_EU1,sarima_var_EU1,arfima_var_EU12,sarima_var_EU12,ann_mann_EU12,rf_rfm_EU12))

nn2 <- number_neighbours(neigh_list,finalOECD_DF)
arfima_var_OECD1<-scores_ratio(OECDfinalScores1$`VAR(1)`, OECDfinalScores1$ARFIMA, row.names(resultsOECD_uvm$Scores1),'ARFIMA_VAR1')
sarima_var_OECD1<-scores_ratio(OECDfinalScores1$`VAR(1)`, OECDfinalScores1$SARIMA, row.names(resultsOECD_uvm$Scores1),'SARIMA_VAR1')
rf_rfm_OECD4<-scores_ratio( OECDfinalScores4$mRF5d1,OECDfinalScores4$RF5d1,row.names(resultsOECD_uvm$Scores1),'RF4')
arfima_var_OECD4<-scores_ratio( OECDfinalScores4$`VAR(1)`,OECDfinalScores4$ARFIMA,row.names(resultsOECD_uvm$Scores1),'ARFIMA_VAR4')
sarima_var_OECD4<-scores_ratio( OECDfinalScores4$`VAR(1)`,OECDfinalScores4$SARIMA,row.names(resultsOECD_uvm$Scores1),'SARIMA_VAR4')
ann_mann_OECD4<-scores_ratio( OECDfinalScores4$ANN2d1,OECDfinalScores4$mANN3d0,row.names(resultsOECD_uvm$Scores1),'ANN4')
c2<-cor(cbind(nn2,arfima_var_OECD1,sarima_var_OECD1,arfima_var_OECD4,sarima_var_OECD4,ann_mann_OECD4,rf_rfm_OECD4))


TS2<-DF_to_TS(finalOECD_DF)
ndif2<-data.frame(ndiffs=apply(TS2,2,forecast::ndiffs))
high_pacf2<-n_high_pacf(finalOECD_DF,filter_test_qt)
arfima_rf_OECD1<-scores_ratio(OECDfinalScores1$RF5d1, OECDfinalScores1$ARFIMA, row.names(resultsOECD_uvm$Scores1),'ARFIMA_RF1')
sarima_rf_OECD1<-scores_ratio(OECDfinalScores1$RF5d1, OECDfinalScores1$SARIMA, row.names(resultsOECD_uvm$Scores1),'SARIMA_RF1')
arfima_rf_OECD4<-scores_ratio(OECDfinalScores4$RF5d1, OECDfinalScores4$ARFIMA, row.names(resultsOECD_uvm$Scores1),'ARFIMA_RF4')
sarima_rf_OECD4<-scores_ratio(OECDfinalScores4$RF5d1, OECDfinalScores4$SARIMA, row.names(resultsOECD_uvm$Scores1),'SARIMA_RF4')
c5<-cor(cbind(mean_dif_trainOECD,mean_dif_testOECD,mean_dif_testtrainOECD,ndif2,high_pacf2,sarima_rf_OECD1,arfima_rf_OECD1,sarima_rf_OECD4,arfima_rf_OECD4))

var_rfm_OECD4<-scores_ratio( OECDfinalScores4$mRF5d1,OECDfinalScores4$`VAR(1)`,row.names(resultsOECD_uvm$Scores1),'VAR_RF4')
c7<-cor(cbind(nn2,mean_dif_trainOECD,mean_dif_testOECD,mean_dif_testtrainOECD,ndif2,high_pacf2,var_rfm_OECD4))

#resultsLTLV_VAR<-run_VARmodel(neigh_list,finalEurostat_DF[c('TIME','Sweden','Norno)],filter_test_m)


