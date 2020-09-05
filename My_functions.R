import_libraries<-function(){
  library(dplyr) #for data manipulation
  library(reshape) #for pivot/unpivot data frames
  library(corrplot) #for correlation plot 
  library(ggplot2) #for diverse plots
  library(clusterCrit) #for Davies Bouldin and Dunn indices
  library(dtwclust) #for dtw
  library(sf) #for reading data from natural earth
  library(spdep) #for determining neighbours
  library(zoo) #for time series functions
  library(nnet) #for neural networks
  library(NeuralNetTools) #for plotting a neural network
  library(rpart) #for decision tree
  library(rpart.plot) #for plotting a decision tree
  library(randomForest) #for random forest
  library(forecast) #for classic time series models, ANN/AR, and other time series functions
  library(MTS) #for VAR model
  library(Metrics) #for RMSE
  options(stringsAsFactors = FALSE) #for not defining factors when reading a dataset into R
}





#function to plot and save customized plots as a PNG file
PlotPNG<-function(TS, TStitle, xlabel="Time", ylabel="Value", folder="02.Chapter02/02.Images/", filename=TStitle, TSsub=NULL, xticks=time(TS),whlm=c(400,300,1.1,1.2)){
  
  png(paste0(folder, filename, ".png"), width = whlm[1], height = whlm[2])
  plot(TS, main="", ylab="", xlab="")
  axis(side=1, at=xticks, labels =FALSE)
  
  title(main=TStitle, sub=TSsub, ylab=ylabel, xlab=xlabel, cex.lab=whlm[3], cex.main=whlm[4])
  dev.off()
}

#function to plot a time series with or without respective components, to be saved as PNG files
PlotTSMain <- function(TS, TStitle, xlabel="Time", ylabel="Value", decomp=FALSE, folder="02.Chapter02/02.Images/", filename= TStitle, TSat=time(TS),whlm=c(400,300,1.1,1.2)) {
  PlotPNG(TS, TStitle, xlabel, ylabel, folder,filename=filename,whlm=whlm)
  if (decomp==TRUE){
    compTS<-decompose(TS)
    if(TStitle!=""){
      TStitle<-paste0(TStitle, " - ")
    }
    PlotPNG(compTS$trend, paste0(TStitle, "Trend"), xlabel, ylabel, folder,whlm=whlm)
    PlotPNG(compTS$seasonal, paste0(TStitle, "Seasonality"), xlabel, ylabel, folder,whlm=whlm)
    PlotPNG(compTS$random, paste0(TStitle, "Random"), xlabel, ylabel, folder,whlm=whlm)
  }
}
#customized funtion to plot ACF and/or PACF, to be save as PNG file
PlotAutoCorr<-function(TS, TStitle, ACFvPACF="ACF", folder="02.Chapter02/02.Images/", filename=NULL,whlm=c(400,300,1.1,1.2)){

  if(TStitle!=""){
    TStitle<-paste0(TStitle, " - ")
  }

  if(is.null(filename)){
    filename<-TStitle
  }
  else if(filename!="") {
    filename<-paste0(filename, " - ")
  }

  titleAutoCorr<-paste0(TStitle, ACFvPACF)
  if(ACFvPACF=="ACF"){
    acfTS<-acf(TS)
  }
  if (ACFvPACF=="PACF"){
    acfTS<-pacf(TS)
  }
  acfTS$lag[,1,1]<-acfTS$lag[,1,1]*frequency(TS)

  PlotPNG(acfTS, titleAutoCorr, ylabel=ACFvPACF, xlabel="Lag", folder=folder, filename= paste0(filename,ACFvPACF),whlm=whlm)
  return(acfTS)
}



chapter2<-function(){
  #Plot UKgas time series and respective components
  UKgas_title="UK gas consumption"
  UKgas_ylabel="millions of therms"
  UKgas_xlabel="Time (per Quarter)"
  UKgas_sub="UKgas time series"
  PlotTSMain(UKgas, UKgas_title, UKgas_xlabel, UKgas_ylabel , TRUE)
  
  #Plot WWWusage time series
  WWWusage_title="Internet Usage per Minute"
  WWWusage_ylabel="numbers of users connected"
  WWWusage_xlabel="Time (in minutes)"
  WWWusage_sub="WWWusage time series"
  PlotTSMain(WWWusage, WWWusage_title, WWWusage_xlabel, WWWusage_ylabel)
  
  #Plot ACF and PCAF for WWWusage time series
  PlotAutoCorr(WWWusage,WWWusage_title)
  PlotAutoCorr(WWWusage,WWWusage_title,"PACF")
  
  #Time series for qgdp dataset
  data("mts-examples", package="MTS")
  QGDP<-ts(qgdp[, 3:5], start=c(qgdp[1, 1], qgdp[1, 2]), frequency = 4)
  png("02.Chapter02/02.Images/qgdp.png", width = 400, height = 300)
  ts.plot(QGDP, gpars= list(col=rainbow(3), xlab="Year-Quarter", ylab="GDP", main="Quarterly real gross domestic products"))
  legend("bottomleft", colnames(QGDP), col=rainbow(3), lty = 1)
  dev.off()
  
  #Time series for freeny dataset, with exogenous variables
  par(xpd=TRUE)
  png("02.Chapter02/02.Images/freeny.png", width = 400, height = 300)
  FREENY<-ts(freeny, start = c(1962, 2), frequency = 4)
  ts.plot(FREENY, gpars= list(col=rainbow(5), xlab="Year-Quarter", ylab="Revenue", main="Freeny's Revenue Data", lwd = 1.5, xlim=c(1962, 1972)))
  legend("bottomleft", inset=c(0, 0), colnames(FREENY), col=rainbow(5), lty = 1)
  dev.off()
  
  #Forecasting UKgas and WWWusage 20 steps ahead using the Holt-Winters method and SARIMA, respectively
  H<-20
  UKgas_holtwinters<-HoltWinters(UKgas)
  UKgas_holtwinters.forecastH<-forecast::forecast(UKgas_holtwinters, H)
  UKgas_holtwinters.title<-paste0("UKgas Forecast[", H, "] with HoltWinters")
  UKgas_holtwinters.at<-c(time(UKgas), seq(max(time(UKgas))+.25, max(time(UKgas))+.25*H, by=0.25))
  PlotPNG(UKgas_holtwinters.forecastH, UKgas_holtwinters.title, UKgas_xlabel, UKgas_ylabel, xticks = UKgas_holtwinters.at)
  WWWusage_autoarima<-forecast::auto.arima(WWWusage)
  WWWusage_autoarima_forecastH<-forecast::forecast(WWWusage_autoarima, H)
  WWWusage_autoarima_title<-paste0("WWWusage Forecast[", H, "] with ", WWWusage_autoarima_forecastH$method)
  WWWusage_autoarima_at<-c(time(WWWusage), seq(max(time(WWWusage)+1, max(time(WWWusage)+H))))
  PlotPNG(WWWusage_autoarima_forecastH, WWWusage_autoarima_title, WWWusage_xlabel, WWWusage_ylabel, xticks = WWWusage_autoarima_at)
  
  #Plot neural network for airquality dataset
  NeuralNetTools::plotnet(nnet::nnet(Ozone~Solar.R+Temp+Wind, airquality, size=4))
  #The function PNG does not save this image properly. Export from RStudio instead (for instance)
  
  #Plot neural network for qgdp dataset
  QGDP_lagged<-embed(QGDP, 3)
  Q_names<-paste(c(colnames(QGDP), colnames(QGDP), colnames(QGDP)), c(rep("[t]", 3), rep("[t-1]", 3), rep("[t-2]", 3)))
  Q_names<-paste0(c(colnames(QGDP), colnames(QGDP), colnames(QGDP)), c(rep("_t", 3), rep("_t-1", 3), rep("_t-2", 3)))
  colnames(QGDP_lagged)<-Q_names
  QGDP_DF<-as.data.frame(QGDP_lagged)
  assign("Q_in",QGDP_DF[, -c(1:3)],envir = globalenv())
  assign("Q_out",QGDP_DF[, c(1:3)],envir = globalenv())
  NeuralNetTools::plotnet(nnet::nnet(Q_in, Q_out,size=6))
  #The function PNG does not save this image properly. Export from RStudio instead (for instance)
  rm(ibmspko,Q_in,Q_out,qgdp,tenstocks,envir = globalenv())
  
  
  
  #Plot decision tree for airquality dataset
  png("02.Chapter02/02.Images/airquality-ozone-decisiontree.png", width = 400, height = 300)
  rpart.plot::rpart.plot(rpart::rpart(Ozone~Solar.R+Temp+Wind, airquality))
  dev.off()
}

#function to create  empty labeled lists
empty_lists<-function(list_names){
  x<-vector(mode = "list", length = length(list_names))
  names(x)<-list_names
  return(x)
}

#function to create labeled lists of empty labeled lists
empty_listoflists<-function(mainlist_names,lists_names){
  y<-empty_lists(lists_names)
  x<-rep(list(y), length(mainlist_names))
  names(x)<-mainlist_names
  return(x)
}

#function to create data frames with 0s
empty_DataFrames<-function(n_rows,cols_names,rows_names=1:n_rows){
  x<-as.data.frame(matrix(rep(0,n_rows*length(cols_names)),nrow=n_rows))
  names(x)<-cols_names
  row.names(x)<-rows_names
  return(x)
}

#function to create labeled list with data frames with 0s
empty_listofdataframes<-function(mainlist_names,n_rows,cols_names,rows_names=1:n_rows){
  y<-empty_DataFrames(n_rows,cols_names,rows_names)
  x<-rep(list(y), length(mainlist_names))
  names(x)<-mainlist_names
  return(x)
}



#function to read and preprocess the eurostat data
etl_eurostat<-function(){
  #read eurostat dataset
  eurostat<- read.csv("_Data/Eurostat/une_rt_m_1_Data.csv")
  
  #adjust format of the date and value in eurostat dataset
  eurostat$Value<-as.double(gsub(":",NA,eurostat$Value))
  eurostat$TIME<-zoo::as.yearmon(gsub("M","-",eurostat$TIME))

  #adjust countries names of Germany and United States of America in eurostat dataset
  eurostat$GEO[eurostat$GEO=="Germany (until 1990 former territory of the FRG)"]<-"Germany"
  eurostat$GEO[eurostat$GEO=="United States"]<-"United States of America"
  eurostat<-eurostat[,c('TIME','GEO','Value')]
  names(eurostat)<-c('TIME','COUNTRY','Value')
  return(eurostat)
}

#function to read and preprocess the oecd data
etl_oecd<-function(ne_10m_admin_0_countries){
  #read ODCE dataset
  OECD<- read.csv("_Data/OECD/oecd.csv", fileEncoding = 'UTF-8-BOM')
  #adjust format of the time
  OECD$TIME<-zoo::as.yearqtr(gsub("-"," ",OECD$TIME))
  #look for the names of the countries giving the code
  OECD<-merge(OECD,ne_10m_admin_0_countries,by.x=c("LOCATION"),by.y=c("ADM0_A3"))[c('TIME','ADMIN','Value')]
  names(OECD)<-c('TIME','COUNTRY','Value')
  return(OECD)
}

#function to read and filter the natural earth data
etl_ne_countries<-function(){
  #read natural earth countries dataset
  ne_10m_admin_0_countries<-sf::st_read("_Data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
  return(ne_10m_admin_0_countries[c('ADM0_A3','ADMIN','geometry')])
}

#function to pivot the data with TIME in rows and COUNTRY in columns
pivot_byCountry<-function(DF){
  return(reshape::cast(reshape::melt(DF,id.vars=c("TIME","COUNTRY")),TIME~COUNTRY) %>% select_if(~sum(is.na(.)) == 0))
}
#function to filter a period and/or countries from a data frame
filter_time_countries<-function(DF,filter_start=NULL,filter_end=NULL,countries=NULL){
  if(!is.null(countries)){
    DF<-DF %>% filter(COUNTRY %in% countries)
  }
  if(!is.null(filter_start)){
    DF<-DF%>%filter(TIME>=filter_start[1]+(filter_start[2]-1)/12)
  }
  if(!is.null(filter_end)){
    DF<-DF%>%filter(TIME<=filter_end[1]+(filter_end[2]-1)/12)
  }
  return(DF)
}

#function to pivot the data with TIME in rows and COUNTRY in columns, allowing filters for both variables
pivot_byCountry_filter<-function(DF,filter_start=NULL,filter_end=NULL,countries=NULL){
  DF<-filter_time_countries(DF,filter_start,filter_end,countries)
  return(pivot_byCountry(DF))
}

#function to get the neighbours of each country on the natural earth dataset
get_ne_neighbour_countries<-function(ne_10m_admin_0_countries){
  #create a dataset with country codes and names
  ne_countries<-data.frame(code=ne_10m_admin_0_countries$ADM0_A3,name=ne_10m_admin_0_countries$ADMIN)
  #determine neighbours
  neighbours.list <- spdep::poly2nb(ne_10m_admin_0_countries)
  #transform previous lists to matrix and create a data frame from it
  filtered_list<-list()

  for (l in 1:length(neighbours.list)){
    if(neighbours.list[[l]][1]!=0){
      filtered_list[[ne_countries$name[l]]]<-ne_countries$name[neighbours.list[[l]]]
    }
  }
  filtered_list$Sweden<-append(filtered_list$Sweden,'Denmark')
  filtered_list$Denmark<-append(filtered_list$Denmark,'Sweden')
  return(filtered_list)
}

#function to check if at least one element of x is in y
is_any_in<-function(x,y){
  return(TRUE %in% (x %in% y))
}

#function to filter countries with neighbours within the data frame
filter_with_neighbours<-function(DF,neigh_list){
  namesDF<-names(DF)
  #select relevant countries from neighbours list
  sub_ne_list<-neigh_list[namesDF[namesDF %in% names(neigh_list)] ]
  #run function is_any_in to filter countries that has at least a neighbour within the data frame
  with_ne<-names(sub_ne_list[unlist(lapply(sub_ne_list,is_any_in,namesDF))])
  return(DF[,c("TIME",with_ne)])
}

#function to convert a data frame into a (univariate or multivariate) time series format
DF_to_TS<-function(DF,TS_start = DF[1,1],TS_frequency=1/(DF[2,1]-DF[1,1])){
  return(ts(DF,start=TS_start,frequency =TS_frequency)[,-1])
}

#function to create a list for each country containing its data and the data for the neighbour countries
DF_to_DFS_by_neighbours<-function(neigh_list,DF){
  nms<-names(DF)[-1]
  L<-empty_lists(nms)
  
  #for each country:
  for(i in 2:ncol(DF)){
    #filter out other countries that not itself nor neighbours
    neigh<-nms[nms %in% unlist(c(nms[i-1],neigh_list[nms[i-1]]))]
    L[[i-1]]<-DF[,c("TIME",neigh)]
  }
  return(L)
}

#function to colour a set of countries in the world map with further customized features
plot_map_save<-function(ne_countries,DF_names,my_title,filepath,latLim=c(-90,90),lonLim=c(-180,180),leg_position = "bottom",colour_by=NULL,n_cols=6,my_colours=NULL,my_labels=NULL){
  world.map<-ne_countries[,c("ADMIN","geometry")] #get countries names and polygons
  world.map$isin<-rep(NA,nrow(world.map)) #create new column to be used as filter for colouring
  world.map$isin[world.map$ADMIN %in% DF_names]<-world.map$ADMIN[world.map$ADMIN %in% DF_names] #fill respective column with the set of countries
  
  if(is.null(my_colours)){
    if (is.null(colour_by)){
      #if colour_by is not given, each country will have a different colour, plus white for the remaining countries not included
      my_colours<-c(rainbow(length(DF_names)+1)) 
    }
    else{
      #otherwise a set of colours is defined for each unique value in the column colour_by
      l<-length(unique(colour_by))+1
      my_rb<-c(rainbow(l))
      my_colours<-c(my_rb[colour_by+1],my_rb[1])
    }
  }
  if(is.null(my_labels)){
    my_labels<-DF_names
  }
  
  png(filepath, width = 600, height = 450)
  
  print(ggplot(data = world.map) +
          geom_sf(aes(fill=isin)) +
          coord_sf(ylim = latLim, xlim = lonLim, expand = FALSE)+
          scale_fill_manual(values=my_colours,name="",labels=my_labels)+
          ggtitle(my_title)+
          guides(fill=guide_legend(ncol = n_cols,label.theme = element_text(size = 14)))  + 
          theme(panel.grid.major = element_blank(), legend.position=leg_position, plot.title = element_text(hjust = 0.4,size=15)))
  dev.off()
}


#function to create a table in a tex file with the set of neighbours lists for each country with a data frame
neigh_list_to_latex<-function(neigh_list,DF,dfname){
  L<-DF_to_DFS_by_neighbours(neigh_list,DF)
  namesL<-names(L)
  caption<-paste0(dfname,' - neighbours lists')
  label<-paste0(dfname,'neighbourslists')
  sink(paste0("04.Chapter04/04.Latex/",dfname,"_neighbourslists.tex"))
  cat('\\begin{table}[H]\n')
  cat('\t\\begin{center}\n')
  cat(paste0('\t\\caption{',caption,'}\n'))
  cat(paste0('\t\\label{tbl:',label,'}\n'))
  cat('\t\t\\begin{footnotesize}\n')
  cat('\t\t\t\\begin{tabular}{|l|l|}\\hline')
  for(j in namesL){
    cat(paste0('\n\t\t\t',j,' & '))
    namesj<-paste0(names(L[[j]])[!names(L[[j]]) %in% c(j,"TIME")], collapse=', ')
    cat(paste0(namesj,'\\\\'))
  }
  cat('\\hline\n\t\t\t\\end{tabular}\n')
  cat('\t\t\\end{footnotesize}\n')
  cat('\t\\end{center}\n')
  cat('\\end{table}\n')
  sink()
}

#function to create a table in a tex file given a data frame
DF_to_Latex<-function(DF,folder,filename,caption,label,rowname=NULL,colour=FALSE){
  rnames<-row.names(DF)
  rnames[rnames=='United Kingdom']='UK'
  rnames[rnames=='United States of America']='USA'
  cnames<-names(DF)
  nr<-nrow(DF)
  nc<-ncol(DF)
  if(colour==TRUE){
    cl=round(seq(0.95,0.8,length.out=nc),2)
    M=t(apply(DF,1,rank))
  }
  if(is.null(rowname)){
    newL<-'\t\t\t\t'
    and_<-''
  }
  else{
    newL<-paste0('\t\t\t\t{',rowname,'}')
    and_<-' & '
  }
  sink(paste0(folder,filename,"_latex.tex"))
  cat('\\begin{table}[H]\n')
  cat('\t\\begin{center}\n')
  cat(paste0('\t\\caption{',caption,'}\n'))
  cat(paste0('\t\\label{tbl:',label,'}\n'))
  cat('\t\t\\begin{scriptsize}\n')
  cat('\t\t\t\\begin{tabular}{|')
  if(colour==FALSE){
    cat(rep('l|',nc+1),sep="")
    cat('}\\hline\n')
  }
  else{
    cat(rep('l',nc+1),sep="")
    cat('|}\\hline\n')
  }
  
  
  
  for(v in 1:nc){
    newL<-paste0(newL,and_,' {',cnames[v],'}')
    and_<-' & '
  }
  newL<-paste0(newL,'\\\\\\hline\n')
  cat(newL)
  for(l in 1:nr){
    if(is.null(rowname)){
      newL<-paste('\t\t\t\t')
      and_<-''
    }
    else{
      newL<-paste('\t\t\t\t',rnames[l])
      and_<-' & '  
    }
    for(v in 1:nc){
      if(colour==TRUE){
        newL<-paste0(newL,and_,'\\cellcolor[rgb]{',paste0(rep(cl[M[l,v]],3),collapse=','),'}',DF[l,v])
      }
      else{
        newL<-paste0(newL,' & ',DF[l,v])
      }
      and_<-' & '
    }
    newL<-paste0(newL,'\\\\\\hline\n')
    cat(newL)
  }
  cat('\t\t\t\\end{tabular}\n')
  cat('\t\t\\end{scriptsize}\n')
  cat('\t\\end{center}\n')
  cat('\\end{table}\n')
  sink()
}

#function to:
##plot time series, acf, and pacf for each country
##write a table in a tex file with number of differencing times required to turn a time series stationary and the lags for which PACF is outside the confidence interval
save_ts_acf_pcf_ndiffs<-function(DF,subfolder,filter_test=NULL){
  save_in=paste0("04.Chapter04/04.Images/",subfolder,"/")
  #convert data frame into the time series format
  myTS<-DF_to_TS(DF)
  ts_freq<-frequency(myTS)
  if (is.null(filter_test)==FALSE){
    #filter time series if a end date is given
    myTS<-window(myTS,end=filter_test)
  }
  
  threshold=qnorm(.975)/sqrt(nrow(myTS)) #determine threshold for the 95% CI
  thr=round(threshold,2)
  
  corrDF<-empty_DataFrames(ncol(myTS),c("ndiffs",paste0("abs(PACF)$>$",thr)),colnames(myTS))
  
  l3=paste0('\t\t\\subfigure{\\includegraphics[trim={0.8cm 0.5cm 0.8cm 0.8cm},clip,width=0.32\\linewidth]{"04.Chapter04/04.Images/',subfolder,'/')
  
  
  sink(paste0("04.Chapter04/04.Latex/",subfolder,"_acf_latex.tex"))
  for (i in 1:ncol(myTS)){
    tempTS<-myTS[,i]
    corrDF[i,1]<-forecast::ndiffs(tempTS) #determine the number of differencing times required to turn a time series stationary
    cat("\\begin{figure}[H]\n")
    cat("\t\\centering\n")
    country=colnames(myTS)[i]
    filename1=paste0(country, " - Unemployment rate")
    cat(paste0(l3,filename1,'"}}\n'))
    #plot time series
    PlotTSMain(tempTS,"Unemployment rate", "Time (per month)", "unemployment rate", FALSE,save_in, filename=filename1, whlm=c(340,300,1.3,1.3))
    cat(paste0(l3,country,' - ACF"}}\n'))
    #plot acf
    acfTS<-PlotAutoCorr(tempTS,"","ACF",save_in, filename=country, whlm=c(340,300,1.3,1.3))
    cat(paste0(l3,country,' - PACF"}}\n'))
    #plot pcf
    pacfTS<-PlotAutoCorr(tempTS,"","PACF",save_in, filename=country,whlm=c(340,300,1.3,1.3))
    #select pcf above the threshold
    corrDF[i,2]<-paste(which(abs(pacfTS$acf)>threshold),collapse = ", ")
    to_caption<-paste0(subfolder,' - ',country)
    cat(paste0("\\caption{",to_caption,"}\n"))
    to_tabel=gsub(" ", "",tolower(to_caption))
    cat(paste0("\\label{fig:",to_tabel,"}\n"))
    cat("\\end{figure}\n")
    cat('\\vspace*{-5mm}\n')
  }
  sink()
  file_name<-paste0(subfolder,' - ', 'ndiffs and pacf')
  caption<-paste0(subfolder,' - ', 'number of differences required to make the time series stationary (ndiffs) and lags with PACF above the threshold for 95\\% of confidence')
  DF_to_Latex(corrDF,'04.Chapter04/04.Latex/',file_name,caption,gsub(" ", "",tolower(file_name)),rowname='COUNTRY')
}

#function to determine the cross-correlation matrix, save respective correlation plots and code to add them into a tex file
CCM<-function(DF,subfolder,lags_to_plot,latex_suf='',lags=12){
  myTS<-DF_to_TS(DF)
  n<-ncol(myTS)
  #determine ccm
  R<-MTS::ccm(myTS,lags=lags,level = TRUE,output =F)$ccm
  save_in=paste0("04.Chapter04/04.Images/",subfolder,"/")
  l3=paste0('\t\t\\includegraphics[trim={0.5cm 2.4cm 0.2cm 1.2cm},clip,width=0.67\\linewidth]{"04.Chapter04/04.Images/',subfolder,'/')
  sink(paste0("04.Chapter04/04.Latex/",subfolder,latex_suf,"CCM_latex.tex"))
  for (i in 1:(lags+1)){
    
    if((i-1)%in%lags_to_plot){
      tempDF<-matrix(R[,i],nrow=n,dimnames = list(colnames(myTS),colnames(myTS)))
      cat("\\begin{figure}[H]\n")
      cat("\t\\centering\n")
      
      file_name<-paste0("CCM lag ",i-1)
      cat(paste0(l3,file_name,'"}\n'))
      png(paste0(save_in,file_name,".png"))
      #create correlation plot
      corrplot::corrplot(tempDF, type = "lower", tl.srt = 45,diag=FALSE)
      dev.off()
      to_caption<-paste0(subfolder,' - ',file_name)
      cat(paste0("\\caption{",to_caption,"}\n"))
      to_tabel=gsub(" ", "",tolower(to_caption))
      cat(paste0("\\label{fig:",to_tabel,"}\n"))
      cat("\\end{figure}\n")
    }
  }
  sink()
  
}



#function determine and save figure with the Davies-Bouldin and Dunn indices for kmeans
eval_kmeans<-function(DF,subfolder,maxk=10){
  tDF<-t(DF[,-1])
  db<-c(0,2:10)
  d<-c(0,2:10)
  DB<-empty_DataFrames(2*(maxk-1),c("k","INDEX","value"),seq(1,2*(maxk-1),1))
  DB$k<-c(2:maxk,2:maxk)
  for (k in 1:(maxk-1)){
    set.seed(2020)
    km<-kmeans(tDF,k+1)
    DB$INDEX[k]<-"Davies-Bouldin"
    DB$value[k]<-clusterCrit::intCriteria(tDF,km$cluster,c("Davies_Bouldin"))$davies_bouldin
    DB$INDEX[k+maxk-1]<-"Dunn"
    DB$value[k+maxk-1]<-clusterCrit::intCriteria(tDF,km$cluster,c("Dunn"))$dunn
  }
  png(paste0("04.Chapter04/04.Images/",subfolder,"/kmeans_eval.png"), width = 400, height = 200)
  print(ggplot(data=DB, aes(x=k, y=value,colour=INDEX)) +  geom_line()+  geom_point()+ theme(text = element_text(size=15)))
  dev.off()
}

#function to run kmeans with euclidean distances and return clusters indices
get_clusters<-function(DF,k){
  tDF<-t(DF[,-1])
  set.seed(2020)
  km<-kmeans(tDF,k)
  return(km$cluster)
}  

#function run kmeans with dynamic time warping and return clusters indices 
kmeans_DTW<-function(DF,filter_time_countries,k){
  myTS<-window(DF_to_TS(DF),end=filter_time_countries)
  N<-ncol(myTS)
  DTW<-matrix(rep(0,N*N),ncol=N)
  for (i in 1:(N-1)){
    for (j in (i+1):N){
      d<-dtwclust::dtw_basic(myTS[,i],myTS[,j]) #determine dtw
      DTW[i,j]<-d
      DTW[j,i]<-d
    }
  }
  CLUSTERS<-kmeans(t(myTS),k)$cluster
  return(CLUSTERS)
}



#function to add units of time to a date
add_to_date<-function(my_date,to_add,ts_frequency){
  if(length(my_date)==2){
    s=my_date[2]+to_add
    m=s%%ts_frequency
    if(m==0){
      m=ts_frequency
      y=my_date[1]+s%/%(ts_frequency)-1
    }
    else{
      y=my_date[1]+s%/%(ts_frequency)
    }
    return(c(y,m))
  }
  else{
    return(my_date+to_add/ts_frequency)
  }
}


#function to break a (univariate or multivariate) time series into train and test sets, with possibility of creating lagged variables
break_series<-function(myTS,filter_test=NULL,lags=0){
  if(lags==0){
    if(is.null(filter_test)){
      return(myTS)
    }
    else{
      myTS_train<-window(myTS,end=filter_test)
      myTS_test<-window(myTS,start=add_to_date(filter_test,1,frequency(myTS)),end=end(myTS))
      return(list(train=myTS_train,test=myTS_test,all=myTS))
    }

  }
  else{
    laggedTS<-data.frame(embed(myTS,lags+1)) #create lagged variables
    names(laggedTS)<-c('CurrentUT',paste0('at',1:lags,'UTs_ago')) #name currentUT as the column with target for the forecast
    if(is.null(filter_test)){
      return(list(train=laggedTS))
    }
    else{
      n_test<-length(window(myTS,start=add_to_date(filter_test,1,frequency(myTS))))
      N=nrow(laggedTS)
      n_train=N-n_test
      trainDF<-laggedTS[1:n_train,]
      testDF<-laggedTS[(n_train+1):N,]
      return(list(train=trainDF,test=testDF,all=laggedTS))      
    }
  }
}

#function to create a data frame with lagged variables for multiple countries, having a country as target for the forecast
mts_to_laggedDF<-function(myMTS,lags,filter_test=NULL){
  cts<-ncol(myMTS)
  laggedTS<-data.frame(embed(myMTS,lags+1))[-c(2:cts)]
  names(laggedTS)<-c('CurrentUT',paste0(gsub(" ","",colnames(myMTS)),rep(1:lags,each=cts),'UTs_ago'))
  if(is.null(filter_test)){
    return(list(train=laggedTS))
  }
  else{
    n_test<-nrow(window(myMTS,start=add_to_date(filter_test,1,frequency(myMTS)),end=end(myMTS)))
    N=nrow(laggedTS)
    n_train=N-n_test
    trainDF<-laggedTS[1:n_train,]
    testDF<-laggedTS[(n_train+1):N,]
    return(list(train=trainDF,test=testDF,all=laggedTS))      
  }
}

#function to calculate calculate the mean of the elements in a list
mean_from_list<-function(my_list){
  x<-as.data.frame(sapply(my_list,"[",))
  x<-dplyr::mutate_all(x, function(y) unlist(as.numeric(as.character(y))))
  return(apply(x,2,mean))
}

#function to calculate the mean RMSE, median RMSE and mean rank of a data frame with scores
mScores_and_rank<-function(Scores){
  scoresDF<-data.frame(rbind(round(apply(Scores,2,mean),4),round(apply(Scores,2,median),4),round(apply(apply(Scores,1,rank),1,mean),2)),row.names=c('Mean RMSE','Median RMSE','Mean Rank'))
  names(scoresDF)<-names(Scores)
  return(scoresDF)
}


#function to merge a list of data frames by the row names
merge_by_rowsnames<-function(dfs_list){
  n<-length(dfs_list)
  mdf<-merge(dfs_list[[1]],dfs_list[[2]],by="row.names")
  rownames(mdf) <- mdf$Row.names
  mdf<-mdf[,-1]
  ct<-2
  while(ct<n){
    ct<-ct+1
    mdf<-merge(mdf,dfs_list[[ct]],by="row.names")
    rownames(mdf) <- mdf$Row.names
    mdf<-mdf[,-1]
  }
  return(mdf)
}

#function to predict h one-step forecasts (for arima, arfima, or nnetar models)
h_one_step_forecasts<-function(my_model,my_data,h){
  #in the forecast package, the one-step forecasts can be determined by fitting the trained model the entire dataset
  if(is.ts(my_data)){
    n=length(my_data)
    t=n-h+1
    if(forecast::is.Arima(my_model)){
      adj_model<-forecast::Arima(my_data,model=my_model)
    }
    else if(forecast::is.nnetar(my_model)){
      adj_model<-forecast::nnetar(my_data,model=my_model)
    }
    else if(class(my_model)=='fracdiff'){
      adj_model<-forecast::arfima(my_data,model=my_model)
    }
    return(list(predictions=adj_model$fitted[t:n],actual=my_data[t:n]))
  }
}

#function to determine h recursive one-step forecasts (i.e., the 2nd or further forecasts use the previously determined forecasts)
recursive_h_one_step_forecast<-function(my_model,my_data,h){
  #for the machine learning algorithms using data frames, a forecast is made at a time, and added to the lagged variables for the next date
  if(is.data.frame(my_data)){
    n=nrow(my_data)
    t=n-h+1
    l=ncol(my_data)
    pred_data<-my_data[t:n,]
    pred_data[1,1]<-predict(my_model,pred_data[1,])
    for(i in 2:h){
      pred_data[i,2:l]<-pred_data[i-1,1:(l-1)]
      pred_data[i,1]<-predict(my_model,pred_data[i,])
    }
    return(pred_data)
  }
  #for the functions in the forecast package, the function forecast can have h as the horitzon to predict
  else if(is.ts(my_data)){
    return(forecast::forecast(my_model,h)$mean)
  }
}


custom_arfima<-function(TS){
  ARFIMA<-forecast::arfima(TS) #find ARFIMA model with default parameters
  temp_AIC<-AIC(ARFIMA) #determine AIC for initial model
  initp<-length(ARFIMA$ar) #p of initial model
  initq<-length(ARFIMA$ma) #q of initial model
  
  for(p in 0:5){
    for(q in 0:5){
      if((p+q)!=0&(p!=initp|q!=initq)){
        temp_ARFIMA<-fracdiff::fracdiff(TS,nar=p,nma=q) #fit new model
        #adopt new model if no errors occur and if AIC is lower
        if((temp_ARFIMA$msg[1]=="ok")&(temp_ARFIMA$msg[2]=="ok")&temp_AIC>AIC(temp_ARFIMA)){
          ARFIMA<-temp_ARFIMA
          temp_AIC<-AIC(temp_ARFIMA)
        }
      }
    }
  }
  return(forecast::arfima(TS,model=ARFIMA)) #set the format model as in the forecast package
}



#function to run models arima, arfima and nnetar
run_Rforecast_models<-function(DF,filter_test){
  #input:
  #### DF: data frame with first column with the time, and other columns with data for each country
  #### filter_test: expects c(year,month or quarter)
  
  myTS<-DF_to_TS(DF) #convert data frame to time series
  ts_freq<-frequency(myTS) #determine frequency of the time series
  nc<-ncol(myTS) #number of columns (countries)
  nms<-names(DF)[-1] #countries names
  brk_TS<-break_series(myTS,filter_test) #break time series into train and test sets (each per country)
  trainTS<-brk_TS$train #train sets
  n_train<-nrow(trainTS) #number of records in each train set
  testTS<-brk_TS$test #test set
  n_test<-nrow(testTS) #number of records in each test set
  Models<-empty_listoflists(c("SARIMA","ARFIMA","ANN/AR"),nms) #create empty lists - a list per model, each with all countries to store models
  Models_strings<-empty_DataFrames(length(nms),c("SARIMA","ARFIMA","ANN/AR"),nms) #create data frames with 0s [n countries x 3 models] to store models descriptions
  if(ts_freq==12){ #monthly data
    #create empty lists of data frames with 0s: a list per model, with data frames sized [n months test set x n countries] to store one-step forecasts
    Forecasts1<-empty_listofdataframes(c("SARIMA","ARFIMA","ANN/AR"),n_test,nms,zoo::as.yearmon(zoo::index(testTS))) 
  }
  else{ #quarterly data
    #create empty lists data frames: a list per model, with data frames sized [n quarters test set x n countries] to store one-step forecasts
    Forecasts1<-empty_listofdataframes(c("SARIMA","ARFIMA","ANN/AR"),n_test,nms,zoo::as.yearqtr(zoo::index(testTS)))
  }
  Scores1<-empty_DataFrames(length(nms),c("SARIMA","ARFIMA","ANN/AR"),nms)  #create data frames with 0s [n countries x 3 models] to store RMSE for one-step forecasts scores
  ForecastsH<-Forecasts1 #to store h recursive one-step forecasts
  ScoresH<-Scores1 #to store h recursive one-step forecasts scores
  ScoresTraining<-Scores1 #to store scores for training set
  #for each country
  for (i in 1:nc){
    tempTS<-myTS[,i] #select respective time series
    temp_trainTS<-trainTS[,i] #select respective train set
    temp_testTS<-testTS[,i] #select respective test set
    Models$SARIMA[[i]]<-forecast::auto.arima(temp_trainTS) #build arima model
    set.seed(2020) #fix random seed
    Models$`ANN/AR`[[i]]<-forecast::nnetar(temp_trainTS) #build ANN/AR model
    Models$ARFIMA[[i]]<-custom_arfima(temp_trainTS) #build arfima model
    Models_strings[i,"SARIMA"]<-gsub('ARIMA','',forecast::forecast(Models$SARIMA[[i]])$method) #store arima model description
    Models_strings[i,"ARFIMA"]<-gsub('ARFIMA','',forecast::forecast(Models$ARFIMA[[i]])$method) #store arfima model description
    Models_strings[i,"ANN/AR"]<-gsub('ANN/AR','',forecast::forecast(Models$`ANN/AR`[[i]])$method) #store ann/ar model description
    Forecasts1$SARIMA[,i]<-h_one_step_forecasts(Models$SARIMA[[i]],tempTS,n_test)$predictions #determine one-step forecasts with arima model
    ForecastsH$SARIMA[,i]<-recursive_h_one_step_forecast(Models$SARIMA[[i]],tempTS,n_test) #determine recursive one-step forecasts with arima model
    Forecasts1$ARFIMA[,i]<-h_one_step_forecasts(Models$ARFIMA[[i]],tempTS,n_test)$predictions #determine one-step forecasts with arfima model
    ForecastsH$ARFIMA[,i]<-recursive_h_one_step_forecast(Models$ARFIMA[[i]],tempTS,n_test) #determine recursive one-step forecasts with arfima model
    Forecasts1$`ANN/AR`[,i]<-h_one_step_forecasts(Models$`ANN/AR`[[i]],tempTS,n_test)$predictions #determine one-step forecasts with ann/ar model
    ForecastsH$`ANN/AR`[,i]<-recursive_h_one_step_forecast(Models$`ANN/AR`[[i]],tempTS,n_test) #determine recursive one-step forecasts with ann/ar model
    Scores1[i,"SARIMA"]<-Metrics::rmse(temp_testTS,Forecasts1$SARIMA[,i]) #determine one-step forecasts RMSE with arima model
    ScoresH[i,"SARIMA"]<-Metrics::rmse(temp_testTS,ForecastsH$SARIMA[,i]) #determine recursive one-step RMSE forecasts with arima model
    Scores1[i,"ARFIMA"]<-Metrics::rmse(temp_testTS,Forecasts1$ARFIMA[,i]) #determine one-step forecasts RMSE with arfima model
    ScoresH[i,"ARFIMA"]<-Metrics::rmse(temp_testTS,ForecastsH$ARFIMA[,i]) #determine recursive one-step forecasts RMSE with arfima model
    Scores1[i,"ANN/AR"]<-Metrics::rmse(temp_testTS,Forecasts1$`ANN/AR`[,i]) #determine one-step forecasts RMSE with ann/ar model
    ScoresH[i,"ANN/AR"]<-Metrics::rmse(temp_testTS,ForecastsH$`ANN/AR`[,i]) #determine recursive one-step forecasts RMSE with ann/ar model
    
    #the fitted values can be used to calculate the RMSE for the training set
    #for arfima and arima, the fitted values for the first observations from backcasting (i.e., estimate values for observations earlier than the first element of the training set)
    #for ann/ar this is not applicable, and the first records needed to start the model have the fitted values as null
    ScoresTraining[i,"SARIMA"]<-Metrics::rmse(Models$SARIMA[[i]]$x,Models$SARIMA[[i]]$fitted) #determine training set RMSE with arima model
    w<-is.na(Models$"ARFIMA"[[i]]$fitted) #for the rare cases were an NA is produced
    ScoresTraining[i,"ARFIMA"]<-Metrics::rmse(Models$ARFIMA[[i]]$x[!w],Models$ARFIMA[[i]]$fitted[!w]) #determine training set RMSE with arfima model
    #determine training set RMSE with ann/ar model, excluding first records with NA
    w<-is.na(Models$`ANN/AR`[[i]]$fitted)
    ScoresTraining[i,"ANN/AR"]<-Metrics::rmse(Models$`ANN/AR`[[i]]$x[!w],Models$`ANN/AR`[[i]]$fitted[!w])
    
  }
  return(list(Models=Models,Models_strings=Models_strings,Forecasts1=Forecasts1,ForecastsH=ForecastsH,Scores1=Scores1,mScores1=mean_from_list(Scores1),ScoresH=ScoresH,mScoresH=mean_from_list(ScoresH),ScoresT=ScoresTraining,mScoresT=mean_from_list(ScoresTraining)))
 
}

#function to run Random forest and neural networks using lagged variables
run_ML_uvm_models<-function(DF,filter_test,maxlags=12,NDIF=0){
  #input:
    #DF: data frame with first column with the time, and other columns with data for each country
    #filter_test: expects c(year,month or quarter)
    #max_lags: number of lagged variables
    #NDIF: number of times to apply differencing - prepared for 0 or 1
  
  RFname<-paste0("RF",maxlags,'d',NDIF) #name random forest model according to number of lagged varaibles and number of times to apply differencing
  ANNname<-paste0("ANN",maxlags,'d',NDIF) #name ann model according to number of lagged varaibles and number of times to apply differencing
  myTS<-DF_to_TS(DF) #convert data frame to time series
  ts_freq<-frequency(myTS) #determine frequency of the time series
  nc<-ncol(myTS) #number of columns (countries)
  nms<-names(DF)[-1] #countries names
  brk_TS<-break_series(myTS,filter_test) #break time series into train and test sets (each per country)
  trainTS<-brk_TS$train #train sets
  n_train<-nrow(trainTS) #number of records in each train set
  testTS<-brk_TS$test #test set
  n_test<-nrow(testTS) #number of records in each test set
  Models<-empty_listoflists(c(RFname,ANNname),nms) #create empty lists - a list per model, each with all countries to store models
  if(ts_freq==12){ #monthly data
    #create empty lists of data frames with 0s: a list per model, with data frames sized [n months test set x n countries] to store one-step forecasts
    Forecasts1<-empty_listofdataframes(c(RFname,ANNname),n_test,nms,zoo::as.yearmon(zoo::index(testTS)))
  }
  else{ #quarterly data
    #create empty lists data frames: a list per model, with data frames sized [n quarters test set x n countries] to store one-step forecasts
    Forecasts1<-empty_listofdataframes(c(RFname,ANNname),n_test,nms,zoo::as.yearqtr(zoo::index(testTS)))
  }
  Scores1<-empty_DataFrames(length(nms),c(RFname,ANNname),nms)#create data frames with 0s [n countries x 2 models] to store RMSE for one-step forecasts scores
  ForecastsH<-Forecasts1 #to store h recursive one-step forecasts
  ScoresH<-Scores1 #to store h recursive one-step forecasts scores
  ScoresTraining<-Scores1 #to store scores for training set
  
  #for each country  
  for (i in 1:nc){
    tempTS<-myTS[,i] #select respective time series
    temp_trainTS<-trainTS[,i] #select respective train set
    temp_testTS<-testTS[,i] #select respective test set    

    to_dif<-c()
    to_dif_train<-c()
    difTS<-tempTS
    #apply differencing (this could be used more applying differencing more than once)
    while(length(to_dif)<NDIF){
      #store inital value of the data frame (excluding intial rows used only as lagged variables) so that the original data can be reconstructed
      to_dif_train<-append(to_dif_train,difTS[maxlags+1])
      #store last value of training set so that the original data of test set can ben reconstructed
      to_dif<-append(to_dif,difTS[n_train-1])
      #applying differencing
      difTS<-diff(difTS)
    }
    #set number of columns - current and past observarions according to the number of lagged variables
    filter_cols<-c(1:(maxlags+1))
    #break time series after differencing (if applicable) and create data frame with lagged variables
    temp_brk_DF<-break_series(difTS,filter_test,max(filter_cols)-1) 
    
    set.seed(2020) #fix random seed
    #build random forest
    Models[[RFname]][[i]]<-randomForest::randomForest(CurrentUT~.,temp_brk_DF$train[,filter_cols])
    set.seed(2020) #fix random seed
    #build ann
    Models[[ANNname]][[i]]<-nnet::nnet(CurrentUT~.,temp_brk_DF$train[,filter_cols],size=15,decay = 5e-4, maxit = 100,linout=TRUE,trace=FALSE)
    
    
    #determine recursive one-step forecasts with random forest
    ForecastsH[[RFname]][,i]<-recursive_h_one_step_forecast(Models[[RFname]][[i]],temp_brk_DF$all,n_test)$CurrentUT
    #determine recursive one-step forecasts with ann
    ForecastsH[[ANNname]][,i]<-recursive_h_one_step_forecast(Models[[ANNname]][[i]],temp_brk_DF$all,n_test)$CurrentUT
    
    #make preditions for training set with random forest
    predTrainRF<-predict(Models[[RFname]][[i]],temp_brk_DF$train)
    #make preditions for training set with ann
    predTrainANN<-predict(Models[[ANNname]][[i]],temp_brk_DF$train)
    tr<-temp_brk_DF$train[,1] #collect training set with differencing
    
    #adjust recursive one-step forecasts and predictions to original data for applying cumulative sums
    if(NDIF==1){
      #original first value of training set + cumulative sum of training set with differencing
      tr<-cumsum(c(to_dif_train[1],tr))
      #add predictions (difference) to training set values
      predTrainRF<-predTrainRF+tr[-length(tr)]
      predTrainANN<-predTrainANN+tr[-length(tr)]
      #original last value of training set + cumulative sum of the recursive one-step forecasts
      ForecastsH[[RFname]][,i]<-cumsum(c(to_dif[1],ForecastsH[[RFname]][,i]))[-1]
      ForecastsH[[ANNname]][,i]<-cumsum(c(to_dif[1],ForecastsH[[ANNname]][,i]))[-1]
      tr<-tr[-1]
    }

    
    #calculate RMSE for training set
    ScoresTraining[i,RFname]<-Metrics::rmse(predTrainRF,tr)
    ScoresTraining[i,ANNname]<-Metrics::rmse(predTrainANN,tr)
  
    #calculate RMSE for recursive one-step forecasts
    ScoresH[i,RFname]<-Metrics::rmse(temp_testTS,ForecastsH[[RFname]][,i])
    ScoresH[i,ANNname]<-Metrics::rmse(temp_testTS,ForecastsH[[ANNname]][,i])
    
    #determine on-step forecasts
    #for cycle to train model so that the prediction for each date of th test set can be done on a model trained on all previous data
    newN<-nrow(temp_brk_DF$all)
    for(j in 1:n_test){
      k<-newN-(n_test-j)-1 #last row of training set
      set.seed(2020) #fix random seed
      #build random forest
      tempRF<-randomForest::randomForest(CurrentUT~.,temp_brk_DF$all[1:k,filter_cols])
      set.seed(2020) #fix random seed
      #build ann
      tempANN<-nnet::nnet(CurrentUT~.,temp_brk_DF$all[1:k,filter_cols],size=15,decay = 5e-4, maxit = 100,linout=TRUE,trace=FALSE)
      #store forecasts
      Forecasts1[[RFname]][j,i]<-predict(tempRF,temp_brk_DF$test[j,])
      Forecasts1[[ANNname]][j,i]<-predict(tempANN,temp_brk_DF$test[j,])
      
      #adjust forecast to original data if differencing was applied
      if(NDIF==1){
        v1=DF[n_train+j-1,i+1]
        Forecasts1[[RFname]][j,i]<-Forecasts1[[RFname]][j,i]+v1
        Forecasts1[[ANNname]][j,i]<-Forecasts1[[ANNname]][j,i]+v1
      }
      
      #calculate RMSE for one-step forcasts
      Scores1[i,RFname]<-Metrics::rmse(temp_testTS,Forecasts1[[RFname]][,i])
      Scores1[i,ANNname]<-Metrics::rmse(temp_testTS,Forecasts1[[ANNname]][,i])
    }

  }
  return(list(Models=Models,Forecasts1=Forecasts1,ForecastsH=ForecastsH,Scores1=Scores1,mScores1=mean_from_list(Scores1),ScoresH=ScoresH,mScoresH=mean_from_list(ScoresH),ScoresT=ScoresTraining,mScoresT=mean_from_list(ScoresTraining)))
}

#function to run VAR(1), VAR(2), and VAR(3)
run_VARmodel<-function(neigh_list,DF,filter_test){
  #input:
    #neigh_list: list with neighours countries
    #DF: data frame with first column with the time, and other columns with data for each country
    #filter_test: expects c(year,month or quarter)
  L<-DF_to_DFS_by_neighbours(neigh_list,DF) #create subset of datasets for each country
  namesL<-names(L)
  n_test<-nrow(break_series(DF_to_TS(DF),filter_test)$test) #determine horizon for forecast
  L2<-empty_lists(namesL) #list to store predictions
  rmseH<-empty_DataFrames(length(L),c('VAR(1)','VAR(2)','VAR(3)'),namesL) #to store RMSE for the recursive forecasts
  VARs_AIC<-empty_DataFrames(length(L),c('VAR(1)','VAR(2)','VAR(3)'),namesL) #to store AIC of the models
  PredOneStep<-empty_listofdataframes(c('VAR(1)','VAR(2)','VAR(3)'),n_test,namesL) #to store predictions of single 1-step forecasts
  rmse1<-empty_DataFrames(length(L),c('VAR(1)','VAR(2)','VAR(3)'),namesL) #to store rmse given the single 1-step forecasts
  for(l in 1:length(L)){
    if(ncol(L[[l]])>2){
      target<-namesL[l] #name of the country
      myTS<-DF_to_TS(L[[l]]) #from data frame to time series
      ts_freq<-frequency(myTS) #determine frequency
      brk_TS<-break_series(myTS,filter_test) #split train set from test set
      my_VAR1<-MTS::VAR(brk_TS$train,1,output = F) #VAR(1)
      my_VAR2<-MTS::VAR(brk_TS$train,2,output = F) #VAR(2)
      my_VAR3<-MTS::VAR(brk_TS$train,3,output = F) #VAR(3)
      #determine and store predictions with horizon h
      PredictionsH<-list(VAR1=MTS::VARpred(my_VAR1,h=n_test)$pred[,target],VAR2=MTS::VARpred(my_VAR2,h=n_test)$pred[,target],VAR3=MTS::VARpred(my_VAR3,h=n_test)$pred[,target])
      #determine and store respective values of RMSE
      RMSEsH<-list(VAR1=Metrics::rmse(PredictionsH$VAR1,brk_TS$test[,target]),VAR2=Metrics::rmse(PredictionsH$VAR2,brk_TS$test[,target]),VAR3=Metrics::rmse(PredictionsH$VAR3,brk_TS$test[,target]))
      rmseH[l,1:3]<-unlist(RMSEsH)
      #store AIC for each model
      AICs<-list(VAR1=my_VAR1$aic,VAR2=my_VAR2$aic,VAR3=my_VAR3$aic)
      VARs_AIC[l,1:3]<-unlist(AICs)
      
      #determine all one-step forecasts
      for(i in 1:n_test){
        new_filter<-add_to_date(filter_test,i-1,ts_freq) #define last record of training set
        new_train<-break_series(myTS,new_filter)$train #break dataset in training and test sets
        new_VAR1<-MTS::VAR(new_train,1,output = F) #VAR(1)
        PredOneStep$`VAR(1)`[i,target]<-MTS::VARpred(new_VAR1,h=1)$pred[target] #respective 1-step prediction
        new_VAR2<-MTS::VAR(new_train,2,output = F) #VAR(2)
        PredOneStep$`VAR(2)`[i,target]<-MTS::VARpred(new_VAR2,h=1)$pred[target]
        new_VAR3<-MTS::VAR(new_train,3,output = F) #VAR(3)
        PredOneStep$`VAR(3)`[i,target]<-MTS::VARpred(new_VAR3,h=1)$pred[target]
      }
      #determine and store values of RMSE
      RMSEs1<-list(VAR1=Metrics::rmse(PredOneStep$`VAR(1)`[,target],brk_TS$test[,target]),VAR2=Metrics::rmse(PredOneStep$`VAR(2)`[,target],brk_TS$test[,target]),VAR3=Metrics::rmse(PredOneStep$`VAR(3)`[,target],brk_TS$test[,target]))
      rmse1[l,1:3]<-unlist(RMSEs1)
      #store predictions
      L2[[l]]<-list(PredictionH=PredictionsH,Prediction1=PredOneStep)
      
    }
  }
  return(list(predictions=L2,RMSEH=rmseH,AIC=VARs_AIC,RMSE1=rmse1))
}

#function to run ML models using the neighbours data and make predictions for the test set
run_MTS_ML<-function(neigh_list, DF, filter_test, lags, NDIF=0, models_selection='both', Models=list(), nodes=5){
  #input:
    #neigh_list: list with neighours countries
    #DF: data frame with first column with the time, and other columns with data for each country
    #filter_test: expects c(year,month or quarter)
    #lags: number of previous months/quarters to use as predictors
    #NDIF: number of times to apply differencing
    #models_selection: RF/ANN/both to run the respected selecterd models
    #Models: list of trained models if the aim is only to make new predictions
    #nodes: number of nodes of hidden layer for ANN
  L<-DF_to_DFS_by_neighbours(neigh_list, DF) #create subset of datasets for each country
  namesL<-names(L)
  L2<-empty_lists(namesL) #list to store predictions
  RFname<-paste0("mRF", lags, 'd', NDIF) #name random forest model according to number of lagged varaibles and number of times to apply differencing
  ANNname<-paste0("mANN", lags, 'd', NDIF) #name ann model according to number of lagged varaibles and number of times to apply differencing
  #create empty lists and data frames according to the models selection
  if(length(Models)==0){ 
    Models<-empty_listoflists(c('RF', 'ANN'), namesL)
  }
  if (models_selection=='RF'){
    MTS_ML_RMSE<-empty_DataFrames(length(L), RFname, namesL)
  }
  else if (models_selection=='ANN'){
    MTS_ML_RMSE<-empty_DataFrames(length(L), ANNname, namesL)
  }
  else{
    MTS_ML_RMSE<-empty_DataFrames(length(L), c(RFname, ANNname), namesL)
  }  
  for(l in 1:length(L)){ #for each country
    if(ncol(L[[l]])>2){
      target<-namesL[l] #select country name
      myTS<-DF_to_TS(L[[l]]) #from data frame to time series
      brk_TS<-break_series(myTS, filter_test)
      n_train<-nrow(brk_TS$train)
      n_test<-nrow(brk_TS$test)
      to_dif<-c()
      to_dif_train<-c()
      difTS<-myTS
      #apply differencing (this could be used more applying differencing more than once)
      while(length(to_dif)<NDIF){
        #store inital value of the data frame (excluding intial rows used only as lagged variables) so that the original data can be reconstructed
        to_dif_train<-append(to_dif_train, difTS[lags+1, target])
        #store last value of training set so that the original data of test set can ben reconstructed
        to_dif<-append(to_dif, difTS[n_train-1, target])
        #applying differencing
        difTS<-diff(difTS)
      }
      new_brk_TS<-break_series(difTS, filter_test) #split train and test set
      neigh<-colnames(difTS)[!colnames(difTS) %in% target] #select data of neighbour countries
      newTS<-cbind(difTS[,target], difTS[,neigh]) #select data of own country
      colnames(newTS)<-c(target, neigh)
      temp_brk_DF<-mts_to_laggedDF(newTS, lags, filter_test) #create data frame with lagged variables
      if (models_selection!='ANN'){
        set.seed(2020)
        if(is.null(Models$RF[[l]])){
          #train random forest
          Models$RF[[l]]<-randomForest::randomForest(CurrentUT~., temp_brk_DF$train)
        }
        predRF<-list(RF=predict(Models$RF[[l]], temp_brk_DF$test)) #make predictions for test set
        if(NDIF==1){
          predRF$RF<-cumsum(c(to_dif[1], predRF$RF))[-1] #apply cumulative sum if differencing had been applied
        }
        if(n_test>1){
          rmseRF<-list(RF=Metrics::rmse(predRF$RF, brk_TS$test[,target])) #calculate RMSE
        }
      }
      if (models_selection!='RF'){
        set.seed(2020)
        if(is.null(Models$ANN[[l]])){
          #train ANN
          Models$ANN[[l]]<-nnet::nnet(CurrentUT~., temp_brk_DF$train, size=nodes, decay = 5e-3,  maxit =200, linout=TRUE, trace=FALSE)
        }
        predANN<-list(ANN=predict(Models$ANN[[l]], temp_brk_DF$test))
        if(NDIF==1){
          predANN$ANN<-cumsum(c(to_dif[1], predANN$ANN))[-1] #apply cumulative sum if differencing had been applied
        }
        if(n_test>1){
          rmseANN<-list(ANN=Metrics::rmse(predANN$ANN, brk_TS$test[, target])) #calculate RMSE
        }
      }
      #merge predictions
      if (models_selection=='RF'){
        Predictions<-predRF
        if(n_test>1){
          RMSEs<-rmseRF
        }
      }
      else if (models_selection=='ANN'){
        Predictions<-predANN
        if(n_test>1){
          RMSEs<-rmseANN
        }
      }
      else{
        Predictions<-c(predRF, predANN)
        if(n_test>1){
          RMSEs<-c(rmseRF, rmseANN)
        }
      }
      if(n_test>1){
        MTS_ML_RMSE[l, 1:length(RMSEs)]<-unlist(RMSEs)
      }
      L2[[l]]<-list(Prediction=Predictions)
    }
  }
  if(n_test>1){
    return(list(Models=Models, predictions=L2, RMSE=MTS_ML_RMSE))
  }
  else{
    return(list(Models=Models, predictions=L2))
  }
}

#function to run ML models using the neighbours data and make one-step and recursive predictions for the test set
run_recursive_MTS_ML<-function(neigh_list, DF, filter_test, lags, NDIF=0, models_selection='both', nodes=5){
  #input:
  #neigh_list: list with neighours countries
  #DF: data frame with first column with the time, and other columns with data for each country
  #filter_test: expects c(year,month or quarter)
  #lags: number of previous months/quarters to use as predictors
  #NDIF: number of times to apply differencing
  #models_selection: RF/ANN/both to run the respected selecterd models
  #nodes: number of nodes of hidden layer for ANN
  results<-run_MTS_ML(neigh_list, DF, filter_test, lags, NDIF, models_selection, nodes = nodes) #run ML for one-step forecasts
  myTS<-DF_to_TS(DF) #from data frame to time series
  namesdf<-names(DF[-1])
  ts_freq<-frequency(myTS)
  brk_TS<-break_series(myTS, filter_test)
  n_train=nrow(brk_TS$train)
  n_test=nrow(brk_TS$test)
  dfRF<-DF
  dfANN<-DF
  for(i in 1:n_test){#loop for recursive forecasts
    new_filter<-add_to_date(filter_test, i-1, ts_freq) #update last date of training set
    newN<-n_train+i
    #run RF and make next prediction
    pRF<-run_MTS_ML(neigh_list, dfRF[1:newN,], new_filter, lags, NDIF, 'RF', results$Models)$predictions
    dfRF[newN, names(pRF)]<-unlist(pRF, use.names = FALSE)
    #run ANN and make next prediction
    pANN<-run_MTS_ML(neigh_list, dfANN[1:newN,], new_filter, lags, NDIF, 'ANN', results$Models)$predictions
    dfANN[newN, names(pANN)]<-unlist(pANN, use.names = FALSE)
  }
  mdls<-c() #initiate list of trained models
  Predictions<-list() #initiate list of predictions
  if(models_selection!='ANN'){
    RFname<-paste0("mRF", lags, 'd', NDIF) #name random forest model according to number of lagged varaibles and number of times to apply differencing
    mdls<-c(mdls, RFname)
    Predictions$RF=dfRF[(n_train+1):nrow(DF), -1]
  }
  if(models_selection!='RF'){
    ANNname<-paste0("mANN", lags, 'd', NDIF) #name ann model according to number of lagged varaibles and number of times to apply differencing
    mdls<-c(mdls, ANNname)
    Predictions$ANN=dfANN[(n_train+1):nrow(DF), -1]
  }
  
  RMSEs<-empty_DataFrames(length(namesdf), mdls, namesdf)
  for(k in namesdf){
    if(models_selection!='ANN'){
      RMSEs[k, RFname]<-Metrics::rmse(DF[(n_train+1):nrow(DF), k], dfRF[(n_train+1):nrow(DF), k])
    }
    if(models_selection!='RF'){
      RMSEs[k, ANNname]<-Metrics::rmse(DF[(n_train+1):nrow(DF), k], dfANN[(n_train+1):nrow(DF), k])
    }
  }
  return(list(predictionsH=Predictions, predictions1=results$predictions, RMSEH=RMSEs, RMSE1=results$RMSE))
}


plot_test_set<-function(DF,before_first,last_date,subfolder,xlim,ylim){
  df <- reshape::melt.data.frame(DF,'TIME',variable_name='COUNTRY')
  dfn <-pivot_byCountry_filter(df,before_first,last_date)
  myTS<-diff(DF_to_TS(dfn))
  
  tsdf<-data.frame(ABSMEANDIF=apply(abs(myTS),2,mean))
  countries1<-row.names(tsdf)[tsdf$ABSMEANDIF>=0.1]
  df <-filter_time_countries(df,add_to_date(before_first,1,frequency(myTS)))
  df<- transform(df, TIME = as.Date( format(TIME,"01-%m-%Y"),"%d-%m-%Y"))
  df<- transform(df, HighVariation = COUNTRY %in% countries1)
  filefolder=paste0("05.Chapter05/05.Images/",subfolder,'/')
  png(paste0(filefolder,'differences_testset.png'), width = 700, height =450)
  g<-ggplot(df, aes(x = TIME, y = value)) +  geom_line(aes(color = COUNTRY))+  scale_colour_discrete(guide = 'none') + geom_dl(aes(label = COUNTRY), method = list(dl.combine("first.points","last.points"), cex = 0.8)) +scale_x_date(expand = c(0, 60))+facet_grid(.~HighVariation)
  print(g)
  dev.off()
  pallete<-c('lightblue2','aquamarine','cyan','deepskyblue','dodgerblue','royalblue','blue')
  orderc<-round(rescale(log(tsdf$ABSMEANDIF),to=c(1,7)))
  colours<-pallete[orderc]
  plot_map_save(ne_10m_admin_0_countries,row.names(tsdf), 'Mean absolute variation',
                paste0(filefolder,"meanvariations_testset.png"),ylim,xlim,
                my_colours= colours,n_cols=4,my_labels = paste0(row.names(tsdf),': ',round(tsdf$ABSMEANDIF,2)))
  return(tsdf)
}

#function to plot best models,shades by univariate vs. multivariate
plot_best_models<-function(Scores,subfolder,filename,xlim,ylim){
  pallete<-c()
  for(nms in names(Scores)){
    if(substr(nms, 1, 3)=='mRF'){
      pallete<-append(pallete,rgb(1,0,1))
    }
    else if(substr(nms, 1, 4)=='mANN'){
      pallete<-append(pallete,rgb(.9,0,.85))
    }
    else if(substr(nms, 1, 3)=='VAR'){
      pallete<-append(pallete,rgb(.75,0,.7))
    }
    else if (nms=='SARIMA'){
      pallete<-append(pallete,rgb(0,1,1))
    }
    else if (substr(nms, 1, 4)=='ANN/'){
      pallete<-append(pallete,rgb(0,.9,.9))
    }
    else if (substr(nms, 1, 3)=='ANN'){
      pallete<-append(pallete,rgb(0,.8,.8))
    }
    else if (substr(nms, 1, 2)=='RF'){
      pallete<-append(pallete,rgb(0,.7,.7))
    }
    else if (nms=='ARFIMA'){
      pallete<-append(pallete,rgb(0,.6,.6))
    }
  }
  best_index<-apply(Scores,1,which.min)
  colours<-pallete[apply(Scores,1,which.min)]
  filefolder<-paste0("05.Chapter05/05.Images/",subfolder,'/')
  plot_map_save(ne_10m_admin_0_countries,row.names(Scores), filename,
                paste0(filefolder,filename,".png"),ylim,xlim,
                my_colours= colours,n_cols=3,my_labels = paste0(row.names(Scores),': ',names(Scores)[best_index]))
  
}

#function to plot best models,shades by classic vs. ML
plot_best_models2<-function(Scores,subfolder,filename,xlim,ylim){
  pallete<-c()
  for(nms in names(Scores)){
    if(substr(nms, 1, 3)=='mRF'){
      pallete<-append(pallete,rgb(1,0,0))
    }
    else if(substr(nms, 1, 4)=='mANN'){
      pallete<-append(pallete,rgb(.9,0,0))
    }
    else if(substr(nms, 1, 3)=='VAR'){
      pallete<-append(pallete,rgb(0,0,1))
    }
    else if (nms=='SARIMA'){
      pallete<-append(pallete,rgb(0,0,0.9))
    }
    else if (substr(nms, 1, 4)=='ANN/'){
      pallete<-append(pallete,rgb(1,1,0))
    }
    else if (substr(nms, 1, 3)=='ANN'){
      pallete<-append(pallete,rgb(.7,0,0))
    }
    else if (substr(nms, 1, 2)=='RF'){
      pallete<-append(pallete,rgb(.6,0,0))
    }
    else if (nms=='ARFIMA'){
      pallete<-append(pallete,rgb(0,0,.8))
    }
  }
  best_index<-apply(Scores,1,which.min)
  
  colours<-pallete[apply(Scores,1,which.min)]
  filefolder<-paste0("05.Chapter05/05.Images/",subfolder,'/')
  plot_map_save(ne_10m_admin_0_countries,row.names(Scores), filename,
                paste0(filefolder,filename,".png"),ylim,xlim,
                my_colours= colours,n_cols=3,my_labels = paste0(row.names(Scores),': ',names(Scores)[best_index]))
}

#function to determine the mean absolute variation between each month/quarter
mean_dif<-function(DF,before_first,last_date){
  df <- reshape::melt.data.frame(DF,'TIME',variable_name='COUNTRY')
  dfn <-pivot_byCountry_filter(df,before_first,last_date)
  myTS<-diff(DF_to_TS(dfn))
  
  tsdf<-data.frame(ABSMEANDIF=apply(abs(myTS),2,mean))
  
  return(tsdf)
}

#function to count number of neighbours
number_neighbours<-function(neigh_list,DF){
  LN<-DF_to_DFS_by_neighbours(neigh_list,DF)
  l<-length(LN)
  NN<-empty_DataFrames(l,'nn',names(DF)[-1])
  for(i in 1:l){
    NN[i,1]<-length(LN[[i]])-2
  }
  return(NN)
}


#function to create a data frame with with the ratio between two variables  
scores_ratio<-function(scores_num,scores2_den,names_rows,ratio_name){
  df<-data.frame(scores_num/scores2_den)
  names(df)<-ratio_name
  row.names(df)<-names_rows
  return(df)
}

#function to return the number of lags for which the PACF is above the threshold (IC 95%)
n_high_pacf<-function(DF,filter_test){
  myTS<-window(DF_to_TS(DF),end=filter_test)
  threshold=qnorm(.975)/sqrt(nrow(myTS))
  nms<-names(DF)[-1]
  newDF<-empty_DataFrames(length(nms),'high_pacf',nms)
  for (i in 1:length(nms)){
    newDF[i,1]<-length(which(abs(pacf(myTS[,i],6)$acf)>threshold))
    
  }
  return(newDF)
}



unlist_to_df<-function(list_of_lists,var_str){
  df<-data.frame(unlist(sapply(list_of_lists,"[",var_str)))
  rownames(df)<-names(list_of_lists)
  names(df)<-var_str
  return(df)
}