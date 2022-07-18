# This script plots de DSr maps and the distribution of RMSE histograms
#
# Input files: LentejasSilvestresAreasProtegidas.xlsx
#              Silvestres_Presente_alliter_*
#              SupplementaryMaterial10_RubioTeso_etal.xlsx
#              silvest_data_future_,modelprec,.xlsx
# Output file: ../tables/data_map_,modelprec,_,CChange_model,.csv
#              ../tables/HighValueSamples_,modelprec,_,CChange_model,.csv
#              ../tables/model_KSdist.csv
#              ../tables/SpeciesCountHV[FUTURE,NOW,PROTECTED]_modelprec_CChange_model
# Plots      : ../plots/   ALL MAPS
#            : ../plots/hist_preds_CChange_model_modelprec

library(ggplot2)
library(patchwork)
library(dplyr)
library(readxl)
library(maps)
library(countrycode)
library(forcats)
library(scico)

# Plot samples map

plot_map <-function(mapa,data_to_plot,vxlim=c(),vylim=c(),psize=1.5,valpha=0.7,mg="Variation",model="",lposition="right")
{
  print(mg)
  colprotec="gray15"
  if (mg == "Variation")
    data_to_plot$magnitude <- data_to_plot$Variation
  if (mg == "MagnitudPresent")
    data_to_plot$magnitude <- data_to_plot$MagnitudPresent
  if (mg == "MagnitudFuture")
    data_to_plot$magnitude <- data_to_plot$MagnitudFuture
  if (mg == "BIO12"){
    diferencia <- data_to_plot$FUTURE_BIO12-data_to_plot$CURRENT_BIO12
    data_to_plot$magnitude <- sign(diferencia)*sqrt(abs(diferencia))
    Legend_title <- "Precipitaton\nVariation\n(mm/year)"
  }
  # Temperatures were multiplied by 10 to build the model for normalization purposes
  if (mg == "BIO1"){
    diferencia <- (data_to_plot$FUTURE_BIO1-data_to_plot$CURRENT_BIO1)/10
    data_to_plot$magnitude <- sign(diferencia)*sqrt(abs(diferencia))
    Legend_title <- "Variaton of\nAvg. temp.\n(ºC)"
  }
    
  mapa_lens <- ggplot() +
    geom_polygon(data = mapa, aes(x=long, y = lat, group = group), 
                 fill="white",col="wheat3", alpha=0.1, lwd=0.3)
  if (mg == "BIO12") {
    colores_escala = c("turquoise4","papayawhip","orange")
    limitgraph <-c(min(data_to_plot$magnitude),max(data_to_plot$magnitude))
  }
  if (mg == "BIO1") {
    colores_escala = c("skyblue4","papayawhip","salmon3")
    limitgraph <-c(min(data_to_plot$magnitude),max(data_to_plot$magnitude))
  }
  if (mg == "Variation") {
    colores_escala = c("darkblue","papayawhip","firebrick")
    limitgraph <-c(-1,1)* max(abs(data_to_plot$magnitude))
    Legend_title <- "DSr Variation"
  }
  if ((mg == "MagnitudPresent") | (mg == "MagnitudFuture")){
    colores_escala = c("darkgreen","green","orange","red")
    limitgraph <-c(20,75)
    Legend_title <- "DSr"
  }
  mapa_lens <- mapa_lens + geom_point(data=data_to_plot, 
                                      aes(x=Longitude_decimal, y=Latitude_decimal, 
                                          fill = magnitude,color=magnitude,shape=Protected),
                                      alpha=valpha,size=psize)+
    scale_shape_manual(values = c(16, 15))
  if (!(mg %in% c("BIO12","BIO1"))){
  mapa_lens <- mapa_lens+
                 scale_fill_gradientn(colours = colores_escala, name=Legend_title, limits = limitgraph) +
                 scale_color_gradientn(colours = colores_escala, name=Legend_title,limits = limitgraph) 
  mapa_lens<- mapa_lens+guides(shape = guide_legend(order = 2),col = guide_colourbar(order = 1),fill = guide_colourbar(order = 1))
  
  }
  else{
    if (mg=="BIO1")
      salto <- 1
    else
      salto <- 10
    vector_breaks <- seq(min(data_to_plot$magnitude),max(data_to_plot$magnitude),by=salto)
    if (mg=="BIO1")
      vector_labs <- round(sign(vector_breaks)*(vector_breaks)^2,1)
    else
      vector_labs <- round(sign(vector_breaks)*(vector_breaks)^2,0)
    mapa_lens <- mapa_lens+ylab("")+
    scale_fill_gradientn(colours = colores_escala, name=Legend_title,
                         labels=vector_labs,breaks=vector_breaks)+
    scale_color_gradientn(colours = colores_escala, name=Legend_title,
                          labels=vector_labs,breaks=vector_breaks)
    mapa_lens<- mapa_lens+guides(fill = guide_colourbar(order = 1),color=guide_colourbar(order = 1),shape = guide_legend(order = 2))
  
  }
   
  mapa_lens <- mapa_lens + coord_map()+theme_bw() + xlim(vxlim) +
               xlab("Longitude") + 
               theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size = 0.4,linetype = 3))
  if (length(vylim)>0)
    mapa_lens <- mapa_lens + ylim(vylim)
  if (mg == "MagnitudPresent")
    etq_title <- "Present time  "
  else
    etq_title <- paste("Model",model," ")
  mapa_lens <- mapa_lens + theme(plot.title = element_text(vjust = -10, hjust = 1, 
                                                           size = 12, color="grey50")) + ggtitle(etq_title)  
  mapa_lens <- mapa_lens + theme(panel.border = element_rect(colour = "black", size = 0.2, fill = NA),
                                 legend.position = lposition,
                                 legend.text = element_text(size = 11),
                                 axis.text = element_text( size=11),
                                 axis.title.x = element_text( size=12),
                                 axis.title.y  = element_text( size=12),
                            plot.margin=unit(c(-0.50,-0.50,-0.50,-0.50), "null")) 
  mapa_legend_less <- mapa_lens + theme(legend.position = "none")  
  return(list(mapa_lens,mapa_legend_less))
}

# Encircles high value samples

AddVHInterest <- function(pmap,datos,color = "magenta")
{
  pmap <- pmap + geom_point( data=datos[datos$VeryHighValue,], 
                aes(x=Longitude_decimal, y=Latitude_decimal),
                shape= 21,size=4,stroke=1,alpha=0.5, col= color)
  return(pmap)
}

# Add alphabetical labels to a multiple plot

AddMultiplotLabel <- function(pl,pvjust=-0.5,fsize=11)
{
  return (pl + plot_annotation(tag_levels = 'A') &
    theme(plot.tag = element_text(size = fsize,face="bold",family=" Times New Roman",hjust = 0, vjust = pvjust)))
}

# Save plot as png and eps files

SavePlot <- function(plotfile,plotname, pwidth=10, pheight=5, ppi = rppi, eps=FALSE)
{
  png(paste0(plotfile,".png"),width=pwidth*ppi,height=pheight*ppi,res=ppi)
  print(plotname)
  dev.off()
  if (eps){
    cairo_ps(paste0(plotfile,".eps"),width=pwidth,height=pheight,fallback_resolution=ppi)
    plot(plotname)
    dev.off()
  }
}

# Plot the histogram of predicted DSr values for wild samples

plotprvsfuthistogram <- function(datos,modelo,CChange_model,texto,nbins=25){
  # Histogram of DSr values
  dfnow <- data.frame("DSr"=datos$MagnitudPresent)
  dfnow$metodo <- "Present time"
  dffuture <- data.frame("DSr"=datos$MagnitudFuture)
  dffuture$metodo <- CChange_model
  colores_escala = c("orange","lightblue")
  df1 <- rbind(dfnow,dffuture)
  mediannow <- median(dfnow$DSr)
  medianfuture <- median(dffuture$DSr)
  p <- ggplot(df1,aes(x=DSr,fill=metodo))+ geom_histogram(aes(y=..count../sum(..count..)),  
                                                          alpha = .5,
                                                          position="identity", bins =nbins)+ 
    xlab(texto)+ylab("Density")+scale_fill_manual(values = colores_escala) +
    geom_vline(data = dfnow, aes(xintercept = median(DSr)),color="orange", size=0.7,alpha=0.9)+
    geom_vline(data = dffuture, aes(xintercept = median(DSr)),color="skyblue3", size=0.7,alpha=0.9)+
    
    geom_text(x=mediannow+1.5,y = 0.012,label = round(mediannow,2),color="darkorange3",size = 4, hjust=0, vjust = 0, angle=90)+
    geom_text(x=medianfuture-0.5,y = 0.012,label = round(medianfuture,2),color="blue",size =4, hjust=0, vjust = 0, angle=90)+
    theme_bw()+
    theme(panel.border = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3", size = 0.2),
          panel.grid.major.x = element_blank(), 
          legend.title = element_blank(),
          legend.position = c(0.9,0.9),
          legend.text = element_text(size=10),
          legend.background = element_rect(fill="transparent"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=8, hjust=0.95,vjust=-20),
          axis.text = element_text(face="bold", size=12),
          axis.title.x = element_text(face="bold", size=12),
          axis.title.y  = element_text(face="bold", size=12) )
  return(p)
}

# Plot the variation histogram of variables affected by climate change

plotBIOvarhistogram <- function(datos,modelo,CChange_model,nbins=25){
  varbio12 <- data.frame("valor"=datos$FUTURE_BIO12-datos$CURRENT_BIO12)
  varbio1 <-  data.frame("valor"=(datos$FUTURE_BIO1-datos$CURRENT_BIO1)/10)  
  textos <- c("Var Precip.(mm/year)","Var Temp. (ºC)")
  ldata <- list(varbio12,varbio1)
  myplots <- list()
  colores <- c("pink","green")
  for (j in (1:length(ldata))){
  p <- eval(substitute(
       ggplot(ldata[[j]],aes(x=valor))+ geom_histogram(aes(y=..count../sum(..count..)),  
                                                     alpha = .5,fill=colores[j],color=colores[j],
                                                     position="identity", bins =nbins)+ 
           xlab(textos[j])+ylab("")+theme_bw()+
           theme(panel.border = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank(),
           panel.grid.major.y = element_line(linetype = 2, color="ivory3", size = 0.2),
           panel.grid.major.x = element_blank(), 
           legend.position = "none",
           axis.line = element_line(colour = "black"),
           axis.text = element_text(face="bold", size=12),
           axis.title.x = element_text(face="bold", size=12),
           axis.title.y  = element_text(face="bold", size=12) )
       ,list(j = j)))
       myplots[[j]] <- p
  }
  return(myplots)
}

# CONFIGURATION CODE
print_indiv_maps <- FALSE     # print individual country maps. Time consuming
printepsfile <- TRUE          # print in EPS format output files. By default just .png
rppi <- 600                   # plots resolution
encircle_all_interest_samples <- TRUE
CChange_model <- "BCC_370"    # Use BCC_370 results. You can change it by any other IPCC scenario
modelprec <- "RF"             # Use Random Forest as predictor model
odir <- "../plots"
results_dir <- "../Pyscripts/results/"
data_dir <- "../Pyscripts/datasets/DatosPresente/"

if (!dir.exists(odir))
  dir.create(odir)
ProtAreas <- read_excel(paste0(data_dir,"LentejasSilvestresAreasProtegidas.xlsx"))
# Data loading. Predictions of values in present time
Present_DSr_predictions <-read_excel(paste0(results_dir,"Silvestres_Presente_alliter_",modelprec,".xlsx"))

# 30 resistant populations according to previous paper
# ResistantPopulations <- read_excel(paste0(data_dir,"SupplementaryMaterial10_RubioTeso_etal.xlsx"),skip = 3)

# Predictions under different climatic change scenarios
Future_DSr_predictions <- read_excel(paste0(results_dir,"Silvestres_Future_alliter_",modelprec,".xlsx"))
Future_DSr_predictions <- Future_DSr_predictions[,c(CChange_model,"test_index","DECLONGITUDE","DECLATITUDE")]
names(Future_DSr_predictions)[1] <- "MODEL"

model_features <- read_excel(paste0(results_dir,"silvest_data_future_",modelprec,".xlsx"))
model_features <- model_features[,(names(model_features) %in% c(CChange_model,paste0(CChange_model,"_B12"),"DECLATITUDE","DECLONGITUDE","CURRENT_BIO1","CURRENT_BIO12"))]
names(model_features) <- gsub("DECLATITUDE","Latitude_decimal",names(model_features))
names(model_features) <- gsub("DECLONGITUDE","Longitude_decimal",names(model_features))
names(model_features)[1] <- "FUTURE_BIO1"
names(model_features)[2] <- "FUTURE_BIO12"


vmags <- c("MagnitudPresent","MagnitudFuture","Variation")
for (magnitudplot in vmags)
{
  alpha_dots <- 0.5
  if (magnitudplot == "Variation")
    alpha_dots <- 0.9
  if (magnitudplot != "MagnitudPresent")
    etq_map <- paste0(CChange_model,"_",modelprec)
  else
    etq_map <- modelprec
  
  locations <- Present_DSr_predictions[,-c(1)]
  locations <- locations[!duplicated(locations),]
  
  medidacentrals_presente <- Present_DSr_predictions %>% group_by(test_index) %>%
    summarise(mean = mean(DSR_prediction))
  
  medidacentrals_futuro <-   Future_DSr_predictions %>% group_by(test_index) %>%
    summarise(mean = mean(MODEL))
  
  medidacentrals <- merge(x=medidacentrals_presente,y=medidacentrals_futuro,by="test_index")
  names(medidacentrals)[2] <- "MagnitudPresent"
  names(medidacentrals)[3] <- "MagnitudFuture"

  medidacentrals$Variation <- medidacentrals$MagnitudFuture-medidacentrals$MagnitudPresent
  medidacentrals$VariationPerc <- 100*(medidacentrals$MagnitudFuture-medidacentrals$MagnitudPresent)/medidacentrals$MagnitudPresent
  data_to_plot <- medidacentrals
  data_to_plot$Longitude_decimal <- 0
  data_to_plot$Latitude_decimal <- 0
  for (i in 1:nrow(data_to_plot)){
      fila <- which(Future_DSr_predictions$test_index==data_to_plot$test_index[i])[1]
      data_to_plot$Longitude_decimal[i] <- locations[fila,]$DECLONGITUDE
      data_to_plot$Latitude_decimal[i] <- locations[fila,]$DECLATITUDE
  }
  data_to_plot$simbolo <- as.factor(data_to_plot$Variation< 0)
  
  mapamundo <- map_data("world")
  all_countries <- c("Spain","Portugal","France","Italy","Slovenia","Croatia","Greece","Turkey","Cyprus", "Montenegro",
              "Bosnia and Herzegovina", "Macedonia","Albania","Bulgaria","Romania", "Ukraine","Serbia","Moldavia",
              "Malta","Kosovo")
  mapa <- subset(mapamundo, region %in% all_countries)
  
  data_to_plot$Protected <- "No"
  data_to_plot$ACCENUMB <- 0
  data_to_plot$COLLSITE <- ""
  data_to_plot$SPECIES <- ""
  for (k in 1:nrow(data_to_plot)){
    
    if ((is.element(data_to_plot$Latitude_decimal[k],ProtAreas$DECLATITUD)) &
        (is.element(data_to_plot$Longitude_decimal[k],ProtAreas$DECLONGITU))){
        data_to_plot$Protected[k] <- "Yes" 
        muestra <- ProtAreas[data_to_plot$Latitude_decimal[k] == ProtAreas$DECLATITUD &
                           data_to_plot$Longitude_decimal[k]== ProtAreas$DECLONGITU,]
        data_to_plot$SPECIES[k] <- muestra$SPECIES
        data_to_plot$ACCENUMB[k] <- muestra$ACCENUMB
        data_to_plot$COLLSITE[k] <- muestra$COLLSITE
       }
  
  }
  data_to_plot$Protected <- as.factor(data_to_plot$Protected)
  
  
  data_to_plot <- merge(x=data_to_plot,y=model_features,by=c("Latitude_decimal","Longitude_decimal"))
  
  plims <- c(-10,45)
  pl_map <- plot_map(mapa,data_to_plot,psize=1.5,vxlim=plims,mg=magnitudplot,model=CChange_model,
                        valpha=alpha_dots)
  mapa_lens <- pl_map[[1]]+ylab("Latitude")
  if (magnitudplot=="MagnitudPresent")
    mapa_lens_present <- mapa_lens
  if (magnitudplot=="Variation")
    mapa_lens_variation <- mapa_lens
  
  vprecip_map_lens <- plot_map(mapa,data_to_plot,psize=1.5,vxlim=plims,mg="BIO12",
                               model=CChange_model,valpha=alpha_dots)
  vtemp_map_lens <- plot_map(mapa,data_to_plot,psize=1.5,vxlim=plims,mg="BIO1",
                             model=CChange_model,valpha=alpha_dots)
  
  plims <- c(-10,10)
  mapa <- subset(mapamundo, region %in% c("Spain","Portugal","France"))
  pl_map <- plot_map(mapa,data_to_plot,vxlim=plims,  mg=magnitudplot,model=CChange_model,valpha=alpha_dots)
  mapa_lens_west <- pl_map[[1]]+ylab("Latitude")
  
  if (magnitudplot=="MagnitudPresent")
    plwest_pres<-pl_map[[1]]+ylab("Latitude")
  if (magnitudplot=="MagnitudFuture")
    plwest_futur<-pl_map[[2]]+ylab("")
  if (magnitudplot=="Variation")
    plwest_variation<-pl_map[[1]]+ylab("")
  
  vprecip_map_west <- plot_map(mapa,data_to_plot,psize=1.5,vxlim=plims,mg="BIO12",model=CChange_model,valpha=alpha_dots)
  vtemp_map_west <- plot_map(mapa,data_to_plot,psize=1.5,vxlim=plims,mg="BIO1",model=CChange_model,valpha=alpha_dots)
  
  plims <- c(20,25)
  mapa <- subset(mapamundo, region %in% c("Greece"))
  pl_map <- plot_map(mapa,data_to_plot,vxlim=plims,vylim=c(36,42), mg=magnitudplot,
                                model=CChange_model,valpha=alpha_dots)
  mapa_lens_greece <- pl_map[[1]]+ylab("Latitude")
  
  
  if (print_indiv_maps){
    SavePlot(paste0(odir,"/mapa_lens_",magnitudplot,etq_map,".png"),mapa_lens)
    SavePlot(paste0(odir,"/mapa_lens_west",magnitudplot,etq_map,".png"),mapa_lens_west)
    SavePlot(paste0(odir,"/mapa_lens_greece",magnitudplot,etq_map,".png"),mapa_lens_greece)
  }
  if (magnitudplot=="MagnitudPresent")
    plgreece_pres<-pl_map[[1]]+ylab("Latitude")
  if (magnitudplot=="MagnitudFuture")
    plgreece_futur<-pl_map[[2]]+ylab("")
  if (magnitudplot=="Variation")
    plgreece_variation<-pl_map[[1]]+ylab("")
}

resistDSr <- summary(data_to_plot$MagnitudPresent)[2] # First quartile

print(paste("resistDSr",resistDSr))
data_to_plot$VeryHighValue <- (data_to_plot$Protected == "Yes") & (data_to_plot$MagnitudPresent <= resistDSr) & (data_to_plot$MagnitudFuture <= resistDSr)
data_to_plot$HighValueNow <- (data_to_plot$Protected == "Yes") & (data_to_plot$MagnitudPresent <= resistDSr)


if (encircle_all_interest_samples){
  plgreece_pres <- AddVHInterest(plgreece_pres,data_to_plot)
  plwest_pres <- AddVHInterest(plwest_pres,data_to_plot)
}

b <- plgreece_pres| plgreece_futur | plgreece_variation
b <- AddMultiplotLabel(b)

bpair <- plgreece_pres| plgreece_variation
bpair <- AddMultiplotLabel(bpair)

w <- plwest_pres| plwest_futur | plwest_variation
w <- AddMultiplotLabel(w) 

wpair <- plwest_pres| plwest_variation
wpair <- AddMultiplotLabel(wpair) 

# Add high value marks to full_map
mapa_lens_present <- AddVHInterest(mapa_lens_present,data_to_plot)+
  theme(axis.title.x=element_blank())
ml <- mapa_lens_present / mapa_lens_variation
ml <- AddMultiplotLabel(ml) 

mapa_west_changes <- (plwest_variation+ylab("Latitude")) | vprecip_map_west[[1]] | vtemp_map_west[[1]]
mapa_west_changes <- AddMultiplotLabel(mapa_west_changes)

mapa_lens_changes <- mapa_lens_present / (mapa_lens_variation +
                                            theme(axis.title.x=element_blank()))/ (vprecip_map_lens[[1]] +ylab("Latitude")) 
mapa_lens_changes <- AddMultiplotLabel(mapa_lens_changes,pvjust=-5,fsize=15)


SavePlot(paste0(odir,"/mapa_lens_west_COMPARATIVE_",CChange_model,"_",modelprec),w,pwidth=15,pheight=5,eps=printepsfile)
SavePlot(paste0(odir,"/mapa_lens_PROTECTED_",CChange_model,"_",modelprec),ml,pwidth=9,pheight=8,eps=printepsfile)
SavePlot(paste0(odir,"/mapa_lens_greece_COMPARATIVE_",CChange_model,"_",modelprec),b, pwidth=10, pheight=5,eps=printepsfile)
SavePlot(paste0(odir,"/mapa_lens_greece_PAIR_",CChange_model,"_",modelprec),bpair, pwidth=10, pheight=5,eps=printepsfile)
SavePlot(paste0(odir,"/mapa_lens_west_PAIR_",CChange_model,"_",modelprec),wpair, pwidth=10, pheight=5,eps=printepsfile)

SavePlot(paste0(odir,"/mapa_lens_west_CHANGES_",CChange_model,"_",modelprec),mapa_west_changes, pwidth=18, pheight=6,eps=printepsfile)
SavePlot(paste0(odir,"/mapa_lens_ALL_CHANGES_",CChange_model,"_",modelprec),mapa_lens_changes, pwidth=10, pheight=10*0.9*sqrt(2),eps=printepsfile)

ph <- plotprvsfuthistogram(data_to_plot,modelprec,CChange_model,"DSr",nbins=25)
h <- plotBIOvarhistogram(data_to_plot,modelprec,CChange_model,nbins=25)

histsp <- (ph | (h[[1]] / h[[2]])) + plot_layout(widths = c(2, 1))
histsp <- AddMultiplotLabel(histsp,fsize = 15)

SavePlot(paste0(odir,"/histpreds_",CChange_model,"_",modelprec),histsp, pwidth=10, pheight=5,eps=printepsfile)


tdir <- "../tables"
if (!dir.exists(tdir))
  dir.create(tdir)
write.csv2(data_to_plot,paste0(tdir,"/data_map_",modelprec,"_",CChange_model,".csv"),row.names=FALSE)

high_value_samples <- data_to_plot[data_to_plot$HighValueNow,]
write.csv2(high_value_samples,paste0(tdir,"/HighValueSamples_",modelprec,"_",CChange_model,".csv"),row.names=FALSE)

specall <- as.data.frame(table(data_to_plot$SPECIES))
specprot <- as.data.frame(table(data_to_plot[data_to_plot$Protected=="Yes",]$SPECIES))
specvalnow <- as.data.frame(table(high_value_samples$SPECIES))
specvalfuture <- as.data.frame(table(high_value_samples[high_value_samples$VeryHighValue,]$SPECIES))

names(specall) <- c("Species", "Count")
names(specprot) <- names(specall)
write.csv2(specprot,paste0(tdir,"/SpeciesCountPROTECTED_",modelprec,"_",CChange_model,".csv"),row.names=FALSE)
names(specvalnow) <- names(specall)
write.csv2(specvalnow,paste0(tdir,"/SpeciesCountHVNOW_",modelprec,"_",CChange_model,".csv"),row.names=FALSE)
names(specvalfuture) <- names(specall)
write.csv2(specvalfuture,paste0(tdir,"/SpeciesCountHVFUTURE_",modelprec,"_",CChange_model,".csv"),row.names=FALSE)
