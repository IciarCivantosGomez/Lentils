# This script computes the RMSE distributions for each location of the testing set
# where DSr was measured in laboratory
#
# Input files: results/Roya_Presente_AVG_ALL_iter_*** where *** stands for "Ridge", "RF" or "XGBoost"
# Output file: tables/model_Errors_AVG.csv
#              tables/Roya_Presente_merged_ALL.csv
#              tables/model_KSdist.csv
library(readxl)
library(ggplot2)
library(seewave)
library(patchwork)

plotpredhistogram <- function(datos,modelo,texto,KSdist,nbins=25){
  df1 <- data.frame("DSr"=datos$DSR_prediction.x)
  df1$metodo <- modelo
  df2 <- data.frame("DSr"=datos$ROYA_CAMPO_AVERAGE_DSR)
  df2$metodo <- "Measured"
  df1 <- rbind(df1,df2)
  p <- ggplot(df1,aes(x=DSr,fill=metodo))+ geom_histogram(aes(y=..count../sum(..count..)),  
                                                          alpha = .3,
                                                          position="identity", bins =nbins)+ 
    xlab(texto)+ylab("")+ggtitle(sprintf("KS distance %0.3f",as.numeric(KSdist)))+
    ylim(c(0,0.15))+xlim(c(0,100))+
    theme_bw()+
    theme(panel.border = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3", size = 0.2),
          panel.grid.major.x = element_blank(), 
          legend.title = element_blank(),
          legend.position = c(0.2,0.85),
          legend.key.width = unit(0.3, "cm"),
          legend.key.height = unit(0.3, "cm"),
          legend.text = element_text(size=8),
          legend.background = element_rect(fill="transparent"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=8, hjust=0.95,vjust=-20),
          axis.text = element_text(face="bold", size=9),
          axis.title.x = element_text(face="bold", size=9),
          axis.title.y  = element_text(face="bold", size=9) )
  return(p)
}

metodos <- c("RF","Ridge","XGB")
medianas <- data.frame("model"=rep("",length(metodos)),
                       "mediana"=rep(0,length(metodos)),
                       "media"=rep(0,length(metodos)))
dirresults <- "../Futuro/results"
j <- 1
for (metod in metodos)
{
  Roya_Presente_AVG_ALL_iter <- read_excel(paste0(dirresults,"/Roya_Presente_ALL_iter_",metod,".xlsx"))
  Roya_Presente_AVG <- read_excel(paste0(dirresults,"/Roya_Presente_avg_",metod,".xlsx"))
  Roya_Presente_merged <- merge(x=Roya_Presente_AVG_ALL_iter,y=Roya_Presente_AVG,by=c("DECLATITUDE","DECLONGITUDE"))
  Roya_Presente_merged$model <- metod
  Roya_Presente_AVG$test_index <- -1
  
  for (i in 1:nrow(Roya_Presente_AVG)){
    Roya_Presente_AVG$test_index[i] <- Roya_Presente_merged[Roya_Presente_merged$DECLATITUDE==Roya_Presente_AVG$DECLATITUDE[i] &
                           Roya_Presente_merged$DECLONGITUDE==Roya_Presente_AVG$DECLONGITUDE[i], ]$test_index[1]
  }
  Roya_Presente_AVG$RMSE <- 0
  Roya_Presente_AVG$media <- 0
  Roya_Presente_AVG$mediana <- 0
  Roya_Presente_AVG$model <- metod
  for (i in 1:nrow(Roya_Presente_AVG)){
    valueDSr <- Roya_Presente_AVG$ROYA_CAMPO_AVERAGE_DSR[i]
    predictionsdf <- Roya_Presente_merged[Roya_Presente_merged$test_index == Roya_Presente_AVG$test_index[i],]
    media <- mean(predictionsdf$DSR_prediction.x)
    mediana <- median(predictionsdf$DSR_prediction.x)
    RMSE <- sqrt(sum((predictionsdf$DSR_prediction.x-predictionsdf$ROYA_CAMPO_AVERAGE_DSR[1])^2)/nrow(predictionsdf))
    Roya_Presente_AVG$RMSE[i] <- RMSE
    Roya_Presente_AVG$media[i] <- media
    Roya_Presente_AVG$mediana[i] <- mediana
  }
  Roya_Presente_AVG <- Roya_Presente_AVG[order(Roya_Presente_AVG$ROYA_CAMPO_AVERAGE_DSR),]
  Roya_Presente_AVG$cumRMSE <- cumsum(Roya_Presente_AVG$RMSE)/nrow(Roya_Presente_AVG)
  print(metod)
  print("RMSE")
  print(summary(Roya_Presente_AVG$RMSE))
  medianas$mediana[j] <- summary(Roya_Presente_AVG$RMSE)[3]
  medianas$media[j] <- summary(Roya_Presente_AVG$RMSE)[4]
  medianas$model[j] <- metod
  print(paste("j",j))
  j <- j+1
  ks_distance <- ks.dist(Roya_Presente_AVG$ROYA_CAMPO_AVERAGE_DSR, Roya_Presente_AVG$mediana)$D
  print(paste("KS distance",ks_distance))
  mediatot <- mean(Roya_Presente_merged$ROYA_CAMPO_AVERAGE_DSR)
  x <- summary(Roya_Presente_AVG$RMSE)
  data.frame(x=matrix(x),row.names=names(x))
  if (metod == metodos[1]){
    Roya_Presente_AVG_ALL <- Roya_Presente_AVG
    Roya_Presente_merged_ALL <- Roya_Presente_merged
    model_KSdist <- data.frame("model"=metod,"KSdist"=ks_distance)
    sumario <- data.frame(x=t(matrix(x)))
  }
  else{
    Roya_Presente_AVG_ALL <- rbind(Roya_Presente_AVG_ALL,Roya_Presente_AVG)
    Roya_Presente_merged_ALL <- Roya_Presente_merged_ALL[,!(names(Roya_Presente_merged_ALL) %in% c("ROYA_CAMPO_AVERAGE_DSR.x"))]
    names(Roya_Presente_merged_ALL) <- names(Roya_Presente_merged)
    Roya_Presente_merged_ALL <- rbind(Roya_Presente_merged_ALL,Roya_Presente_merged)
    model_KSdist <- rbind(model_KSdist,data.frame("model"=metod,"KSdist"=ks_distance))
    sumario <- rbind(sumario,data.frame(x=t(matrix(x))))
  }
}
Roya_Presente_AVG_ALL <- Roya_Presente_AVG_ALL[,!(names(Roya_Presente_AVG_ALL) %in% c("DSR_prediction","index"))]
Roya_Presente_merged_ALL <- Roya_Presente_merged_ALL[,!(names(Roya_Presente_merged_ALL) %in% c("DSR_prediction.y","index"))]
sumario$Model <- metodos 
names(sumario) <- c(names(x),"Model")
RMSEplot <- ggplot(data=Roya_Presente_AVG_ALL,aes(x=model,y=RMSE))+geom_boxplot()+
           scale_y_sqrt(limits = c(0,100),breaks=c(1,10,30,seq(20,80,by=20)))+
  geom_point(data = medianas, aes(y = media,color=model),shape=18,size=2)+
  geom_text(data = medianas, aes(y = mediana,color=model,label = round(media,2)),size = 2.5, hjust=1.2, vjust = -0.5)+
  geom_text(data = medianas, aes(y = mediana,label = round(mediana,2)),size = 2.5, vjust = 1.5)+
  
  theme_bw()+xlab("")+
           theme(panel.border = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 panel.grid.major.y = element_line(linetype = 2, color="ivory3", size = 0.2),
                 panel.grid.major.x = element_blank(), 
           legend.position = "none",
           axis.line = element_line(colour = "black"),
           plot.title = element_text(lineheight=1.5, face="bold"),
           axis.text = element_text(face="bold", size=9),
           axis.title.x = element_text(face="bold", size=9),
           axis.title.y  = element_text(face="bold", size=9) )


RMSEcumsum <- ggplot(data=Roya_Presente_AVG_ALL,aes(x=ROYA_CAMPO_AVERAGE_DSR,y=cumRMSE,colour=model))+geom_point(alpha=.3)+
  theme_bw()+xlab("Lab samples DSr")+ylab("Cumulative RMSE\n")+
  theme(panel.border = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="ivory3", size = 0.2),
        panel.grid.major.x = element_blank(), 
        legend.title = element_blank(),
        legend.position = c(0.1,0.9),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size=8),
        legend.background = element_rect(fill="transparent"),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(lineheight=1.5, face="bold"),
        axis.text = element_text(face="bold", size=9),
        axis.title.x = element_text(face="bold", size=9),
        axis.title.y  = element_text(face="bold", size=9) )

odir <- "../plots"
if (!dir.exists(odir))
  dir.create(odir)
tdir <- "../tables"
if (!dir.exists(tdir))
  dir.create(tdir)

ppi <- 600
png(paste0(odir,"/RMSE_Roya.png"), width=7*ppi, height=5*ppi, res=ppi)
print(RMSEplot)
dev.off()
write.csv2(Roya_Presente_AVG_ALL,paste0(tdir,"/Model_Errors_AVG.csv"),row.names = FALSE)
write.csv2(Roya_Presente_merged_ALL,paste0(tdir,"/Roya_Presente_merged_ALL.csv"),row.names = FALSE)
write.csv2(model_KSdist,paste0(tdir,"/Model_KSdist.csv"),row.names = FALSE)
write.csv2(sumario,paste0(tdir,"/RMSE_summary.csv"),row.names = FALSE)
models <- sort(unique(Roya_Presente_merged_ALL$model))
i <- 1
p <- list()
sites <- unique(Roya_Presente_merged_ALL$test_index)
for (modelselect in models){
  preds <- Roya_Presente_merged_ALL[Roya_Presente_merged_ALL$model==modelselect,]
  pmod <- plotpredhistogram(preds,modelselect,"DSr",model_KSdist[model_KSdist$model==modelselect,]$KSdist)
  print(paste(modelselect))
  p[[i]] <- pmod
  i <- i+1
}
wsup <- (RMSEplot | RMSEcumsum) + plot_layout(widths = c(0.4,0.6 ))
winf <- (p[[1]]+ylab("Density\n") | p[[2]] | p[[3]])
w <- wsup / winf  + plot_layout(heights = c(0.45,0.55)) + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 11,face="bold",family=" Times New Roman",hjust = 0, vjust = -0.5))

nfile <- paste0(odir,"/predictions_errors")
png(paste0(nfile,".png"),width=10*ppi,height=6*ppi,res=ppi)
print(w)
dev.off()
cairo_ps(paste0(nfile,".eps"),width=10,height=6,fallback_resolution=ppi)
plot(w)
dev.off()