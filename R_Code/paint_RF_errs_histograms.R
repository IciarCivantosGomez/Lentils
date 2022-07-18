# This script computes the RMSE distributions for each location with RF predictor
# for each IPCC model
#
# Input files: results/Roya_Future_all_iter_RF.xlsx
# Output file: tables/IPCCmodels_RMSE_site_RF.csv
#              tables/IPCCmodels_medRMSE_RF.csv
#              plots/IPCCmodels_RMSE_RF

library(ggplot2)
library(readxl)

Roya_Future_all_iter_RF <- read_excel("../Pyscripts/results/Roya_Future_all_iter_RF.xlsx")

sites <- unique(Roya_Future_all_iter_RF$index)
modelos <- names(Roya_Future_all_iter_RF)[1:9]
dferrores <- data.frame("RMSE"=c(),"Model"=c(),"index"=c())
for (j in 1:length(modelos)){
  model <- modelos[j]
  print(model)
  for (k in sites){
    valsite <- Roya_Future_all_iter_RF[Roya_Future_all_iter_RF$index==k,]$ROYA_CAMPO_AVERAGE_DSR[1]
    dfsite <- Roya_Future_all_iter_RF[Roya_Future_all_iter_RF$index==k,j]
    rmsesite <- sqrt((1/nrow(dfsite))*sum((dfsite-valsite)^2))
    dferrores <- rbind(dferrores,data.frame("RMSE"=rmsesite,"Model"=gsub("_"," ",model),"index"=k))
  }
}

RMSEplot <- ggplot(data=dferrores,aes(x=RMSE,y=Model))+geom_boxplot()+
  theme_bw()+xlab("RMSE for RF predictor")+ylab("IPCC Model")
  theme(panel.border = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        legend.position = "none",
        axis.line = element_line(colour = "black"),
        plot.title = element_text(lineheight=1.5, face="bold"),
        axis.text = element_text(face="bold", size=9),
        axis.title.x = element_text(face="bold", size=9),
        axis.title.y  = element_text(face="bold", size=9) )

ppi = 300  
odir <- "../plots"
if (!dir.exists(odir))
  dir.create(odir)
tdir <- "../tables"
if (!dir.exists(tdir))
  dir.create(tdir)

nfile <- paste0(odir,"/IPCCmodels_RMSE_RF")
png(paste0(nfile,".png"),width=7*ppi,height=6*ppi,res=ppi)
print(RMSEplot)
dev.off()
cairo_ps(paste0(nfile,".eps"),width=7,height=6,fallback_resolution=ppi)
plot(RMSEplot)
dev.off()

write.csv2(dferrores,paste0(tdir,"/IPCCmodels_RMSE_site_RF.csv"),row.names = FALSE)

dferroresglobal <- data.frame("RMSE"=c(),"Model"=c())
for (j in 1:length(modelos)){
  model <- modelos[j]
  medmodelo <- median(dferrores[(dferrores$Model==gsub("_"," ",model)),]$RMSE)
  dferroresglobal <- rbind(dferroresglobal,data.frame("RMSE"=medmodelo,"Model"=gsub("_"," ",model)))
}

write.csv2(dferroresglobal,paste0(tdir,"/IPCCmodels_medRMSE_RF.csv"),row.names = FALSE)
