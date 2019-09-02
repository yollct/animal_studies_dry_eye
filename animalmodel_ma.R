library(meta)
library(metafor)
library(rmeta)
library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(plotly)

meta <- read.csv("/home/chit/Desktop/Hiwi_BMC/micestudies/revman_input1.csv", header=T, sep=",")
colnames(meta) <- c("model","system","study","country","parameter","site","Me","Se","Ne","Mc","Sc","Nc")

#automation for performing meta-analysis for each animal model
count <- 0
for (i in unique(meta$model)){
  png(sprintf("/home/chit/Desktop/Hiwi_BMC/micestudies/animal_%s.png", count), width = 1000, height=1000)
  #filter one model
  filter <- meta %>%
    filter(model==i)
  #meta-analysis for continuous data 
  #input for numaber of sample, mean, standard deviation for both experimental and control
  meta_mc <- metacont(Ne,Me,Se,Nc,Mc,Sc,
                      data=filter,
                      studlab = paste0(study,"_",parameter,"_",i),
                      sm="SMD",
                      comb.fixed = T #compute for fixed effect model,
                      comb.random = T #comput for random effect model,
                      label.e = "Dry eye model",
                      label.c = "Normal control")
  
  #decide each animal using fixed effect or random effect model
  ##check for inverse variance <0.5 use fixed effect model, otherwise use random effect model
  if (meta_mc$I2<0.5 | is.na(meta_mc$I2)){
    forest(meta_mc,
           comb.fixed = T,
           comb.random = F,
           digits = 2,
           digits.se = 2,
           col.square = "grey",
           col.square.lines = "grey",
           leftlabs = i,
           xlim = c(-20,20))
  } else {
    forest(meta_mc,
           comb.fixed = F,
           comb.random = T,
           digits = 2,
           digits.se = 2,
           col.square = "grey",
           col.square.lines = "grey",
           leftlabs = i,
           xlim = c(-20,20))
  }
  dev.off() ##image saved at the desired path
  count <- count+1 #counting loop iteration
  

}