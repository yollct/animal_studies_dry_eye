library(meta)
library(metafor)
library(rmeta)
library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(plotly)

#subgroup meta-analysis for animal models
png("/home/chit/Desktop/Hiwi_BMC/micestudies/animalnew.png", width = 600, height =400)
smd <- c()
seTE <- c()
lower <- c()
upper <- c()
study <- unique(meta$model)
#results for each model is obtained
#smd, standard error, lower and upper bound of confident interval for each study input
for (i in unique(meta$model)){
  filter <- meta %>%
    filter(model==i)
  meta_mc <- metacont(Ne,Me,Se,Nc,Mc,Sc,
                      data=filter,
                      studlab = model,
                      sm="SMD",
                      comb.fixed = F,
                      comb.random = T,
                      label.e = "Dry eye model",
                      label.c = "Normal control")
  #meta-analysis for continuous data 
  #input for numaber of sample, mean, standard deviation for both experimental and control
  if (meta_mc$I2<0.5 | is.na(meta_mc$I2)){  
    smd <- c(smd, meta_mc$TE.fixed)
    seTE <- c(seTE, meta_mc$seTE.fixed)
    lower <- c(lower, meta_mc$lower.fixed)
    upper <- c(upper, meta_mc$upper.fixed)
  } else {
    smd <- c(smd, meta_mc$TE.random)
    seTE <- c(seTE, meta_mc$seTE.random)
    lower <- c(lower, meta_mc$lower.random)
    upper <- c(upper, meta_mc$upper.random)
  }
}

##generic inverse variance meta-analysis for subgroups
#TE: estimated effect (smd), seTE: standard error, lower and upper: lower and upper of confident interval
meta_immune <- metagen(TE=smd,
                       seTE=seTE,
                       lower = lower,
                       upper = upper,
                       studlab = study,
                       sm="SMD")

#results in forest plot
forest(meta_immune,
       comb.fixed = F,
       comb.random = T,
       digits = 2,
       digits.se = 2,
       leftcols = c("studlab"),
       leftlabs = "Model",
       rightcols = c("effect","ci"),
       studlab = T,
       col.diamond.random = "lightblue",
       col.square = "darkblue",
       test.overall.random = F,
       just.studlab = "left",
       hetstat = F,
       hetlab = "",
       label.right = "Favours disease",
       label.left = "Favours control")
dev.off() ##saved