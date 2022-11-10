####Language data from goal project 
#Packages to install / load to run this script
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("psych")) install.packages("psych")
if (!require("qualtRics")) install.packages("qualtRics")
if (!require("summarytools")) install.packages("summarytools")

#library the packages
library(tidyverse)
library(qualtRics)
library(psych)
library(summarytools)
library(Hmisc)
library(corrplot)
library(doc2concrete)

setwd("~/Desktop/goals project") 
df <- read_csv("LIWC-22 Results - disaggregated_data - LIWC Analysis.csv") #read in data 

palette = c("#772e25", "#c44536", "#ee9b00", "#197278", "#283d3b")
plot_aes = theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size = 8),
        text = element_text(size = 10, family = "Futura Medium"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        axis.ticks.y = element_blank())




#fitler WC < 25
data <- df %>% filter( WC > 25 )
#get descriptives for LIWC variables 
describeBy(data, group = data$timepoint_num)

#names <- select(data,14:130) #get names of LIWC variables 
#LIWCvar <- colnames(names) #if you wanna graph ALL the variables

#or can do this for a simpler amount 
LIWCvar <- c("WC","Analytic","Cognition","cogproc","cause","tentat","Drives","emo_pos", "emo_neg","emo_anx",
             'focuspast','focuspresent','focusfuture','i',"achieve","Autonomous","Controlling")


#loop to create density plots 

LIWC_dens <- function(data, x, y){
ggplot(data = data) +
  geom_density(aes_string(x = LIWCvar[i]),
               adjust = 1.5, 
               alpha = 0.5, fill = "dodgerblue") + theme_bw()
}

for(i in 1:17) { 
  nam <- paste("d", i, sep = "")
  assign(nam, LIWC_dens(data,LIWCvar[i],"timepoint"))
}
ggarrange(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17, common.legend = T, legend = 'right')


df$concreteness <- doc2concrete(df$goal)

data_small <- df[c(5,7:10,15,49,132:134)]


rcorr <- rcorr(as.matrix(data_small))
rcorr

coeff = rcorr$r #coefficient
pval = rcorr$P #p-value

#Visualize 
corrplot(rcorr$r, type="upper",bg = "black", method = "number",p.mat = rcorr$P, sig.level = 0.10, number.cex= 7.9/ncol(data_small), insig = 'blank')





