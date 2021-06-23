#############################################################
library(openxlsx)
library(car)
library(lmtest)
library(sandwich)
library(multcomp)
library(gplots)
#############################################################

setwd("/home/alf/Scrivania/lav_ugolini")

###################################################################################
# read data
# summary_data=read.xlsx("Summary.xlsx","tutti")
# saveRDS(summary_data,"summary_data.rds")
###################################################################################

summary_data=readRDS("summary_data.rds")

# "Species"     "traitment"   "len_roots"   "N_seed_germ" "N_seeds"     "P_germ"  

root_model<-glm(len_roots~ Species*traitment,data=summary_data)
germ_model<-glm(P_germ~ Species*traitment,data=summary_data)

# Build Two-way ANOVA model without interaction 

Anova(root_model)


png("root_glm_Residplots.png")
plot(root_model,which=1, main="Length roots GLM- Res vs Fitted")
dev.off()


png("root_glm_QQplots.png")
plot(root_model,which=2, main="Length roots GLM - QQplot")
dev.off()


Anova(germ_model)


png("germ_glm_REsidplots.png")
plot(germ_model,which=1, main="Full germination rate GLM - Res vs Fitted")
dev.off()


png("germ_glm_QQplots.png")
plot(germ_model,which=2, main="Full germination rate GLM - QQplot")
dev.off()





# References
# https://sites.utexas.edu/sos/guided/inferential/numeric/glm/
# https://www.statmethods.net/
