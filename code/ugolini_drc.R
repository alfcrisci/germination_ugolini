# devtools::install_github("onofriAndreaPG/drcSeedGerm")

library(openxlsx)
library(drc)
library(lmtest)
library(sandwich)
library(drcSeedGerm)
library(tidyr)
library(maditr)
library(multcomp)

setwd(".")

germination_data=read.xlsx("ugolini_data.xlsx",1)
germination_table=read.xlsx("ugolini_data.xlsx",2)
 
saveRDS(germination_data,"germination_data.rds")
saveRDS(germination_table,"germination_table.rds")

germination_data=readRDS("germination_data.rds")
germination_table=readRDS("germination_table.rds")


germination_data_ls=split(germination_data,germination_data$specie)
germination_table_ls=split(germination_table,germination_table$species)
name_sp=gsub(" ","_",names(germination_data_ls))

res_summary=list()
res_ed=list()
res_coef=list()


for ( i in seq_along(germination_data_ls)) {
message(names(germination_data_ls)[i])  
message("\n")  
  
temp=germination_data_ls[[i]]
temp$day=temp$day+1
temp2=temp %>% dcast(treatment+reply~ day,value.var ="N_seeds_ger")
names(temp2)[2]="Dish"
counts <- temp2[,3:length(temp2[1,])]
treat <- data.frame(tratt=temp2[,1])
nViable <- rep(germination_table_ls[[i]]$seeds,nrow(counts))
moniTimes <- 1:(length(counts)-1)
dataset <- makeDrm(counts=counts, treat=treat, nViable=nViable, moniTimes)



model<- try(drm(propCum~timeBef , data=dataset, curveid = group,fct=LL2.3()))

if ( i ==4) {model<- try(drm(propCum~timeAf , data=dataset, curveid = group,fct=LL2.3()))}

png(filename = paste0(name_sp[i],"_model.png"))
plot(model,log="",col = TRUE,legendPos=c(4,0.4),cex.legend = 0.9,xlab="Days",main=names(germination_data_ls)[i])
dev.off()

if ( i ==3) {
  
  png(filename = paste0(name_sp[i],"_model.png"))
  plot(model,log="",col = TRUE,legendPos=c(6,0.4),cex.legend = 0.9,xlab="Days",main=names(germination_data_ls)[i])
  dev.off()
}

if ( i ==8) {
  
  png(filename = paste0(name_sp[i],"_model.png"))
  plot(model,log="",col = TRUE,legendPos=c(2,0.4),cex.legend = 0.9,xlab="Days",main=names(germination_data_ls)[i])
  dev.off()
}
res_ed[[i]]=ED(model, c(10,50,90), interval = "fls") # ED estimates effective doses (ECp/EDp/ICp) for given reponse levels.
res_coef[[i]]=coeftest(model, vcov = sandwich)
res_summary[[i]]=summary(model) # showing a summmary of the model fit (including parameter estimates)


}

names(res_ed)=names(germination_data_ls)
names(res_coef)=names(germination_data_ls)
names(res_summary)=names(germination_data_ls)
# references
# https://www.statforbiology.com/seedgermination/germindices
