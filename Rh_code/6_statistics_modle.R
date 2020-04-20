library('dplyr') #if funciton masked, please run it again

workingpath = getwd()
res.data <- read.csv(choose.files(),check.names=FALSE)

dir.create(paste0(workingpath,"/ModelTable")) 

###model extraction function--------------------
lm.cof <- function(df,y,x){
  Eq = as.formula(paste0(y,'~',x))
  m <- summary(lm(Eq, df))
  output = as.data.frame(m$coefficients)[-1,]
  output$R.squared =m$adj.r.squared
  output$Sigma = m$sigma
  output$Counts = length(m$residuals)
  return(output)
}
model.cof <- function(mdata){
  m <- summary(mdata)
  output = as.data.frame(m$coefficients)[-1,]
  output$R.squared =m$adj.r.squared
  output$Sigma = m$sigma
  output$Counts = length(m$residuals)
  return(output)
}
pair.matrix<-function(m){
  pair = data.frame(row=rownames(m)[row(m)[upper.tri(m)]], 
             col=colnames(m)[col(m)[upper.tri(m)]], 
             corr=m[upper.tri(m)])
}
standardize_sd = function(x){
  ss = (x-mean(x,na.rm = TRUE))/sd(x,na.rm = TRUE)
}
###prepare data---------------------
work.data = filter(res.data, !is.na(Rh_ratio)&Study_midyear >= 1987 )#

summary(aov(Rh_ratio~Meas_method,work.data))
###optional: standardization
work.data = select_if(work.data,is.numeric)
#dependant variables,'Biome'
dv = c('Rs_annual','Rh_annual','Ra_annual','Rh_ratio')
idv = c('Study_midyear','Latitude','Elevation',
        'Mean.SPEI','Stdev.SPEI','MAT','MAP','ΔMean.Temp.',
        'ΔMean.Precip.','ΔStdev.Temp.','ΔStdev.Precip.')
work.data[,idv] = sapply(work.data[,idv],standardize_sd)
work.data = na.omit(work.data[,c(idv,dv)])
###multiple regression------------------------------
###-=-------------------


#categorical variables-------------------
C.idv = c('Meas_method','YearsOfData','Ecosystem_type')
C.idv.part = paste(C.idv,collapse = '+')

#'Study_midyear:Meas_method','SPEI:YearsOfData','Mean.Temp.:YearsOfData','Mean.Precip.:YearsOfData',
i=4

### model1---------the linear without 30y change
#consecutive variables ------------------
idv = c('Study_midyear','Latitude','Elevation','Mean.SPEI','MAT','MAP')
idv.part1 = paste(idv,collapse = '+')
idv.part2 = paste('I(',idv,'^2)',collapse = '+')
#interaction variables-------------------
inter.idv = c('Study_midyear:Latitude',
              'MAT:MAP',
              'Study_midyear:Latitude:MAT',
              'Study_midyear:Latitude:MAP',
              'Study_midyear:Latitude:Mean.SPEI')
inter.idv.part = paste(inter.idv,collapse = '+')
lmformula = paste0(dv[i],'~', idv.part1,'+',inter.idv.part)
multi.lm = lm(lmformula ,data = work.data,na.action = 'na.omit')
summary(multi.lm)
model.1 <-model.cof(step(multi.lm,direction = 'backward'))
aov.modle.1 <- anova(step(multi.lm,direction = 'backward'))
write.csv(model.1,paste0(workingpath,'/ModelTable/',dv[i],'.lm.model1.csv'),row.names=TRUE)
write.csv(aov.modle.1,paste0(workingpath,'/ModelTable/',dv[i],'.aov.model1.csv'),row.names=TRUE)

### model2---------the linear with 30y change------------------
#consecutive variables ------------------
idv = c('Study_midyear','Latitude','Elevation','Mean.SPEI','Stdev.SPEI',
        'MAT','MAP','ΔMean.Temp.','ΔMean.Precip.','ΔStdev.Temp.','ΔStdev.Precip.')
idv.part1 = paste(idv,collapse = '+')
idv.part2 = paste('I(',idv,'^2)',collapse = '+')

#interaction variables-------------------
inter.idv = c('Study_midyear:Latitude',
              'MAT:MAP',
              'Study_midyear:Latitude:MAT',
              'Study_midyear:Latitude:MAP',
              'Study_midyear:Latitude:Mean.SPEI')
inter.idv.part = paste(inter.idv,collapse = '+')
lmformula = paste0(dv[i],'~', idv.part1,'+', inter.idv.part)
multi.lm = lm(lmformula ,data = work.data)
model.2 <-model.cof(step(multi.lm,direction = 'backward'))
aov.modle.2 <- anova(step(multi.lm,direction = 'backward'))
write.csv(model.2,paste0(workingpath,'/ModelTable/',dv[i],'.lm.model2.csv'),row.names=TRUE)
write.csv(aov.modle.2,paste0(workingpath,'/ModelTable/',dv[i],'.aov.model2.csv'),row.names=TRUE)

### model3---------Only linear with 30y change
#consecutive variables ------------------
idv = c('Study_midyear','Latitude','Elevation','Stdev.SPEI',
        'ΔMean.Temp.','ΔMean.Precip.','ΔStdev.Temp.','ΔStdev.Precip.')
idv.part1 = paste(idv,collapse = '+')
idv.part2 = paste('I(',idv,'^2)',collapse = '+')
inter.idv = c('Study_midyear:Latitude',
              'Study_midyear:Latitude:Stdev.SPEI',
              'Study_midyear:Latitude:ΔMean.Temp.',
              'Study_midyear:Latitude:ΔMean.Precip.')
inter.idv.part = paste(inter.idv,collapse = '+')
lmformula = paste0(dv[i],'~', idv.part1, '+',inter.idv.part)
multi.lm = lm(lmformula ,data = work.data)
model.3 <-model.cof(step(multi.lm,direction = 'backward'))
aov.modle.3 <- anova(step(multi.lm,direction = 'backward'))
write.csv(model.3,paste0(workingpath,'/ModelTable/',dv[i],'.lm.model3.csv'),row.names=TRUE)
write.csv(aov.modle.3,paste0(workingpath,'/ModelTable/',dv[i],'.aov.model3.csv'),row.names=TRUE)

### model4---------linear full model
#consecutive variables ------------------
idv = c('Study_midyear','Latitude','Elevation','Mean.SPEI','Stdev.SPEI',
        'MAT','MAP','ΔMean.Temp.','ΔMean.Precip.','ΔStdev.Temp.','ΔStdev.Precip.')
idv.2 = paste0('I(',idv,'^2)')
idv.part1 = paste(idv,collapse = ' + ')
idv.part2 = paste(idv.2,collapse = '+')
inter.idv = c('Study_midyear:Latitude','MAT:MAP',
              'Latitude:MAT',
              'Latitude:MAP',
              'Latitude:Mean.SPEI',
              'Latitude:Stdev.SPEI',
              'Latitude:ΔMean.Temp.',
              'Latitude:ΔMean.Precip.',
              'Latitude:ΔStdev.Temp.',
              'Latitude:ΔStdev.Precip.') 
inter.idv.part = paste(inter.idv,collapse = ' + ')
lmformula = paste(dv[i],'~', idv.part1, '+',inter.idv.part)
multi.lm = lm(lmformula ,data = work.data)
model.4 <-model.cof(step(multi.lm,direction = 'backward'))
aov.modle.4 <- anova(step(multi.lm,direction = 'backward'))
write.csv(model.4,paste0(workingpath,'/ModelTable/',dv[i],'.lm.model4.csv'),row.names=TRUE)
write.csv(aov.modle.4,paste0(workingpath,'/ModelTable/',dv[i],'.aov.model4.csv'),row.names=TRUE)

### model5---------Both linear and unlinear, full model
lmformula = paste0(dv[i],'~', idv.part1, '+',idv.part2,'+',inter.idv.part)
multi.lm = lm(lmformula ,data = work.data)
model.5 <-model.cof(step(multi.lm,direction = 'backward'))
aov.modle.5 <- anova(step(multi.lm,direction = 'backward'))
write.csv(model.5,paste0(workingpath,'/ModelTable/',dv[i],'.lm.model5.csv'),row.names=TRUE)
write.csv(aov.modle.5,paste0(workingpath,'/ModelTable/',dv[i],'.aov.model5.csv'),row.names=TRUE)

###Reduction.model.anova: model5 as full model---------------- idv.2,idv.2,idv.2,
t=1
R.model.anova = data.frame(matrix(ncol =6,nrow= 0))
for (t in 1:length(c(idv,inter.idv))) {
  onedrop.model.f = c(idv,inter.idv)[-t] %>% paste(collapse = '+') %>% paste(dv[i],'~',.)
  onedrop.model = (lm(onedrop.model.f,data = work.data))
  R.model.anova = rbind(R.model.anova,anova(onedrop.model,multi.lm,test = 'F')[2,])
}
R.model.anova$Var. = c(idv,inter.idv)
write.csv(R.model.anova,paste0(workingpath,'/ModelTable/R.model.anova.csv'),row.names=TRUE)
# full model anova
full.model.anova = anova(multi.lm)
write.csv(full.model.anova,paste0(workingpath,'/ModelTable/full.model.anova.csv'),row.names=TRUE)


f.data = select_if(res.data,is.factor) %>% .[,-2]
f.data$Rh_ratio = res.data$Rh_ratio
fac = colnames(f.data) %>% paste(.,collapse = '+')

other = names(summary(f.data$Ecosystem_type))[6:9]
f.data$Ecosystem_type[which(f.data$Ecosystem_type %in% other)] = 'Others'
f.data = droplevels(f.data)
table(f.data$Partition_method)
factor.aov = aov(Rh_ratio~fac,data = f.data)
factor.aov = aov(res.data$Rh_ratio~Leaf_habit+Biome+Ecosystem_type+Soil_type+Species+Partition_method,data = f.data)
summary(factor.aov)
summary(f.data$Biome)

###formula idv.part2,'+',idv.part2,'+',,C.idv.part,'+'
for (i in 1:4) {
  Eq = as.formula(paste0(dv[i],'~',idv.part1,'+',inter.idv.part))
  multi.lm = step(lm(Eq,data = work.data),direction = 'backward')
  summary(multi.lm)
  cof.lm = lm.cof(df = work.data,y = dv[i],x = paste(idv.part1,'+',inter.idv.part))
  write.csv(cof.lm,paste0(workingpath,'/ModelTable/Multi.lm.',dv[i],'.csv'),row.names=TRUE)
  aov.lm = anova(multi.lm)
  write.csv(aov.lm,paste0(workingpath,'/ModelTable/Anova.',dv[i],'.csv'),row.names=TRUE)
}


###select the independant variables and dependant variables--------
colnames(res.data)
idv <- colnames(res.data)[c(24:26,29:35,38:41,44:49,2,7,14,15,11,12,3,19)]
idv.ul <- paste0('I(',idv[1:24],'^2)')
dv <- colnames(res.data)[20:23]

###linear，unlinear and polynominal model for variables---------------
i=4;t=1
for (i in 1:length(dv)) {
  lmI.result = data.frame()
  ulmI.result= data.frame()
  plmI.result= data.frame()
  for (t in 1:length(idv.ul)) {
    lmI = lm.cof(df = res.data,y = dv[i],x = idv[t])
    ulmI= lm.cof(df = res.data,y = dv[i],x = idv.ul[t])
    plmI= lm.cof(df = res.data,y = dv[i],x = paste(idv[t],'+',idv.ul[t]))[2,]
    lmI.result = rbind(lmI.result,lmI)
    ulmI.result= rbind(ulmI.result,ulmI)
    plmI.result= rbind(plmI.result,plmI)
  }
  write.csv(lmI.result,paste0(workingpath,"/ModelTable/lm.I.result.",dv[i],'.csv'),row.names = TRUE)
  write.csv(ulmI.result,paste0(workingpath,"/ModelTable/Unlm.I.result.",dv[i],'.csv'),row.names = TRUE)
  write.csv(plmI.result,paste0(workingpath,"/ModelTable/Polylm.I.result.",dv[i],'.csv'),row.names = TRUE)
}
rm(lmI,lmI.result,ulmI.result,ulmI,plmI,plmI.result)
rm(i,t)
