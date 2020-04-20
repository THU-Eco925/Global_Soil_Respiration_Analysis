###---------------------------------------
### 1.save the climatic data in your project path
### 2.read the res.data
###------------------------------------------
library(dplyr)

res.data <- read.csv(choose.files(),check.names=FALSE) 
TEMPpath <- "/air_temp_2017/"
PRECIPpath <- "/precip_2017/"

###--------------------------------------------------------------------
#temp & precip extraction
#note: the index is the same for both temp. and precip. data
yearsrange = c(floor(min(res.data$Study_midyear)-1):2017)
tempdata = read.table(paste0(getwd(),TEMPpath,'air_temp.',yearsrange[1]))
prepdata = read.table(paste0(getwd(),PRECIPpath,'precip.',yearsrange[1]))
tempdata$Site = paste(tempdata$V2,tempdata$V1)
prepdata$Site = paste(prepdata$V2,prepdata$V1)
index = which(tempdata$Site%in%res.data$Site)
tempdata.list = tempdata[index,'Site']
prepdata.list = prepdata[index,'Site']

# nearest neighbour


for (i in 1:length(yearsrange)){
  tempdata = read.table(paste0(getwd(),TEMPpath,'air_temp.',yearsrange[i])) %>% .[index,]
  prepdata = read.table(paste0(getwd(),PRECIPpath,'precip.',yearsrange[i])) %>% .[index,]
  colnames(prepdata)[3:14] =colnames(tempdata)[3:14] = c(paste0(rep(yearsrange[i],times=12),'_',c(1:12)))
  tempdata.list = cbind(tempdata.list,tempdata[,c(3:14)])
  prepdata.list = cbind(prepdata.list,prepdata[,c(3:14)])  
}
colnames(tempdata.list)[1] = colnames(prepdata.list)[1] = 'Site'

write.csv(tempdata.list,"temp_extract_result.csv",row.names=FALSE)
write.csv(prepdata.list,"precip_extract_result.csv",row.names=FALSE)

#######Temp & Precip--------------------------------------------& YearsOfData %in% c(1:4)
attach(res.data)

Mean.Temp. <-  Mean.Temp.30y  <- c()
Mean.Precip. <- Mean.Precip.30y  <- c()
Record_num = c()

# check the start position\end position\row position
for (i in 1:nrow(res.data)){
  start.p = (Study_midyear[i]-1986)*12-round(12*YearsOfData[i]/2)+2
  end.p = (Study_midyear[i]-1986)*12+round(12*YearsOfData[i]/2)+1
  row.p = which(tempdata.list$Site == as.character(res.data$Site[i]))
  Record_num [i] = Record_number [i]
  #temp
  temp.list = as.numeric(tempdata.list[row.p,c(start.p:end.p)])
  Mean.Temp.[i] = mean (temp.list)
  Mean.Temp.30y[i]=mean(as.numeric(tempdata.list[row.p,-1]))
  #precip
  precip.list = as.numeric(prepdata.list[row.p,c(start.p:end.p)])
  Mean.Precip.[i] = sum (precip.list)/(length(temp.list)/12)
  Mean.Precip.30y[i]=mean(as.numeric(prepdata.list[row.p,-1]))*12
  }

res.data$Mean.Temp.= Mean.Temp.
res.data$Mean.Precip.= Mean.Precip.
res.data$Delta.Mean.Temp.= Mean.Temp. - Mean.Temp.30y
res.data$Delta.Mean.Precip.= Mean.Precip. - Mean.Precip.30y

detach(res.data)

write.csv(res.data,"Processed_data_0414.csv",row.names=FALSE)




