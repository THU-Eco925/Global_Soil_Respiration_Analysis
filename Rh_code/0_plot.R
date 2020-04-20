library('ggplot2')
library('RColorBrewer')
library('dplyr') #if funciton masked, please run it again
library('magrittr')
###-----------------------------------
#label the lat,year,and SPEI
labelLat <- function(x){
  if (abs(x) <= 30){x = "Low latitude"} 
  else if (abs(x) <= 50){x = "Middle latitude"} 
  else {x = "High latitude"}
  return(x)}
labelElev <- function(x){
  if (abs(x) <= 500){x = "0-500"} 
  else if (abs(x) <= 1000){x = "500-1000"} 
  else {x = "1000-"}
  return(x)}
labelYear <- function(x){
  if (x <= 1996){x = "1987-1996"} 
  else if (x <= 2005){x = "1997-2005"} 
  else {x = "2006-2015"}
  return(x)}

###-------------------------------
checkborder <- function(df){
  if (colnames(df)[2] == "Rh_ratio"){
    limitcor <- coord_cartesian()
  } else if(colnames(df)[2] == "Rs_annual"){
    limitcor <- coord_cartesian(ylim=c(sort(df[,2])[nrow(df)-2]:sort(df[,2])[2]))
  }else {
    limitcor <- coord_cartesian(ylim=c(sort(df[,2])[nrow(df)-1]:sort(df[,2])[2]))
  }
  return(limitcor)
}
ymax <- function(df){
  if (colnames(df)[2] == "Rh_ratio"){
    a=max(df[,2])
  } else if(colnames(df)[2] == "Rs_annual"){
    a=sort(df[,2])[nrow(df)-2]
  }else {
    a=sort(df[,2])[nrow(df)-1]
  }
  return(a)
}
###annote equation function in plot
lm_eqn <- function(df){
  m <- lm(df[,2] ~ df[,3], df);
  eq=c(a = format(coef(m)[2], digits = 6),
       r2 = format(summary(m)$r.squared, digits = 6),
       p = format(summary(m)$coefficients[2,4],digits = 6))
  #eqstring1[q]=paste0("S:",eq[1]," R^2:",eq[2])
  #eqstring2[q]=paste0("P:",eq[3]," c=",nrow(df))  
  return(eq)
}

###-----------------------------------
workingpath = getwd()
res.data <- read.csv(choose.files(),check.names=FALSE)  
res.data = filter(res.data, 
                  Meas_method %in% c('Gas Chromatography','IRGA')&
                    Ecosystem_type != 'Agriculture'
                  & !is.na(SOC_015)
                  & !is.na(Mean.Temp.))

res.data$Lat.Area = as.character(lapply(res.data$Latitude,labelLat)) %>% as.factor()
res.data$Lat.Area = factor(res.data$Lat.Area, levels=c("Low latitude","Middle latitude","High latitude"))
res.data$Timerange = as.character(lapply(res.data$Study_midyear,labelYear)) %>% as.factor()

