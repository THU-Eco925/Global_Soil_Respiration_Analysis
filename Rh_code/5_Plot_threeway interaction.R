library('ggplot2')
library('dplyr') #if funciton masked, please run it again
library('magrittr')
setwd("D:/Global_Tem_SPEI/SRDB_V4_CHN")
workingpath = getwd()
res.data <- read.csv("Processed_data_0830.csv",check.names=FALSE) 

###-----------------------------------
#label the lat,year,and SPEI
labelLat <- function(x){
  if (abs(x) <= 30){x = "Low latitude"} 
  else if (abs(x) <= 50){x = "Middle latitude"} 
  else {x = "High latitude"}
  return(x)}
labelYear <- function(x){
  if (x <= 1996){x = "1987-1996"} 
  else if (x <= 2006){x = "1997-2006"} 
  else {x = "2007-2015"}
  return(x)}
labelElev <- function(x){
  if (abs(x) <= 500){x = "0-500"} 
  else if (abs(x) <= 1000){x = "500-1000"} 
  else {x = "1000-"}
  return(x)}

#------------------
checkborder <- function(df){
if (colnames(df)[3] == "Rh_ratio"){
  limitcor <- coord_cartesian()
} else if(colnames(df)[3] == "Rs_annual"){
  limitcor <- coord_cartesian(ylim=c(sort(df[,3])[nrow(df)-6]:sort(df[,3])[6]))
}else {
  limitcor <- coord_cartesian(ylim=c(sort(df[,3])[nrow(df)-1]:sort(df[,3])[2]))
}
  return(limitcor)
}
ymax <- function(df){
  if (colnames(df)[3] == "Rh_ratio"){
    a=max(df[,3])
  } else if(colnames(df)[3] == "Rs_annual"){
    a=sort(df[,3])[nrow(df)-6]
  }else {
    a=sort(df[,3])[nrow(df)-1]
  }
  return(a)
}
###annote equation function in plot
lm_eqn <- function(df){
  if (nrow(df)>5) {
    m <- lm(df[,2] ~ df[,3], df);
    eq=c(a = format(coef(m)[2], digits = 6),
         r2 = format(summary(m)$r.squared, digits = 6),
         p = format(summary(m)$coefficients[2,4],digits = 6))
  } else {eq =c()}
  return(eq)
}
###-----------------------------------
deletesinglton <- function(x) {# x为输入的数据???
  stand.col <- x[, 1] # 设根据x的第一列进行删除操???
  count <- table(stand.col)
  if (all(count < 2)) stop("no repeated records")
  else {
    ind <- sapply(stand.col, function(t) ifelse(count[as.character(t)] > 1, TRUE, FALSE))
  }
  return(x[ind, ])
}
###-----------------------------------
#先添加各种lable factor(纬度，温度，年代)
res.data$Lat.Area = as.character(lapply(res.data$Latitude,labelLat))%>% as.factor()
res.data$Timerange = as.character(lapply(res.data$Study_midyear,labelYear))%>% as.factor()
res.data$Elev.Area = as.character(lapply(res.data$Elevation,labelElev))%>% as.factor()

rm(Lat.Area,Timerange,labelLat,labelYear)
###-----------------------------------
###提取各种变量名字方便批量出图
#objlist  <- colnames(res.data)

#deltatemp
  res.data = arrange(res.data,ΔMean.Temp.)
  res.data$deltatemp = 'Cooling'
  res.data$deltatemp[round(nrow(res.data)/3):round(nrow(res.data)*2/3)] = 'Moderate'
  res.data$deltatemp[round(nrow(res.data)*2/3):nrow(res.data)] = 'Warming'
  res.data$deltatemp=factor(res.data$deltatemp,levels=c("Cooling","Moderate","Warming"))
#deltaprecip.
  res.data = arrange(res.data,ΔMean.Precip.)
  res.data$deltaprecip = 'Dry'
  res.data$deltaprecip[round(nrow(res.data)/3):round(nrow(res.data)*2/3)] = 'Normal'
  res.data$deltaprecip[round(nrow(res.data)*2/3):nrow(res.data)] = 'Wet'
  res.data$deltaprecip=factor(res.data$deltaprecip,levels=c("Dry","Normal","Wet"))
#tempzone
  res.data = arrange(res.data,MAT)
  res.data$tempzone = 'Cold_zone'
  res.data$tempzone[round(nrow(res.data)/3):round(nrow(res.data)*2/3)] = 'Temperate_zone'
  res.data$tempzone[round(nrow(res.data)*2/3):nrow(res.data)] = 'Hot_zone'
  res.data$tempzone=factor(res.data$tempzone,levels=c("Cold_zone","Temperate_zone","Hot_zone"))
#precip.
  res.data = arrange(res.data,MAP)
  res.data$precipzone = 'Low_precipitation'
  res.data$precipzone[round(nrow(res.data)/3):round(nrow(res.data)*2/3)] = 'Medium_precipitation'
  res.data$precipzone[round(nrow(res.data)*2/3):nrow(res.data)] = 'High_precipitation'
  res.data$precipzone=factor(res.data$precipzone,levels=c("Low_precipitation","Medium_precipitation","High_precipitation"))
###------------构建基本不变的全局绘图参数----
  res.data = filter(res.data, Rh_ratio >= 0.25 & 
                      Meas_method %in% c('Gas Chromatography','IRGA')&
                      Ecosystem_type != 'Agriculture'
                    & !is.na(SOC_015)
                    & !is.na(ΔMean.Temp.)
                    & Record_number <10000)

#----------------------------------------------以上代码需要提前运???
### specific for inercomparision(lat&year&global warming)

dir.create(paste0(workingpath,"/polish")) 
setwd(paste0(workingpath,"/polish"))
res.data$Lat.Area = factor(res.data$Lat.Area, levels=c("Low latitude","Middle latitude","High latitude"))

t=3
i=5
p=2
q=1
tar <- c("Rs_annual", "Ra_annual","Rh_annual","Rh_ratio")
var1 <- c('Study_midyear','MAT','MAP','ΔMean.Temp.','ΔMean.Precip.')
cate<- c('Timerange',"Lat.Area",'deltatemp','deltaprecip','tempzone','precipzone','Elev.Area')
cate2<- c('Timerange',"Lat.Area",'deltatemp','deltaprecip','tempzone','precipzone','Elev.Area')
for(t in 1:length(tar)){
  for(i in 1:length(var1)){
    for(p in 1:length(cate)){
    for(q in 1:length(cate2)){

    plotdata = select(res.data,cate[p],tar[t],var1[i],cate2[q]) %>% na.omit(.)
    colnames(plotdata)[1]='X1'
    colnames(plotdata)[4]='X4'
    plotdata$X14 = paste0(plotdata$X1,'_',plotdata$X4)%>% factor(.,
                    levels=c('Low latitude_1987-1996','Low latitude_1997-2006','Low latitude_2007-2015',
                             'Middle latitude_1987-1996','Middle latitude_1997-2006','Middle latitude_2007-2015',
                             'High latitude_1987-1996','High latitude_1997-2006','High latitude_2007-2015'))
    
 #   plotdata = deletesinglton(plotdata) #factor is the first column
  #图层的设???
    PointBgset <- ggplot(plotdata, aes_string(var1[i], tar[t]))
    
    ### lm plot," n=",nrow(lmsubdata)
    TS <- theme(text = element_text(size=10),legend.position = "none",
                      axis.text.x = element_text(size=8, hjust = 0.5,vjust = 0.5),
                      panel.background=element_blank(),
                      panel.border = element_rect(colour = "grey", fill=NA, size=1))
    factorselect1 = levels(plotdata[,1])
    factorselect2 = levels(plotdata[,4])

    m=2
    n=3
    cm   <- c('brown2',"brown4", "brown4",
            'darkseagreen1',"darkolivegreen3", "darkgreen",
            'darkturquoise',"royalblue2", "royalblue4")#'coral1',
    fm <- c('lightpink',"indianred1", "indianred3",
            'darkseagreen1',"darkolivegreen3", "darkgreen",
            'aliceblue',"cadetblue3", "dodgerblue4")
    ssm <- scale_shape_manual(values=c(25,23, 21))
    savepath=getwd()
    for (m in 1:length(factorselect1)){
      m=3
      rcdata = plotdata[which(plotdata$X1==factorselect1[m]),]
      scm = scale_colour_manual(values = cm[(3*m-2):(3*m)])
      sfm = scale_fill_manual(values = fm[(3*m-2):(3*m)])
      #for (n in 1:length(factorselect2)) {
      
      print(seq(
        floor(min(rcdata[,3])),1.1*ceiling(max(rcdata[,3])),
        by = (0.2*(ceiling(max(rcdata[,3]))-floor(min(rcdata[,3]))))
        ))
      
      print(seq(
        floor(min(rcdata[,2])),1.1*ceiling(max(rcdata[,2])),
        by = (0.2*(ceiling(max(rcdata[,2]))-floor(min(rcdata[,2]))))
        ))
      sx =scale_x_continuous(breaks = c(-3,0,3,6), limits = c(-4, 6))
      sy =scale_y_continuous(breaks = c(700,1400,2100,2800), limits = c(0, 2900))
      sy =scale_y_continuous(breaks = c(300,600,900,1200), limits = c(0, 1300))
      sy =scale_y_continuous(breaks = c(250,500,750,1000), limits = c(0, 1100))

      PBg <- ggplot(rcdata, aes_string(x = var1[i],y = 'Rh_annual'))
      PS <-geom_point(color = 'black',#,color=Latitude
                        alpha = 0.5,stroke = 0.2,
                        size=1.5,aes(fill = X4,shape=X4))
      LM   <-geom_smooth(method='lm',alpha = 0,aes(color = X4))
      pointplot <-PBg + PS +TS +LM+ssm+scm+sfm+sx+sy
      ggsave(plot=pointplot,path=savepath,dpi = 200,
            filename=paste(factorselect1[m],'+',var1[i],'.png'),width=2.2,height = 2.2)
    }
    eqstring1=c()
    eqstring2=c()
    c=3
    tdata = plotdata[which(plotdata$X1==factorselect1[c]),]
    for (m in 1:length(factorselect2)){
        lmsubdata=filter(tdata,tdata[,4] == factorselect2[m])
        eq= lm_eqn(lmsubdata)
        eqstring1=c(eqstring1,paste0("S:",eq[1]))#," P:",eq[3]
        eqstring2=c(eqstring2,paste0("S:",eq[1]," R^2:",eq[2],"P:",eq[3]," c=",nrow(lmsubdata)))
      }
    }

    
    axislimit<-checkborder(plotdata)
    
    ###----------------------------------------
    ###---------------------从这里开始替换（用geom_text替换anotate???
    #公式数据???
    eq1 <- data.frame(
      eq1   = eqstring1,
      eq2   = eqstring2,
      X1 = rep(factorselect1,each=3)
    )
    eq1$eq1 =as.character(eq1[,1])
    eq1[1,1]=('S:NA P:NA')
    eqn1_text <-geom_text(  data = eq1,size = 8,
                            mapping = aes(x=min(plotdata[,2]), 
                                          y=rep(c(ymax(plotdata),0.9*ymax(plotdata),0.8*ymax(plotdata)),3), label = eq1),
                            hjust=0)
    eqn2_text <-geom_text(  data = eq1,size = 8,
                            mapping = aes(x=min(plotdata[,2]), 
                                          y=ymax(plotdata), label = eq2),
                            hjust=0)
    ###--------------------------------一直替换到这里
    ###--------------------------------------------------------,shape=plotdata[,1]+guides(color=FALSE)
    #准备保存到不同的文件???,并判断是否展示outlier + eqn1_text +eqn2_text0.9*ymax(plotdata)
    # rep(c(ymax(plotdata),0.9*ymax(plotdata),0.8*ymax(plotdata)),3)+ eqn1_text+axislimit+scm+sfm
    
    scm <- scale_colour_manual(values=c("brown2", "brown4",
                                        'darkseagreen1',"darkolivegreen3", "darkgreen",
                                        'darkturquoise',"royalblue2", "royalblue4"))#'coral1',
    sfm <- scale_fill_manual(values=c('lightpink',"indianred1", "indianred3",
                                      'darkseagreen1',"darkolivegreen3", "darkgreen",
                                      'aliceblue',"cadetblue3", "dodgerblue4"))
    ssm <- scale_shape_manual(values=c(25,23, 21))
    PointThemeset <- theme(text = element_text(size=10),legend.position = "none",
                           axis.text.x = element_text(size=8, hjust = 0.5,vjust = 0.5),
                           panel.background=element_blank(),
                           panel.border = element_rect(colour = "grey", fill=NA, size=1))
    Pointset <-geom_point(color='black',alpha = 0.4,stroke = 0.1,
                          size=2, aes_string(fill=plotdata[,5],shape=plotdata[,4]))
    Innerlmplot   <-geom_smooth(method='lm',aes(color = plotdata[,5]),alpha=0,size =0.7)
    pointplot <- (PointBgset + Pointset + 
                    Innerlmplot +PointThemeset+ssm+scm+sfm
                     +facet_grid(.~X1) #fill=plotdata[,1],,shape=plotdata[,4]+sfm+scm
                   )
    eq1$eq1
    savepath=paste0(getwd())
    ggsave(path=savepath,  
           plot=pointplot,
           filename=paste0(tar[t],"+",var1[i],'+',cate[p],'+',cate2[q],'.png'),
           width=6.7, dpi = 300,height=3.3)      
    }
    }}}}
 eq1$eq1 =as.character(eq1[,1])
eq1[1,1]=('S:NA P:NA n=3')
 rm(plotdata)
 
 
####--------- Lat.Area
for(t in 1:length(tar)){
  for(i in 1:length(var1)){
      plotdata = select(res.data,Lat.Area,var1[i],tar[t])
      plotdata = subset(plotdata, plotdata[,1] != 'NA'&complete.cases(plotdata[,2:3]))
      plotdata = deletesinglton(plotdata) #factor is the first column
      #regression coefficients
      factorselect = names(summary(plotdata[,1]))
      eqstring1=c()
      eqstring2=c()
      for (q in 1:length(factorselect)){
        lmsubdata =  filter(plotdata,plotdata[,1] == factorselect[q])
        eq= lm_eqn(lmsubdata)
        eqstring1[q]=paste0("S:",eq[1]," R^2:",eq[2])
        eqstring2[q]=paste0("P:",eq[3]," c=",nrow(lmsubdata))
      }
      #图层的设???
      PointBgset <- ggplot(plotdata, aes_string(var1[i], tar[t]))
      Pointset <-geom_point(color = 'black',
                            alpha = 0.5,stroke = 1.5,
                            size=3,shape=21,
                            aes_string(fill=plotdata[,1]))
      Innerlmplot   <-geom_smooth(method='lm',aes(color = plotdata[,1]))
      axislimit<-checkborder(plotdata)
      eqn1_text <-annotate("text", label = eqstring1,size = 8, 
                           x=min(plotdata[,2]), 
                           y=0.9*ymax(plotdata),
                           hjust=0)
      eqn2_text <-annotate("text", label = eqstring2,size = 8, 
                           x=min(plotdata[,2]), 
                           y=ymax(plotdata),
                           hjust=0)
      #准备保存到不同的文件???,并判断是否展示outlier
      pointplot <- (PointBgset + Pointset + 
                      Innerlmplot +PointThemeset+
                      axislimit+ eqn1_text +eqn2_text +
                      facet_grid(.~Lat.Area))
      savepath=paste0(workingpath,"/Inercomparision/",tar[t])
      dir.create(savepath)
      ggsave(path=savepath,
             plot=pointplot,
             filename=paste0(tar[t],"+",var1[i],'+','Lat.Area','.png'),
             width=15, dpi = 100,height=5)
    }}
rm(plotdata,i,t)

###
eq= lm_eqn(plotdata)
eq= paste0('S=',eq[1],' R2=',eq[2],' P=',eq[3])
eqn1_text <-annotate("text", label = eq,size = 8, 
                     x=min(plotdata[,2]), 
                     y=0.9*ymax(plotdata),
                     hjust=0)

####--------- Aridty_Degree
for(t in 1:length(tar)){
  for(i in 1:length(var1)){
    plotdata = select(res.data,Aridty_Degree,var1[i],tar[t])
    plotdata = subset(plotdata, plotdata[,1] != 'NA'&complete.cases(plotdata[,2:3]))
    plotdata = deletesinglton(plotdata) #factor is the first column
    plotdata$Aridty_Degree <- factor(plotdata$Aridty_Degree,
                                     levels=c("dry","semi-dry","balance","semi-wet","wet"))
    #regression coefficients
    factorselect = names(summary(plotdata[,1]))
    eqstring1=c()
    eqstring2=c()
    for (q in 1:length(factorselect)){
      lmsubdata =  filter(plotdata,plotdata[,1] == factorselect[q])
      eq= lm_eqn(lmsubdata)
      eqstring1[q]=paste0("S:",eq[1]," R^2:",eq[2])
      eqstring2[q]=paste0("P:",eq[3]," c=",nrow(lmsubdata))
    }
    #图层的设???
    PointBgset <- ggplot(plotdata, aes_string(var1[i], tar[t]))
    Pointset <-geom_point(color = 'black',
                          alpha = 0.5,stroke = 1.5,
                          size=3,shape=21,
                          aes_string(fill=plotdata[,1]))
    Innerlmplot   <-geom_smooth(method='lm',aes(color = plotdata[,1]))
    axislimit<-checkborder(plotdata)
    eqn1_text <-annotate("text", label = eqstring1,size = 8, 
                         x=min(plotdata[,2]), 
                         y=0.9*ymax(plotdata),
                         hjust=0)
    eqn2_text <-annotate("text", label = eqstring2,size = 8, 
                         x=min(plotdata[,2]), 
                         y=ymax(plotdata),
                         hjust=0)
    #准备保存到不同的文件???,并判断是否展示outlier
    pointplot <- (PointBgset + Pointset + 
                    Innerlmplot +PointThemeset+
                    axislimit+ eqn1_text +eqn2_text +
                    facet_grid(.~Aridty_Degree))
    savepath=paste0(workingpath,"/Inercomparision/",tar[t])
    dir.create(savepath)
    ggsave(path=savepath,
           plot=pointplot,
           filename=paste0(tar[t],"+",var1[i],'+','Aridty_Degree','.png'),
           width=15, dpi = 100,height=5)
  }}
rm(plotdata,i,t)
####--------- Timerange
for(t in 1:length(tar)){
  for(i in 1:length(var1)){
    plotdata = select(res.data,Timerange,var1[i],tar[t])
    plotdata = subset(plotdata, plotdata[,1] != 'NA'&complete.cases(plotdata[,2:3]))
    plotdata = deletesinglton(plotdata) #factor is the first column
    #regression coefficients
    factorselect = names(summary(plotdata[,1]))
    eqstring1=c()
    eqstring2=c()
    for (q in 1:length(factorselect)){
      lmsubdata =  filter(plotdata,plotdata[,1] == factorselect[q])
      eq= lm_eqn(lmsubdata)
      eqstring1[q]=paste0("S:",eq[1]," R^2:",eq[2])
      eqstring2[q]=paste0("P:",eq[3]," c=",nrow(lmsubdata))
    }
    #图层的设???
    PointBgset <- ggplot(plotdata, aes_string(var1[i], tar[t]))
    Pointset <-geom_point(color = 'black',
                          alpha = 0.5,stroke = 1.5,
                          size=3,shape=21,
                          aes_string(fill=plotdata[,1]))
    Innerlmplot   <-geom_smooth(method='lm',aes(color = plotdata[,1]))
    axislimit<-checkborder(plotdata)
    eqn1_text <-annotate("text", label = eqstring1,size = 8, 
                         x=min(plotdata[,2]), 
                         y=0.9*ymax(plotdata),
                         hjust=0)
    eqn2_text <-annotate("text", label = eqstring2,size = 8, 
                         x=min(plotdata[,2]), 
                         y=ymax(plotdata),
                         hjust=0)
    #准备保存到不同的文件???,并判断是否展示outlier
    pointplot <- (PointBgset + Pointset + 
                    Innerlmplot +PointThemeset+
                    axislimit+ eqn1_text +eqn2_text +
                    facet_grid(.~Timerange))
    savepath=paste0(workingpath,"/Inercomparision/",tar[t])
    dir.create(savepath)
    ggsave(path=savepath,
           plot=pointplot,
           filename=paste0(tar[t],"+",var1[i],'+','Timerange','.png'),
           width=15, dpi = 100,height=5)
  }}
rm(plotdata,i,t)
####--------- Ecosystem_type
for(t in 1:length(tar)){
  for(i in 1:length(var1)){
    plotdata = select(res.data,Ecosystem_type,var1[i],tar[t])
    plotdata = subset(plotdata, plotdata[,1] != 'NA'&complete.cases(plotdata[,2:3]))
    plotdata = deletesinglton(plotdata) #factor is the first column
    #regression coefficients
    # remove the level without observation
    factorselect = names(summary(droplevels(plotdata$Ecosystem_type)))
    eqstring1=c()
    eqstring2=c()
    for (q in 1:length(factorselect)){
      lmsubdata =  filter(plotdata,plotdata[,1] == factorselect[q])
      eq= lm_eqn(lmsubdata)
      eqstring1[q]=paste0("S:",eq[1]," R^2:",eq[2])
      eqstring2[q]=paste0("P:",eq[3]," c=",nrow(lmsubdata))
    }
    #图层的设???
    PointBgset <- ggplot(plotdata, aes_string(var1[i], tar[t]))
    Pointset <-geom_point(color = 'black',
                          alpha = 0.5,stroke = 1.5,
                          size=3,shape=21,
                          aes_string(fill=plotdata[,1]))
    Innerlmplot   <-geom_smooth(method='lm',aes(color = plotdata[,1]))
    axislimit<-checkborder(plotdata)
    eqn1_text <-annotate("text", label = eqstring1,size = 5.5, 
                         x=min(plotdata[,2]), 
                         y=0.9*ymax(plotdata),
                         hjust=0)
    eqn2_text <-annotate("text", label = eqstring2,size = 5.5, 
                         x=min(plotdata[,2]), 
                         y=ymax(plotdata),
                         hjust=0) 
    #准备保存到不同的文件???,并判断是否展示outlier
    pointplot <- (PointBgset + Pointset + 
                    Innerlmplot +PointThemeset+
                    axislimit+ eqn1_text +eqn2_text +
                    facet_grid(.~Ecosystem_type))
    savepath=paste0(workingpath,"/Inercomparision/",tar[t])
    dir.create(savepath)
    ggsave(path=savepath,
           plot=pointplot,
           filename=paste0(tar[t],"+",var1[i],'+','Ecosystem_type','.png'),
           width=20, dpi = 100,height=5)
  }}
rm(plotdata,i,t)
####--------- Biome
for(t in 1:length(tar)){
  for(i in 1:length(var1)){
    plotdata = select(res.data,Biome,var1[i],tar[t])
    plotdata = subset(plotdata, plotdata[,1] != 'NA'&complete.cases(plotdata[,2:3]))
    plotdata = deletesinglton(plotdata) #factor is the first column
    #regression coefficients
    # remove the level without observation
    factorselect = names(summary(droplevels(plotdata$Biome)))
    eqstring1=c()
    eqstring2=c()
    for (q in 1:length(factorselect)){
      lmsubdata =  filter(plotdata,plotdata[,1] == factorselect[q])
      eq= lm_eqn(lmsubdata)
      eqstring1[q]=paste0("S:",eq[1]," R^2:",eq[2])
      eqstring2[q]=paste0("P:",eq[3]," c=",nrow(lmsubdata))
    }
    #图层的设???
    PointBgset <- ggplot(plotdata, aes_string(var1[i], tar[t]))
    Pointset <-geom_point(color = 'black',
                          alpha = 0.5,stroke = 1.5,
                          size=3,shape=21,
                          aes_string(fill=plotdata[,1]))
    Innerlmplot   <-geom_smooth(method='lm',aes(color = plotdata[,1]))
    axislimit<-checkborder(plotdata)
    eqn1_text <-annotate("text", label = eqstring1,size = 5.5, 
                         x=min(plotdata[,2]), 
                         y=0.9*ymax(plotdata),
                         hjust=0)
    eqn2_text <-annotate("text", label = eqstring2,size = 5.5, 
                         x=min(plotdata[,2]), 
                         y=ymax(plotdata),
                         hjust=0)
    #准备保存到不同的文件???,并判断是否展示outlier
    pointplot <- (PointBgset + Pointset + 
                    Innerlmplot +PointThemeset+
                    axislimit+ eqn1_text +eqn2_text +
                    facet_grid(.~Biome))
    savepath=paste0(workingpath,"/Inercomparision/",tar[t])
    dir.create(savepath)
    ggsave(path=savepath,
           plot=pointplot,
           filename=paste0(tar[t],"+",var1[i],'+','Biome','.png'),
           width=20, dpi = 100,height=5)
  }}
rm(plotdata,i,t)