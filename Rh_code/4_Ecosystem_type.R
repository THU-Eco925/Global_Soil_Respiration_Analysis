source('0_plot.R')

rm(labelLat,labelYear,labelElev)
###-----------------------------------


# 各类图的theme参数方便随时调用
PTs <- theme(text = element_text(size=10),legend.position = "none",
             axis.text.x = element_text(size=8, hjust = 0.5,vjust = 0.5),
             panel.background=element_blank(),
             panel.border = element_rect(colour = "grey", fill=NA, size=1))
# Temfilmanual<-scale_fill_manual(values=c("steelblue1", "steelblue3", "steelblue4"))
#TemColmanual<-scale_colour_manual(values=c("steelblue1", "steelblue3", "steelblue4"))
#Latfilmanual<-scale_fill_manual(values=c("steelblue1", "steelblue3", "steelblue4"))
#LatColmanual<-scale_colour_manual(values=c("steelblue1", "steelblue3", "steelblue4"))
#----------------------------------------------以上代码需要提前运行
### specific for inercomparision(lat&year&global warming)

dir.create(paste0(workingpath,"/polish")) 
setwd(paste0(workingpath,"/polish")) 

var1 <- c('Study_midyear','MAT','MAP','ΔMean.Temp.','ΔMean.Precip.')
i=1
tar <- c("Rs_annual", "Ra_annual","Rh_annual","Rh_ratio")
t = 3

DF = rgb(106,103,203,maxColorValue = 255)
EF = rgb(12,224,171,maxColorValue = 255)
MF = rgb(252,99,107,maxColorValue = 255)
GL = rgb(255,185,0,maxColorValue = 255)
OT = rgb(122,182,245,maxColorValue = 255)
eco.color = c(DF,EF,MF,GL,OT)

    plotdata = select(res.data,Ecosystem_type,tar[t],var1[i])
    plotdata = subset(plotdata, plotdata[,1] != 'NA'&complete.cases(plotdata[,2:3]))
    plotdata[which(plotdata$Ecosystem_type == 'Forest'),1] = 'Others' 
    plotdata$Ecosystem_type = droplevels(plotdata$Ecosystem_type)
    plotdata$Ecosystem_type= factor(plotdata$Ecosystem_type,
                                    levels=c("Deciduous Forest","Evergreen Forest","Mixed Forest","Grassland","Others"))
    #regression coefficients


    PBg <- ggplot(plotdata, aes_string(var1[i], tar[t]))
    Ps <-geom_point(color = 'black',
                    alpha = 0.5,stroke = 0.5,
                    size=3,shape=21,
                    aes_string(fill=plotdata[,1]))
    Innerlmplot   <-geom_smooth(method='lm',aes(color = plotdata[,1]),alpha = 0.1)
    axislimit<-checkborder(plotdata)
    ###---------------------------------------
    ###--------------------------------------------------------
    #准备保存到不同的文件夹,并判断是否展示outlier + eqn1_text+eqn2_text'eqstring2'
    scm <- scale_colour_manual(values=eco.color)
    sfm <- scale_fill_manual(values=eco.color)
    pointplot <- PBg+Ps+Innerlmplot+PTs+scm+sfm
    #pointplot <- pointplot  + eqn1_text+eqn2_text+facet_grid(.~Elev.Area)+axislimit 
    savepath=paste0(workingpath,"/polish/")
    ggsave(path=savepath,
           plot=pointplot,
           filename=paste0(tar[t],"+",var1[i],'+','Elev.Area','.png'),
           width=3.3, dpi = 300,height=3.3)
    

    
#calculate their regression coefficients
    factorselect = names(summary(plotdata[,1]))
    eqstring1=c()
    eqstring2=c()
    for (q in 1:length(factorselect)){
      lmsubdata =  filter(plotdata,plotdata[,1] == factorselect[q])
      eq= lm_eqn(lmsubdata)
      if (eq[3] >= 0.001) {
        eqstring1[q]=paste0('Slope=',
                            format(round(as.numeric(eq[1]), 3), nsmall = 3),
                            " P=",
                            format(round(as.numeric(eq[3]), 3), nsmall = 3))}
      else{eqstring1[q]=paste0('Slope=',
                               format(round(as.numeric(eq[1]), 3), nsmall = 3),
                               " P<0.001")}}

