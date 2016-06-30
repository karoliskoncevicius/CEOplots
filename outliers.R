### Plot intersample correlations and indicate outliers
# INPUT
# 5 vectors of equal length
# vals - mean correlation values
# set - name of set/lot/tissue/age etc.
# isoutlier - is less than mean-3sd
# ID - unique ID for sample
# lab - ZT/mouse ID

source("ggtheme.R")
source("colors.R")

plotOutliers <- function(cors,set,isoutlier,setID,ztlabel){
  
  plotdf <- data.frame("vals"=cors,
                       "Set"=set,
                       "lt3sd"=isoutlier,
                       "ID"=setID,
                       "lab"=ztlabel)
  
  ggplot(subset(plotdf,!lt3sd),aes(Set,vals,color=lt3sd,group=ID))+
    geom_point(position=position_dodge(width=0.5),size=0.25)+
    geom_text(data=subset(plotdf,lt3sd),aes(Set,vals,label=lab),
              position = position_dodge(width=0.5),angle=270,size=7*5/14)+
    scale_color_manual(values=c("TRUE"=colors$purple,"FALSE"=colors$green))+
    getUnifiedGGTheme()+
    xlab("")+ylab("")+guides(color=F)+
    theme(axis.text.x=element_text(angle=90))
  
}