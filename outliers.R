### Plot intersample correlations and indicate outliers
# INPUT
# 5 vectors of equal length
# cors - mean correlation values
# set - name of set/lot/tissue/age etc.
# isoutlier - is less than mean-3sd
# setID - unique ID for sample
# ztlabel - ZT/mouse ID

# pdf(width=4,height=2)

library(ggplot2)

source("ggtheme.R")
source("colors.R")

plotOutliers <- function(cors,set,isoutlier,setID,ztlabel){
  
  plotdf <- data.frame("vals"=cors,
                       "Set"=set,
                       "lt3sd"=isoutlier,
                       "ID"=setID,
                       "lab"=ztlabel)
  
  # ggplot(subset(plotdf,!lt3sd),aes(Set,vals,color=lt3sd,group=ID))+
    # ggplot(plotdf,aes(Set,vals,color=lt3sd,group=ID))+
    # geom_point(position=position_dodge(width=0.5),size=0.25)+
    # geom_text(data=subset(plotdf,lt3sd),aes(Set,vals,label=lab),
    #           position = position_dodge(width=0.5),angle=270,size=7*5/14)+
    # scale_color_manual(values=c("TRUE"=colors$purple,"FALSE"=colors$green))+
    # getUnifiedGGTheme()+
    # xlab("")+ylab("")+guides(color=F)+
    # theme(axis.text.x=element_text(angle=90))
    
  cols <- unlist(colors)[-7]
  names(cols) <- NULL
  
  ggplot(plotdf,aes(Set,vals,fill=Set))+
    geom_violin(width=1,color="grey",scale = "width",size=0.25)+
    # geom_text(data=subset(plotdf,lt3sd),aes(Set,vals,label=lab,group=ID),
    #           position = position_dodge(width=0.5),
    #           angle=90,size=7*5/14,color="black")+
    geom_point(data=subset(plotdf,lt3sd),aes(group=ID),position=position_dodge(width=0.5),size=0.5)+
    scale_color_manual(values=cols)+
    scale_fill_manual(values=cols)+
    getUnifiedGGTheme()+
    ylim(c(0.9,1))+
    xlab("")+ylab("")+guides(color=F,fill=F)+
    theme(axis.text.x=element_text(vjust=0.5,angle=90))
    
  
}
