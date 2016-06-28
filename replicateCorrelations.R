### biological vs technical correlations
# INPUT
# 4 vectors of equal length
# Tissue = parent group (Liver/Lung)
# Age = subgroup (9mo,15mo,25mo)
# Cor = pairwise correlation values
# Relation = legend label for biological and/or technical test

source("colors.R")
source("ggtheme.R")

plotTRBRcors <- function(tissueVector,ageVector,correlations,corType){
  plotdf <- data.frame("Tissue"=tissueVector,
                       "Age"=ageVector,
                       "Cor"=correlations,
                       "Relation"=corType)
  ggplot(plotdf,aes(as.factor(Age),Cor,color=Relation))+
    geom_violin()+
    scale_color_manual(values=c(colors$orange,colors$green))+
    guides(colour = guide_legend(override.aes = list(alpha = 1,size=1)))+
    facet_wrap(~Tissue)+labs(color="")+
    getUnifiedGGTheme()+ylab("Pairwise Correlation")+xlab("Age")
}