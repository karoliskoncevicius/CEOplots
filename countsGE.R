### Stacked barplot of counts in genomic elements
# INPUT
# dataframe with columns:
# V1 - Percent CpGs represented by element
# set - dataset/lot ID
# cnt - number of CpGs in element
# feature - name of the genomic element

#pdf(height=3,width=4)

source("colors.R")
source("ggtheme.R")


plotGEcounts <- function(plotdf){
  require(ggplot2)
  require(dplyr)
  require(data.table)
  require(scales)
  
  Data <- group_by(data.table(plotdf),set) %>%
    mutate(pos = cumsum(V1) - (0.5 * V1))
  
  ggplot(Data)+
    scale_fill_manual(values=c("5kb Downstream"=colors$red,"5kb Upstream"=colors$yellow,"Exon"=colors$orange,
                               "Intron"=colors$green,"Other"=colors$purple))+
    getUnifiedGGTheme()+
    xlab("")+
    scale_y_continuous(labels=percent)+
    ylab("Proportion of CpGs")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(fill="Genomic\nElement")+
    geom_bar(aes(set,V1,fill=feature),stat="identity")+
    geom_text(aes(set,y=pos,label=cnt),size=7*5/14)
}





