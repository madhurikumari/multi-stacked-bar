#Sales Share
#6 figures of stacked bar charts on 1 plot. Common legend and y-axis. Common x-axis on bottom.
library(ggplot2)
library(RColorBrewer)

#set working directory
setwd("/Users/mkumari/Downloads")

#function to plot stacked bar charts
stacked_bar <- function(file,left){
  #left=TRUE means y-axis shows up on left side. Else on right.
  #bottom=TRUE displays x-axis on bottom. Else is blank.
  df_Melted <- melt(file, id.var = "Type")
  colourCount = length(unique(df_Melted$variable))
  p <- ggplot(df_Melted, aes(width=0.95, x = Type, y = value, fill = forcats::fct_rev(variable))) + 
    geom_bar(stat = "identity") +
    theme_bw()+ 
    scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
    guides(colour = guide_legend(nrow = 1))+
    {if(left==TRUE)scale_y_continuous(labels = function(x) paste0(x, "%"))}+
    {if(left==FALSE)scale_y_continuous(labels = function(x) paste0(x, "%"), position="right")}+
    theme( 
      plot.title = element_text(hjust = 0.5, size=15), 
      axis.title.x=element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size=13),
      legend.title = element_blank(),
      legend.text = element_text(size=15),
      legend.position = "none")
  return(p)
}


#BAU2030
df <- read.csv("MK_FromMarshall - BAU2030.csv", header = TRUE)
p1 <- stacked_bar(df, TRUE) + labs(title = "BAU 2030") +theme(axis.text.x = element_blank())

#BAU2050
df2 <- read.csv("MK_FromMarshall - BAU2050.csv", header = TRUE)
p2 <- stacked_bar(df2, FALSE)+  labs(title = "BAU 2050")+theme(axis.text.x = element_blank())


#ZEV2030
df3 <- read.csv("MK_FromMarshall - ZEV2030.csv", header = TRUE)
p3 <- stacked_bar(df3, TRUE) + labs(title = " ZEV 2030")+theme(axis.text.x = element_blank())

#ZEV2050
df4 <- read.csv("MK_FromMarshall - ZEV2050.csv", header = TRUE)
p4 <- stacked_bar(df4, FALSE) +   labs(title = "ZEV 2050")+theme(axis.text.x = element_blank())

#ZEV+B2030
df5 <- read.csv("MK_FromMarshall - ZEV+B2030.csv", header = TRUE)
p5 <- stacked_bar(df5, TRUE) +   labs(title = " ZEV+B 2030") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15))

#ZEV+B2050
df6 <- read.csv("MK_FromMarshall - ZEV+B2050.csv", header = TRUE)
p6 <- stacked_bar(df6, FALSE) +   labs(title = "ZEV+B 2050")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15))

p_final <- ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3, common.legend = TRUE, legend="right",
                     heights = c(1,1,1.54), widths = c(1,1))
annotate_figure(p_final, top=text_grob(label="Sales Share", hjust=1, size=20),
                left = text_grob(label="Percentage Sales Share (%)", hjust=0.5, size=15, rot=90))

