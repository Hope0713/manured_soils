library(xlsx)
library(pheatmap)
library(ggplot2)
library(ggsci)


###### Figure 1
f1 <- read.xlsx("H:/CM6/SupplmentaryInformation/Figure_data.xlsx",sheetName = "figure1")
f1$Type = factor(f1$Type,levels=c("ARG","BRG","MRG","VG"))
f1$Samples = factor(f1$Samples,levels = c("FS","GS1","GS5","GS10","G5_F5","COM"))
f1_1 <- ggplot(f1,aes(x=Samples, y=log2(Abundance)))+
  geom_boxplot(aes(fill=Samples)) +
  scale_fill_d3()+
  #scale_y_continuous(limits = c(0,5))+
  labs(x="",y="Abundance (copies per cell)")+
  theme_bw()
f1_1
p1 <- f1_1 + facet_wrap(~ Type,scales="free",nrow = 2)
p1 + theme(title=element_text(size = 7),
           axis.text.x = element_text(angle = 60,hjust = 1, size = 7),
           axis.title = element_text(size=7),
           axis.text = element_text(size=7))


###### Figure 4
###
f4 <- read.xlsx("H:/CM6/SupplmentaryInformation/Figure_data.xlsx",sheetName="MGE")

f4$Type = factor(f4$Type,levels=c("ARG","BRG","MRG","VG"))
f4$Samples = factor(f4$Samples,levels=c("FS","GS1","GS5","GS10","G5_F5","COM"))

ggplot(f4,aes(x=Samples, y=Diversity))+
  geom_boxplot(aes(fill=Samples)) +
  scale_fill_d3()+
  scale_y_continuous(limits = c(0,35))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size = 7),
        axis.text.y = element_text(size=7),legend.position = "top",
        axis.title = element_text(size=8))
### 250  280
ggplot(f4,aes(x=Samples, y=log2(Abundance)))+
  geom_boxplot(aes(fill=Samples)) +
  scale_fill_d3()+
  scale_y_continuous(limits = c(-2.1,2))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size = 7),
        axis.text.y = element_text(size=7),legend.position = "top",
        axis.title = element_text(size=8))

### 
f4_2 <- read.xlsx("H:/CM6/SupplmentaryInformation/Figure_data.xlsx",sheetName="Sheet2")
f4_3 <- ggplot(f4_2,aes(x=log2(MGE),y=log2(Abundance),color=Type))+
  geom_point(aes(color=Type),size=0.5) +
  scale_color_d3()+
  theme_bw()+
  geom_smooth(method="lm",size=0.5,level=0.95)+
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size=7),legend.position = "top",
        axis.title = element_text(size=8))
f4_3
ggsave(f4_3, file="ratings.pdf", width=2.35, height=2.30)

