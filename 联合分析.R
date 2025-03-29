library(tidyverse)
library(ggplot2)
library(extrafont)
library(corrplot)
#导入字体进入pdf
#showtext包可给定字体文件，加载到 R环境中，生成新的字体家族名字，后期调用这个名字设定字体，并且支持中文写入pdf不乱码
library(showtext)
showtext_auto(enable=TRUE)
require(reshape2)
library(readxl)
library(dplyr)

mRNA <- read_excel("C:/Users/79403/Desktop/Metabolite.xlsx",sheet = "mRNA")
mRNA <- mRNA %>% column_to_rownames(var = "gene_name")  %>% scale()

Metabolite <- read_excel("C:/Users/79403/Desktop/Metabolite.xlsx",sheet = "6 dpf")
Metabolite_class <- Metabolite %>% dplyr::select(c("Metabolite.name","Class"))
Metabolite <- Metabolite %>% column_to_rownames(var = "Metabolite.name") %>% dplyr::select(-"Class") %>% scale()


Metabolite <- read_excel("C:/Users/79403/Desktop/Metabolite.xlsx",sheet = "3 dpf") 
Metabolite_class <- Metabolite %>% dplyr::select(c("Metabolite.name","Class"))
Metabolite <- Metabolite %>% column_to_rownames(var = "Metabolite.name") %>% dplyr::select(-"Class") %>% scale()


#合并
cor_total <- as.data.frame(t(rbind(mRNA,Metabolite)))

#计算显著性差异

comcor<-cor(cor_total,method = "pearson")
comp<-cor.mtest(comcor,conf.level=0.95)
pval<-comp$p
#获取目标基因相关性矩阵
goalcor<- dplyr::select(as.data.frame(comcor),c("pgam1a","pgk1"))%>%rownames_to_column(var="group")

##长宽数据转换
goalcor<-melt(goalcor,id.vars="group")
colnames(goalcor)<-c("target","group","correlation")
#获取目标基因集pvalue矩阵
pval<-dplyr::select(as.data.frame(pval),c("pgam1a","pgk1"))%>%rownames_to_column(var="group")

#长宽数据转换
pval<-melt(pval,id.vars="group")
colnames(pval)<-c("target","group","pvalue")
#将pvalue和correlation两个文件合并
final<-left_join(goalcor,pval,by=c("target","group"))
final<-final%>%filter(!final$target%in%c("pgam1a","pgk1"))

#5.开始绘图

#添加一列,来判断pvalue值范围
final$sign<-case_when(final$pvalue >0.05~">0.05",
                      final$pvalue <0.05~"<0.05")
#添加一列来判断correlation的正负
final$core<-case_when(final$correlation >0 ~"positive",
                      final$correlation<0 ~"negtive")

final$sign <- factor(final$sign,levels = c(">0.05","<0.05"))
final <- final %>% arrange(sign,group)
final$target <- fct_inorder(final$target)

pdf("相关.pdf", width=12, height=17)
p1 <- ggplot(data=final,aes(x=target,y=group))+
    geom_point(aes(fill=sign),shape=21,size=15,alpha=0.7)+
    geom_text(aes(label = round(correlation,2)))+
    scale_fill_manual(values = c("#1F77B4FF","#2CA02CFF"))+
    labs(x="",y="",fill="Pvalue")+
    theme_bw()+
    mytheme+coord_flip()

library(cols4all)
mycol <- c4a('hcl.set2',11) #自定义配色挑选

Metabolite_class$index <- rep("value",nrow(Metabolite_class))
Metabolite_class$Metabolite.name <- factor(Metabolite_class$Metabolite.name, levels = levels(final$target))

p2 <-ggplot(Metabolite_class, aes(x = index, y = Metabolite.name)) +
    geom_tile(aes(fill = Class), colour = "white") +
    #scale_fill_gradient(low="white", high="#22a6b3") +
    #facet_wrap(~ Class, ncol = 2, scales = "free") +
    #scale_x_continuous(breaks = seq(0,60,2),expand = c(0, 0))+
    geom_text(aes(label=Class),color='#f1f2f6',size=5, fontface = "bold") +
    scale_x_discrete(expand = c(0, 0))+
    theme_classic()+labs(x="",y="")+theme_void()+
    theme(text = element_text(family = "sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "#ffffff"),
        strip.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position =  "none",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.margin = margin(0, 0, 0, 0)
    )+scale_fill_manual(values = mycol)
library(patchwork)
p2+p1+plot_layout(widths = c(2, 0.5))
#保存图片
dev.off()



if(T){mytheme <- theme(text = element_text(family = "sans"),
                       plot.title = element_text(size = 20,color="black",hjust = 0.5),
                       axis.title = element_text(size = 20,color ="black"), 
                       axis.text = element_text(size= 20,color = "black"),
                       axis.text.y = element_text(size= 20,color = "black", face = "italic"),
                       panel.grid.minor.y = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       axis.text.x = element_text(angle = 45, hjust = 1 ),
                       legend.position = "right",
                       legend.text = element_text(size= 20,color = "black"),
                       legend.title= element_text(size= 20,color = "black"),
                       axis.line = element_line(linewidth = 0.3),
                       axis.ticks = element_line(size = 0.5),
                       panel.border = element_rect(linewidth = 2.5),
                       plot.margin = margin(0, 0, 0, 0)
) }


library("GGally")
library("geomnet")
library("ggnetwork")
library("statnet")

final_plot <- merge(final,Metabolite_class,by.x="target",by.y="Metabolite.name")
final_plot$target <- as.character(final_plot$target)
for (i in 1:nrow(final_plot)){
    if(final_plot[i,"group"]=="pgam1a"){
        final_plot[i,"target"] <- paste(as.character(final_plot[i,"target"])," ",sep = "")
    }
}

final_plot <- final_plot %>% filter(sign=="<0.05")
xlsx::write.xlsx(final_plot,"final_csv_3dpf.xlsx",row.names =FALSE)


#网络绘图
#data2 <- final_plot[1:2,]
#data2$Class <- data2$group
#data2$target <- data2$group
data <- final_plot

fb.net<-network::network(as.matrix(data[,c(1,2)]))
# 添加顶点属性：队伍所在的会议
fb.net%v%"conf"<- as.character(data$Class)

# 添加边属性：两队是否同属一个会议
data$core <- as.character(data$core)
set.edge.attribute(fb.net,"core",data$core)
set.edge.attribute(fb.net,"core",ifelse(fb.net%e%"core"=="negtive","#386CB0","#E64B35CC"))
data$correlation <- as.numeric(round(data$correlation,2))
set.edge.attribute(fb.net,"correlation",abs(data$correlation))


categories <- c("pgam1a","pgk1","Organic acids and derivatives", "Organic oxygen compounds", "Flavin nucleotides", 
                "Purine nucleotides", "Amino acid", "Homogeneous non-metal compounds", 
                "(5'->5')-dinucleotides", "Nucleosides, nucleotides, and analogues")

colors <- c("grey","grey","#03C19E","#00BFC4","#C6A856","#53B6E1","#E19B79", "#A0B454","#ED90A4", "#6ABD74")

result <- setNames(colors, categories)
#自己的 要全部为characer
pdf("网络.pdf", width=13, height=8)
set.seed(222222)
ggnet2(fb.net,label = TRUE,
       #node.size=10,
       node.color="conf",
       layout.par=list(cell.jitter=0.75),# 可以传递布局参数
       mode="fruchtermanreingold",
       node.size=13,
       # size.cut = 3,  # 使用分位数将大小切割为三个类别
       #size="correlation",
       # 手动映射大小：size.palette = c("Atlantic Coast" = 1,...),
       # node.shape = "conf",
       node.alpha=0.5,
       edge.color="core",# 第一个值：同一组的节点使用相同颜色，否则使用第二个参数
       edge.alpha=0.5,
       edge.size=1,       
       #edge.label = "correlation",
       #edge.label.size = 2
       label.size = 3,
       #edge.lty = "core",palette
       color.legend="CLASS",
       palette = result,
       legend.position = "top",
       color = "grey75"
)+
    geom_point(aes(color = color), size = 17,alpha=0.5)
dev.off()

