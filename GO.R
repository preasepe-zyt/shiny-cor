library(org.Dr.eg.db)  
library(clusterProfiler)
library(enrichplot)
library(ggplot2)
library(ggnewscale)
library(enrichplot)
library(DOSE)
library(stringr)

go <- function(x){
    pvalueFilter=0.05         
    qvalueFilter=1  
    showNum=10
    
    rt <- as.data.frame(x)
    genes=as.vector(rt[,1])
    entrezIDs <- mget(genes, org.Dr.egSYMBOL2EG, ifnotfound= NA )  
    entrezIDs <- as.character(entrezIDs)
    rt=cbind(rt,entrezID=entrezIDs)
    colnames(rt)=c("symbol","entrezID") 
    rt2=rt[rt[,"entrezID"] != "NA",]                        
    gene <- rt2$entrezID
    gene2 <- unique(gene)
    
    colorSel="qvalue"
    if(qvalueFilter>0.05){
        colorSel="pvalue"
    }
    
    
    kk=enrichGO(gene = gene2,OrgDb = org.Dr.eg.db, pvalueCutoff = 1, qvalueCutoff = 1, ont="all", readable =T)
    GO=as.data.frame(kk)
    GO=GO[(GO$pvalue<pvalueFilter & GO$qvalue<qvalueFilter),]
    
    #write.table(GO,file="GO1.xls",sep="\t",quote=T,row.names = T)
    if(nrow(GO)<30){
        showNum=nrow(GO)
    }
    bub<-dotplot(kk,showCategory = showNum, orderBy = "GeneRatio",split="ONTOLOGY", color = colorSel) + 
        facet_grid(ONTOLOGY~., scale='free')+
        scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=45))
        # theme(text=element_text(face= "bold"),
        #       axis.text.y = element_text(size= 5,color = "black",face= "bold"),
        #       axis.text.x = element_text(size= 5,color = "black",face= "bold"),
        #       legend.text = element_text(size= 5,face= "bold"),
        #       legend.title= element_text(size= 5,face= "bold"),
        #       #axis.text.x.top = element_text(size= 5,face= "bold"),
        #       #axis.text.y.left = element_text(size= 5,face= "bold"),
        #       #strip.text.x = element_text(size = 5, face = "bold"),
        #       strip.text.y = element_text(size = 5, face = "bold"),#小标题大 
        #       legend.key.size = unit(0.5, "cm"),
        #       axis.title.x.bottom =  element_text(size = 5, face = "bold"),
        #       axis.line = element_line(size = 0.5),
        #       panel.border = element_rect(linewidth = 3))#图例大小
    print(bub)
    
    af=setReadable(kk, 'org.Dr.eg.db', 'ENTREZID')
    cnet=cnetplot(kk, showCategory = showNum, categorySize="pvalue",circular = TRUE,
             color.params = list(edge = TRUE, category ="#E64B35CC", gene =
                                     "#386CB0"),
             cex.params = list(category_node = 0.8, gene_node = 0.8, 
                               category_label = 0.8, gene_label = 0.8),
             shadowtext='none')+
        theme(text=element_text(face= "bold"),
              #axis.text.y = element_text(size= 30,color = "black",face= "bold"),
              #axis.text.x = element_text(size= 30,color = "black",face= "bold"),
              legend.text = element_text(size= 5,face= "bold"),
              legend.title= element_text(size= 5,face= "bold"),
              #axis.text.x.top = element_text(size= 30,face= "bold"),
              #axis.text.y.left = element_text(size= 30,face= "bold"),
              #strip.text.x = element_text(size = 30, face = "bold"),
              strip.text.y = element_text(size = 5, face = "bold"),#小标题大 
              legend.key.size = unit(0.5, "cm"),
              #axis.title.x.bottom =  element_text(size = 30, face = "bold")
        )
    return(list(bub = bub, cnet = cnet, GO=GO))
}














