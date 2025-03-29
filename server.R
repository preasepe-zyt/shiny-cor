library(shinyjs, quietly = TRUE) 
library(shiny, quietly = TRUE)
library(readxl, quietly = TRUE)
require(shinyjs, quietly = TRUE)
require(tidyverse, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(extrafont, quietly = TRUE)
library(corrplot, quietly = TRUE)
#导入字体进入pdf
#showtext包可给定字体文件，加载到 R环境中，生成新的字体家族名字，后期调用这个名字设定字体，并且支持中文写入pdf不乱码
library(showtext, quietly = TRUE)
showtext_auto(enable=TRUE)
require(reshape2, quietly = TRUE)
library(readxl, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(cols4all, quietly = TRUE)
library(patchwork, quietly = TRUE)


server <- function(input, output, session) {
     
    global_vars <- reactiveValues(
        data1_global = data.frame(),
        data2_global = data.frame(),
        final_cor = data.frame()
    )
      
    observeEvent(input$clear_all, {
        reset("file1")
        reset("file2")
        updateSelectizeInput(session, "selected_item_da1", choices = character(0))
        updateSelectizeInput(session, "selected_item2_da1", choices = character(0))
        updateSelectizeInput(session, "selected_item_da2", choices = character(0))
        updateSelectizeInput(session, "selected_item2_da2", choices = character(0))
        updateSelectizeInput(session, "class", selected = character(0))
        output$data1 <- renderDataTable({})
        output$data2 <- renderDataTable({})
        output$message <- renderUI({
        })
    })
    
    observeEvent(input$file1, {
        req(input$file1)  # 确保文件1已上传
        if(str_detect(input$file1$name,"xls")){
            data1 <- read_excel(input$file1$datapath)
            global_vars$data1_global <- data1 
            output$data1 <- renderDataTable({
                head(data1)
            })
            updateSelectizeInput(session, "selected_item_da1", choices = colnames(data1))
            updateSelectizeInput(session, "selected_item2_da1", choices = colnames(data1))
            updateSelectizeInput(session, "class", choices = colnames(data1))
        }else if(str_detect(input$file1$name,"csv")|str_detect(input$file1$name,"txt")){
            data1 <- read.csv(input$file1$datapath)
            global_vars$data1_global <- data1 
            output$data1 <- renderDataTable({
                head(data1)
            })
            updateSelectizeInput(session, "selected_item_da1", choices = colnames(data1))
            updateSelectizeInput(session, "selected_item2_da1", choices = colnames(data1))
            updateSelectizeInput(session, "class", choices = colnames(data1))
        }else if(str_detect(input$file1$name,".xlsx")){
            data1 <- read_excel(input$file1$datapath)
            global_vars$data1_global <- data1 
            output$data1 <- renderDataTable({
                head(data1)
            })
            updateSelectizeInput(session, "selected_item_da1", choices = colnames(data1))
            updateSelectizeInput(session, "selected_item2_da1", choices = colnames(data1))
            updateSelectizeInput(session, "class", choices = colnames(data1))
        }else{
            data1 <- read.delim(input$file1$datapath)
            global_vars$data1_global <- data1 
            output$data1 <- renderDataTable({
                head(data1)
            })
            updateSelectizeInput(session, "selected_item_da1", choices = colnames(data1))
            updateSelectizeInput(session, "selected_item2_da1", choices = colnames(data1))
            updateSelectizeInput(session, "class", choices = colnames(data1))
        }
        
    })
    
    observeEvent(input$file2, {
        req(input$file2)  # 确保文件2已上传
        if(str_detect(input$file2$name,"xls")){
            data2 <- read_excel(input$file2$datapath)
            global_vars$data2_global <- data2 
            output$data2 <- renderDataTable({
                head(data2)
            })
            updateSelectizeInput(session, "selected_item_da2", choices = colnames(data2))
            updateSelectizeInput(session, "selected_item2_da2", choices = colnames(data2))
        }else if(str_detect(input$file2$name,"csv")|str_detect(input$file2$name,"txt")){
            data2 <- read.csv(input$file2$datapath)
            global_vars$data2_global <- data2 
            output$data2 <- renderDataTable({
                head(data2)
            })
            updateSelectizeInput(session, "selected_item_da2", choices = colnames(data2))
            updateSelectizeInput(session, "selected_item2_da2", choices = colnames(data2))
        }else if(str_detect(input$file2$name,".xlsx")){
            data2 <- read_excel(input$file2$datapath)
            global_vars$data2_global <- data2 
            output$data2 <- renderDataTable({
                head(data2)
            })
            updateSelectizeInput(session, "selected_item_da2", choices = colnames(data2))
            updateSelectizeInput(session, "selected_item2_da2", choices = colnames(data2))
        }else{
            data2 <- read_delim(input$file2$datapath)
            global_vars$data2_global <- data2 
            output$data2 <- renderDataTable({
                head(data2)
            })
            updateSelectizeInput(session, "selected_item_da2", choices = colnames(data2))
            updateSelectizeInput(session, "selected_item2_da2", choices = colnames(data2))
        }
    })
    
    
    # 示例性的分析结果输出
    observe({
        req(input$file1,input$selected_item_da1,input$selected_item2_da1,input$class)
        output$data1 <- renderDataTable({
            file_in <- global_vars$data1_global[c(input$selected_item_da1,input$selected_item2_da1)]
            head(file_in)
        })
    })
    observe({
        req(input$file2,input$selected_item_da2,input$selected_item2_da2)
        output$data2 <- renderDataTable({
            file_in2 <- global_vars$data2_global[c(input$selected_item_da2,input$selected_item2_da2)]
            head(file_in2)
            
            
        })
    })
    
    output$analysis_result <- renderPrint({
        cat("文件1是:",input$file1$name,"选择的名称列是:",input$selected_item_da1,"选择的数值列是:",input$selected_item2_da1)
        
    })
    output$analysis_result2 <- renderPrint({
        
        cat("文件2是:",input$file2$name,"选择的名称列是:",input$selected_item_da2,"选择的数值列是:",input$selected_item2_da2)
        
    })
    
    observeEvent(input$cor,{
        if (is.null(input$file1) || is.null(input$file2) || 
            is.null(input$selected_item_da1) || is.null(input$selected_item2_da1) ||
            is.null(input$selected_item_da2) || is.null(input$selected_item2_da2)) {
            
            output$message <- renderUI({
                tags$h3("请确保所有必填项都已选择或上传", style = "color: red;")
            })
        } else {
            output$message <- renderUI({
                tags$h3("开始联合分析分析")
            })
        }
        req(input$file1,input$file2, input$selected_item_da1, 
            input$selected_item2_da1, input$selected_item_da2, input$selected_item2_da2)
        
        
        
        file1_class <- global_vars$data1_global %>% dplyr::select(c(input$selected_item_da1,input$class))
        file1 <- global_vars$data1_global %>% column_to_rownames(var = input$selected_item_da1) %>% dplyr::select(-input$class) 
        file2 <- global_vars$data2_global %>% column_to_rownames(var = input$selected_item_da2)  
        
        output$data1 <- renderDataTable({
            head(file1)
        })
        output$data2 <- renderDataTable({
            head(file2)
        })
        output$data3 <- renderDataTable({
            head(file1_class)
        })
        
        
        #合并
        cor_total <- as.data.frame(t(rbind(file1,file2))) %>% scale()

        #计算显著性差异
        comcor<-cor(cor_total,method = "spearman")
        comp<-cor.mtest(comcor,conf.level=0.95)
        pval<-comp$p
        #获取目标基因相关性矩阵
        goalcor<- dplyr::select(as.data.frame(comcor),rownames(file2)) %>% rownames_to_column(var="group")

        ##长宽数据转换
        goalcor<-melt(goalcor,id.vars="group")
        colnames(goalcor)<-c("target","group","correlation")
        #获取目标基因集pvalue矩阵
        pval<-dplyr::select(as.data.frame(pval),rownames(file2))%>%rownames_to_column(var="group")

        #长宽数据转换
        pval<-melt(pval,id.vars="group")
        colnames(pval)<-c("target","group","pvalue")
        #将pvalue和correlation两个文件合并
        final<-left_join(goalcor,pval,by=c("target","group"))
        final<-final%>%filter(!final$target %in% rownames(file2))
        final_cor <- final
        
        
        output$analysis_result <- renderPrint({
            cat("相关结果如下")
        })
        output$analysis_result2 <- renderPrint({
            cat("相关结果可视化如下")
        })
        output$data1 <- renderDataTable({
            head(final)
        })
        output$data2 <- renderDataTable({
        })
        
        
    output$download_data <- downloadHandler(
            filename = function() {
                "global_vars.xls"  # 设置下载文件的名字
            },
            content = function(file) {
                # 将 global_vars 写入到 Excel 文件中
                write_excel_csv(final, path = file)  # 这里使用动态的文件路径
            })
    
    # #5.开始绘图
    output$plot1 <- renderPlot({
    #添加一列,来判断pvalue值范围
    final$sign<-case_when(final$pvalue > 0.05~">0.05",
                          final$pvalue < 0.05~"<0.05")
    #添加一列来判断correlation的正负
    final$core<-case_when(final$correlation > 0 ~"positive",
                          final$correlation < 0 ~"negtive")

    final$sign <- factor(final$sign,levels = c(">0.05","<0.05"))
    final <- final %>% arrange(sign,group)
    final$target <- fct_inorder(final$target)
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
    p1 <- ggplot(data=final,aes(x=target,y=group))+
        geom_point(aes(fill=sign),shape=21,size=15,alpha=0.7)+
        geom_text(aes(label = round(correlation,2)))+
        scale_fill_manual(values = c("#1F77B4FF","#2CA02CFF"))+
        labs(x="",y="",fill="Pvalue")+
        theme_bw()+
        mytheme+coord_flip()
    
    mycol <- c4a('hcl.set2',11) #自定义配色挑选
    
    file1_class$index <- rep("value",nrow(file1_class))
    file1_class[[input$selected_item_da1]] <- factor(file1_class[[input$selected_item_da1]], levels = levels(final$target))
    names(file1_class)  <- c("file_name","class","index")
    
    
    output$data3 <- renderDataTable({
        head(file1_class)
    })


    p2 <- ggplot(file1_class, aes(x = index, y = file_name)) +
        geom_tile(aes(fill = class), colour = "white") +
        #scale_fill_gradient(low="white", high="#22a6b3") +
        #facet_wrap(~ Class, ncol = 2, scales = "free") +
        #scale_x_continuous(breaks = seq(0,60,2),expand = c(0, 0))+
        geom_text(aes(label=class),color='#f1f2f6',size=5, fontface = "bold") +
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
    output$download_pdf <- downloadHandler(
        filename = function() {
            "cor.pdf"  # 设置下载的文件名
        },
        content = function(file) {
            plot_obj <- p2+p1+plot_layout(widths = c(2, 0.5))
            ggsave(file, plot = plot_obj , width=12, device = "pdf",height=17)
        })
    p2+p1+plot_layout(widths = c(2, 0.5))
    })

    
})
}