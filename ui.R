
library(shiny, quietly = TRUE)
library(readxl, quietly = TRUE)
require(shinyjs, quietly = TRUE)
require(tidyverse, quietly = TRUE)
library(ggvenn, quietly = TRUE)

# 定义 UI
source("GO.R")
options(shiny.silent.errors = TRUE)
ui <- fluidPage(
    useShinyjs(),
    tags$head(
        tags$style(HTML("
            
            .s1{
                color: white; 
                width: 200px;
                background-color: orange;
                height: 25px;
                border-radious: 20px;
                border-sytle: none;
                cursor: pointer;}
            .s2{
                color: white; 
                width: 200px;
                background-color: #9467BDFF;
                height: 25px;
                border-radious: 20px;
                border-sytle: none;
                cursor: pointer;}
            .s3{
                color: white; 
                width: 200px;
                background-color: #2CA02CFF;
                height: 25px;
                border-radious: 20px;
                border-sytle: none;
                cursor: pointer;}
                .ss{
                color: white; 
                width: 200px;
                background-color: #9D7660;
                height: 25px;
                border-radious: 20px;
                border-sytle: none;
                cursor: pointer;}
            body {
                background: linear-gradient(to right, #D9F0D3,#7BCCC4, #1374B2);
                text-color: black;}
            .tabPanel{
                   background-color: #5DBBCD}
            .helpText{
                   color: black;   
            }"))),
    navlistPanel("分析种类",
                 widths = c(3, 10),
                 well = FALSE,
                 fluid= FALSE,
                 tabPanel("多组学联合分析",
                          sidebarLayout(
                              
                              sidebarPanel(
                                  class="tabPanel",
                                  h4("请选择组学分析文件 转录组（表达矩阵）、蛋白质组（蛋白丰度矩阵）、代谢组（峰面积）",class="helpText"),
                                  fileInput("file1", "选择文件 1", accept = c(".xlsx", ".xls", ".csv")),
                                  fileInput("file2", "选择文件 2", accept = c(".xlsx", ".xls", ".csv")),
                                  selectizeInput("selected_item_da1", 
                                                 label = "文件1的名称列",
                                                 choices = NULL,
                                                 selected = NULL,
                                                 multiple = FALSE,
                                                 options = list(
                                                     placeholder = '请选择一个项目',
                                                     onInitialize = I('function() { this.setValue(""); }'))
                                  ),
                                  selectizeInput("selected_item2_da1", 
                                                 label = "文件1的数值列",
                                                 choices = NULL,
                                                 selected = NULL,
                                                 multiple = TRUE,
                                                 options = list(
                                                     placeholder = '请选择一个或者多个列',
                                                     onInitialize = I('function() { this.setValue(""); }'))
                                  ),
                                  selectizeInput("class", 
                                                 label = "文件1的类",
                                                 choices = NULL,
                                                 selected = NULL,
                                                 multiple = FALSE,
                                                 options = list(
                                                     placeholder = '请选择文件1的类',
                                                     onInitialize = I('function() { this.setValue(""); }'))
                                  ),
                                  selectizeInput("selected_item_da2", 
                                                 label = "文件2的名称列",
                                                 choices = NULL,
                                                 selected = NULL,
                                                 multiple = FALSE,
                                                 options = list(
                                                     placeholder = '请选择一个列',
                                                     onInitialize = I('function() { this.setValue(""); }'))
                                  ),
                                  selectizeInput("selected_item2_da2", 
                                                 label = "文件2的数值列",
                                                 choices = NULL,
                                                 selected = NULL,
                                                 multiple = TRUE,
                                                 options = list(
                                                     placeholder = '请选择一个或者多个列',
                                                     onInitialize = I('function() { this.setValue(""); }'))
                                  ),
                                  br(), 
                                  br(), 
                                  br(), 
                                  actionButton("clear_all", "清除全部选项",class="s2"),
                                  actionButton("cor", "开始关联分析",class="s1"),  # 处理文件的按钮
                                  downloadButton("download_data", "下载 Excel 文件",class="s3"),  # 下载按钮
                                  downloadButton("download_pdf", "下载 PDF 文件",class="s3")
                                  
                              ),
                              mainPanel(
                                  uiOutput("message"),
                                  h3("选择列"),
                                  verbatimTextOutput("analysis_result"),
                                  dataTableOutput("data1"),
                                  verbatimTextOutput("analysis_result2"),
                                  dataTableOutput("data2"),
                                  dataTableOutput("data3"),
                                  plotOutput("plot1", width = "1000px", height = "800px"),
                                  plotOutput("plot2"),
                                  plotOutput("plot3"),
                                  plotOutput("plot4"),
                                  plotOutput("plot5"),
                                  plotOutput("plot6")))
                 ))
)