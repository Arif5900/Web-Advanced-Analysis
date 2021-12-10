#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(devtools)
library(ggbiplot)
library(Hmisc)
library(caret)
library(DiagrammeR)
library(ggiraph)
library(ggiraphExtra)
library(shinythemes)
library(semPLS)
library(ca)
library(mlbench)
library(dplyr)
library(clValid)
library(cluster)
library(semPlot)
library(rsvg)
library(DiagrammeRsvg)
library(datamodelr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  navbarPage(
    "Advance Analysis",
    tabPanel(
      "Biplot Analysis",
      
      # Sidebar with a slider input for number of bins 
      sidebarPanel(
        
        h2("Please Upload Your Data Below", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 24px;
                font-weight: 500;
                text-align:center
                "),
        
        #input file
        fileInput("file_biplot", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        
        numericInput('colcount', 'Categorical Column', 1, min = 1, max = 100),
        submitButton("Submit"),
        
        #Select type file download
        #radioButtons("type", "Select type file download:",
        #             choices = c("pdf", "png"), selected = "pdf", inline = TRUE)
        
      ), #end of sidebarpanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(
            p("10 Top Data", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            tableOutput(outputId = "table_biplot"),
          ),#end of tab panel1 biplot
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            downloadButton(outputId = "down_biplot", label = "Download Plot"),
            plotOutput(outputId = "cetak_data"),
          ),#end of tab panel2 biplot
          
        ), #end od tabsetPanel biplot
      )
    ), #end of tabpanel1
    
    #regression analysis
    tabPanel(
      "Regression Analysis",
      
      # Sidebar with a slider input for number of bins 
      sidebarPanel(
        
        h2("Please Upload Your Data Below", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 24px;
                font-weight: 500;
                text-align:center
                "),
        
        #input file
        fileInput("file_regression", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        
        #choose separator
        #radioButtons("separator", "Separator",
         #            choices = c(Comma = ",",
          #                       Semicolon = ";",
           #                      Tab = "\t"),
            #         selected = ",", inline = TRUE),
        textInput("dcol", "Dependent Variable", value = "",placeholder = "input here",width = 200),
        
        textInput("icol", "Independent Variable", value = "",placeholder = "input here",width = 200),
        
        submitButton("Submit"),
      ), #end of sidebarpanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(
            p("10 Top Data", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            tableOutput(outputId = "table_regression"),
          ),#end of tab panel1 regression
          tabPanel(
            p("Summary of Regression", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            verbatimTextOutput("summary1"),
          ),#end of tab panel1 regression
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            downloadButton(outputId = "down_regresi", label = "Download Plot"),
            plotOutput(outputId = "cetak_data2")
          ),#end of tab panel2 regression
          
        ), #end od tabsetPanel regression
      )
    ), #end of tabpanel2 regression
    
    tabPanel(
      "SEM Analysis",
      
      # Sidebar with a slider input for number of bins 
      sidebarPanel(
        
        h2("Please Upload Your Data Below", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 24px;
                font-weight: 500;
                text-align:center
                "),
        
        #input file
        fileInput("file_SEM", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"),
        ),
        
        h4("Determine Variable Relationship (Inner Model):", style="
                  font-family: 'Poppins', sans-serif;
                  color: Black;
                  font-size: 16px;
                  font-weight: 500;
                  text-align:center
                  "),
        textAreaInput("source", "Source", value = "",placeholder = "input here", height = 150, width = 260),
        textAreaInput("target", "Target", value = "",placeholder = "input here", height = 150, width = 260),
        
        h4("Determine Variable Relationship (Outer Model):", style="
                  font-family: 'Poppins', sans-serif;
                  color: Black;
                  font-size: 16px;
                  font-weight: 500;
                  text-align:center
                  "),
        textAreaInput("source_ind", "Source", value = "",placeholder = "input here", height = 150, width = 260),
        textAreaInput("target_ind", "Target", value = "",placeholder = "input here", height = 150, width = 260),
        
        submitButton("Submit")
      ), #end of sidebarpanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(
            p("10 Top Data", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            tableOutput(outputId = "table_SEM"),
          ),#end of tab panel1 regression
          tabPanel(
            p("Summary of SEM", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            verbatimTextOutput("summary4"),
          ),#end of tab panel1 sem
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            downloadButton(outputId = "down_sem", label = "Download Plot"),
            grVizOutput("cetak_data4")
          ),#end of tab panel2 sem
          
        ), #end od tabsetPanel sem
      )
    ), #end of tabpanel2 sem
    
    #corespondence analysis
    tabPanel(
      "Correspondence Analysis",
      
      # Sidebar with a numeric input for number of bins 
      sidebarPanel(
        
        h2("Please Upload Your Data Below", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 24px;
                font-weight: 500;
                text-align:center
                "),
        
        #input file
        fileInput("file_ca", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        
        numericInput('colcount_ca', 'Categorical Column', 1, min = 1, max = 100),
        submitButton("Submit"),
        
        #Select type file download
        #radioButtons("type", "Select type file download:",
        #             choices = c("pdf", "png"), selected = "pdf", inline = TRUE)
        
      ), #end of sidebarpanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(
            p("10 Top Data", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            tableOutput(outputId = "table_ca"),
          ),#end of tab panel1 ca
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            downloadButton(outputId = "down_ca", label = "Download Plot"),
            plotOutput(outputId = "cetak_data5"),
          ),#end of tab panel2 ca
          
        ), #end od tabsetPanel ca
      )
    ), #end of tabpanel corresponden
    
    
    #hcluster analysis
    tabPanel(
      "Hierarchical Clustering",
      
      # Sidebar with a numeric input for number of bins 
      sidebarPanel(
        
        h2("Please Upload Your Data Below", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 24px;
                font-weight: 500;
                text-align:center
                "),
        
        #input file
        fileInput("file_hclus", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        
        radioButtons("radio_hclus", label = "Choose Hierarchical Cluster Method",
                     choices = list("SingleLinkage", "CompleteLinkage", "AverageLinkage"),
                                   ),
        
        
        numericInput('colcount_hclus', 'Categorical Column', 1, min = 1, max = 10),
        numericInput('colcount_cluster', 'Cluster Count', 1, min = 1, max = 10),
        submitButton("Submit"),
        
        
      ), #end of sidebarpanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(
            p("10 Top Data", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            tableOutput(outputId = "table_hclus"),
          ),#end of tab panel1 hclus
          
          tabPanel(
            p("Summary of HAC", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            verbatimTextOutput("summary5"),
          ),#end of tab panel1 sem
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            downloadButton(outputId = "down_hclus", label = "Download Plot"),
            plotOutput(outputId = "cetak_data6"),
          ),#end of tab panel2 hcluster
          
        ), #end od tabsetPanel hcluster
      )
    ), #end of tabpanel hcluster
    
    
    
    
    tabPanel(
      "References",
      
      p(
      "1. If you want to analyze the characteristics of the attributes and objects of observation and want to see the relative position of these objects, it is recommended to use", strong("Biplot Analysis"),
      
      br(),
      br(),
      
      "2. if you want to see the analysis of the influence and predict the relationship that occurs between 2 objects based on the independent variable and the dependent variable, it is recommended to use", strong("Regression Analysis"),
      
      br(),
      br(),
      
      "3. if you want to analyze the relationship between bound objects based on the cause and effect of the existence of the object, it is recommended to use", strong("SEM Analysis"),
      
      br(),
      br(),
      
      "4. if you want to analyze objects by looking at the categories of each object based on their respective interests, it is recommended to use", strong("Correspondence Analysis"),
      
      br(),
      br(),
      
      "5. If you want to group objects based on the similarity between these objects, it is advisable to use", strong("Hierarchical Cluster Analysis"),
      
      br(),
      br(),
      
      style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;",
      
      ), #end of p references
    ), #end tabpanel references
    
    
  ), #end of navbarpage
  
  

))
