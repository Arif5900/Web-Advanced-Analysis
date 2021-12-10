#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
library(dplyr)
library(mlbench)
library(ca)
library(clValid)
library(cluster)
library(rsvg)
library(DiagrammeRsvg)
library(datamodelr)
library(seminr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  output$cetak_data <- renderPlot({
    
    file_biplot <- input$file_biplot
    
    if (is.null(file_biplot))
      return(NULL)
    
    data = read.csv(file_biplot$datapath)
    
    data_bip = data[,-(input$colcount)]
    dt_new1bip <- data.matrix(data.frame(unclass(data_bip))) 
    dt_new2bip <- as.matrix(data[, input$colcount])
    final_dtbip <- data.frame(dt_new2bip, dt_new1bip)
    dt_new3bip <- final_dtbip %>%
      group_by(dt_new2bip) %>% 
      dplyr::summarise(across(everything(), list(sum)))
    data.normal=scale(dt_new3bip[,-1])
    data.pca=prcomp(data.normal, center=F)
    
    biplot(data.pca, scale=0, cex=0.7)
    g=ggbiplot(data.pca, obs.scale=1, var.scale=1, labels=as.matrix(dt_new3bip[, 1]),
               elipse=T, circle=T)
    g=g+scale_color_discrete(name='')
    g=g+theme(legend.direction="horizontal", legend.position="top")
    print(g)
    theme1 = g+theme_bw()
    print(theme1)
    
    
  }) #end of cetak biplot
  
  output$down_biplot <- downloadHandler(
    #specify filename
    filename = function() {
      #biplot.png
      #biplot.pdf
      paste("biplot","png", sep = ".")
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #  pdf(file)
      
      file_biplot <- input$file_biplot
      
      if (is.null(file_biplot))
        return(NULL)
      
      data = read.csv(file_biplot$datapath)
      
      data_bip = data[,-(input$colcount)]
      dt_new1bip <- data.matrix(data.frame(unclass(data_bip))) 
      dt_new2bip <- as.matrix(data[, input$colcount])
      final_dtbip <- data.frame(dt_new2bip, dt_new1bip)
      dt_new3bip <- final_dtbip %>%
        group_by(dt_new2bip) %>% 
        dplyr::summarise(across(everything(), list(sum)))
     
      data.normal=scale(dt_new3bip[,-1])
      data.pca=prcomp(data.normal, center=F)
      
      biplot(data.pca, scale=0, cex=0.7)
      g=ggbiplot(data.pca, obs.scale=1, var.scale=1, labels=as.matrix(dt_new3bip[, 1]),
                 elipse=T, circle=T)
      g=g+scale_color_discrete(name='')
      g=g+theme(legend.direction="horizontal", legend.position="top")
      print(g)
      theme1 = g+theme_bw()
      print(theme1)
      
      dev.off()
    }
  ) #end of download button for biplot
  
  output$table_biplot <- renderTable({
    file_biplot <- input$file_biplot
    ext <- tools::file_ext(file_biplot$datapath)
    
    req(file_biplot)
    #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
    
    if (is.null(file_biplot))
      return(NULL)
    
    data4 = read.csv(file_biplot$datapath)
    head(data4, n=10)
    
  }, height = 500, width = 700) #end of biplot
  
  output$table_regression <- renderTable({
    file_regression <- input$file_regression
    ext <- tools::file_ext(file_regression$datapath)
    
    req(file_regression)
    #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
    
    if (is.null(file_regression))
      return(NULL)
    
    data2 = read.csv(file_regression$datapath)
    head(data2, n=10)
    
  }, height = 500, width = 700)
  
  
  output$summary1 <- renderPrint({
    file_regression <- input$file_regression
    ext <- tools::file_ext(file_regression$datapath)
    
    req(file_regression)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_regression))
      return(NULL)
    
    data2 = read.csv(file_regression$datapath)
    
    x <- input$dcol
    y <- input$icol
    
    if (is.null(x))
      return(NULL)
    else if (is.null(y))
      return(NULL)
    
    #nomalize
    norm <- data.matrix(data.frame(unclass(data2)))
    
    #change int dataframe
    final <- data.frame(norm)
    
    model=lm(final[, c(x,y)], data=final)
    summary(model)
  })#end of summary regression 1
  
  output$cetak_data2 <- renderPlot({
    
    file_regression <- input$file_regression
    ext <- tools::file_ext(file_regression$datapath)
    
    req(file_regression)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_regression))
      return(NULL)
    
    data2 = read.csv(file_regression$datapath)
    
    #selectedData <- reactive({
    #data2[, c(input$ycol,input$xcol)]
    #})
    x <- input$dcol
    y <- input$icol
    
    if (is.null(x))
      return(NULL)
    else if (is.null(y))
      return(NULL)
    
    #nomalize
    norm <- data.matrix(data.frame(unclass(data2)))
    
    #change int dataframe
    final <- data.frame(norm)
    
    model2=lm(final[, c(x,y)], data=final)
    par(mfrow=c(2,2))
    plot(model2)
    
  },  height = 500, width = 700) #end of regression analysis 1
  
  output$down_regresi <- downloadHandler(
    #specify filename
    filename = function() {
      #regresi.png
      #regresi.pdf
      paste("regresi","png", sep = ".")
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #  pdf(file)
      
      file_regression <- input$file_regression
      ext <- tools::file_ext(file_regression$datapath)
      
      req(file_regression)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      if (is.null(file_regression))
        return(NULL)
      
      data2 = read.csv(file_regression$datapath)
      
      #selectedData <- reactive({
      #data2[, c(input$ycol,input$xcol)]
      #})
      x <- input$dcol
      y <- input$icol
      
      if (is.null(x))
        return(NULL)
      else if (is.null(y))
        return(NULL)
      
      #nomalize
      norm <- data.matrix(data.frame(unclass(data2)))
      
      #change int dataframe
      final <- data.frame(norm)
      
      model2=lm(final[, c(x,y)], data=final)
      par(mfrow=c(2,2))
      plot(model2)
      
      dev.off()
    }
  ) #end of download button for regresi
  
  
  output$table_multiregression <- renderTable({
    file_multiregression <- input$file_multiregression
    ext <- tools::file_ext(file_multiregression$datapath)
    
    req(file_multiregression)
    #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
    
    if (is.null(file_multiregression))
      return(NULL)
    
    data3 = read.csv(file_multiregression$datapath, sep = input$separator)
    head(data3, n=10)
    
  }, height = 500, width = 700)#end of multiregression
  
  output$summary2 <- renderPrint({
    file_multiregression <- input$file_multiregression
    ext <- tools::file_ext(file_multiregression$datapath)
    
    req(file_multiregression)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_multiregression))
      return(NULL)
    
    data3 = read.csv(file_multiregression$datapath, sep = input$separator)
    
    x <- data3[, c(input$dcol)]
    
    if (is.null(x))
      return(NULL)
    
    model=lm(formula = x~., data=data3)
    summary(model)
  })#end of summary multiregression 
  
  output$cetak_data3 <- renderPlot({
    
    file_multiregression <- input$file_multiregression
    ext <- tools::file_ext(file_multiregression$datapath)
    
    req(file_multiregression)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_multiregression))
      return(NULL)
    
    data3 = read.csv(file_multiregression$datapath)
    
    x <- data3[, c(input$dcol)]
    
    if (is.null(x))
      return(NULL)
    
    model2=lm(formula = x~., data=data3)
    par(mfrow=c(2,2))
    plot(model2)
    
  },  height = 500, width = 700) #end of multiregression analysis 
  
  output$table_SEM <- renderTable({
    file_SEM <- input$file_SEM
    ext <- tools::file_ext(file_SEM$datapath)
    
    req(file_SEM)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_SEM))
      return(NULL)
    
    datasem = read.csv(file_SEM$datapath)
    head(datasem, n=10)
  }
  )
  
  output$summary4 <- renderPrint({
    get_source <-read.csv(text = input$source, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source <- unlist(get_source)
    
    
    get_target <-read.csv(text = input$target, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target <- unlist(get_target)
    
    
    matrix_inner = cbind(get_source, get_target)
    colnames(matrix_inner) = c("Source", "Target")
    
    print(matrix_inner)
    cat(sprintf("\n\n"))
    
    get_source_ind <-read.csv(text = input$source_ind, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source_ind <- unlist(get_source_ind)
    
    
    get_target_ind <-read.csv(text = input$target_ind, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target_ind <- unlist(get_target_ind)
    
    
    matrix_outer = cbind(get_source_ind, get_target_ind)
    colnames(matrix_outer) = c("Source Indicator", "Target Indicator")
    
    print(matrix_outer)
    cat(sprintf("\n\n"))
    
    file_SEM <- input$file_SEM
    ext <- tools::file_ext(file_SEM$datapath)
    
    req(file_SEM)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_SEM))
      return(NULL)
    
    datasem = read.csv(file_SEM$datapath)
    
    model_sem <- plsm(data = datasem, strucmod = matrix_inner, measuremod = matrix_outer)
    
    cetak_sem <- sempls(model = model_sem, data = datasem, wscheme = "centroid")
    
    print(cetak_sem)
  })#end of summarySEM analysis
  
  output$cetak_data4 <- renderGrViz({
    
    get_source <-read.csv(text = input$source, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source <- unlist(get_source)
    
    
    get_target <-read.csv(text = input$target, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target <- unlist(get_target)
    
    
    matrix_inner = cbind(get_source, get_target)
    colnames(matrix_inner) = c("Source", "Target")
    
    
    get_source_ind <-read.csv(text = input$source_ind, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source_ind <- unlist(get_source_ind)
    
    
    get_target_ind <-read.csv(text = input$target_ind, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target_ind <- unlist(get_target_ind)
    
    
    matrix_outer = cbind(get_source_ind, get_target_ind)
    colnames(matrix_outer) = c("Source Indicator", "Target Indicator")
    
    
    file_SEM <- input$file_SEM
    ext <- tools::file_ext(file_SEM$datapath)
    
    req(file_SEM)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_SEM))
      return(NULL)
    
    datasem = read.csv(file_SEM$datapath)
    
    model_sem <- plsm(data = datasem, strucmod = matrix_inner, measuremod = matrix_outer)
    
    cetak_sem <- sempls(model = model_sem, data = datasem, wscheme = "centroid")
    
    
    pathDiagram(cetak_sem, file = "graph_sem", full = TRUE, edge.labels = "both", output.type = "graphics", digits = 2, graphics.fmt = "pdf")
    grViz("graph_sem.dot")
    
  }) #end of Plot SEM analysis
  
  output$down_sem <- downloadHandler(
    #specify filename
    filename = function() {
      "sem.png"
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #pdf(file)
      
      #SEM graph
      get_source <-read.csv(text = input$source, header = FALSE, sep = "", na.strings = c("", "NA"))
      get_source <- unlist(get_source)
      
      
      get_target <-read.csv(text = input$target, header = FALSE, sep = "", na.strings = c("", "NA"))
      get_target <- unlist(get_target)
      
      
      matrix_inner = cbind(get_source, get_target)
      colnames(matrix_inner) = c("Source", "Target")
      
      
      get_source_ind <-read.csv(text = input$source_ind, header = FALSE, sep = "", na.strings = c("", "NA"))
      get_source_ind <- unlist(get_source_ind)
      
      
      get_target_ind <-read.csv(text = input$target_ind, header = FALSE, sep = "", na.strings = c("", "NA"))
      get_target_ind <- unlist(get_target_ind)
      
      
      matrix_outer = cbind(get_source_ind, get_target_ind)
      colnames(matrix_outer) = c("Source Indicator", "Target Indicator")
      
      
      file_SEM <- input$file_SEM
      ext <- tools::file_ext(file_SEM$datapath)
      
      req(file_SEM)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      if (is.null(file_SEM))
        return(NULL)
      
      datasem = read.csv(file_SEM$datapath)
      
      model_sem <- plsm(data = datasem, strucmod = matrix_inner, measuremod = matrix_outer)
      
      cetak_sem <- sempls(model = model_sem, data = datasem, wscheme = "centroid")
      
      
      pathDiagram(cetak_sem, file = "graph_sem", full = TRUE, edge.labels = "both", output.type = "graphics", digits = 2, graphics.fmt = "pdf")
      graph <- grViz("graph_sem.dot")
      
      graph %>% export_svg %>% charToRaw %>% rsvg_png()
      #export_graph(grViz("graph_sem.dot"),
       #            file_name = file,
        #           file_type = "png")
      
      dev.off()
    },
    contentType = 'image/png'
  ) #end of download button for sem
  
  output$table_ca <- renderTable({
    file_ca <- input$file_ca
    ext <- tools::file_ext(file_ca$datapath)
    
    req(file_ca)
    #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
    
    if (is.null(file_ca))
      return(NULL)
    
    data5 = read.csv(file_ca$datapath)
    head(data5, n=10)
    
  }, height = 500, width = 700) #end of table ca
  
  output$cetak_data5 <- renderPlot({
    file_ca <- input$file_ca
    ext <- tools::file_ext(file_ca$datapath)
    
    req(file_ca)
    #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
    
    if (is.null(file_ca))
      return(NULL)
    
    data5 = read.csv(file_ca$datapath)
    data_ca = data5[,-(input$colcount_ca)]
    data_ca
    dt_new1 <- data.matrix(data.frame(unclass(data_ca))) 
    dt_new1
    dt_new2 <- as.matrix(data5[, input$colcount_ca])
    dt_new2
    final_dt <- data.frame(dt_new2, dt_new1)
    final_dt
    dt_new3 <- final_dt %>%
      group_by(dt_new2) %>% 
      dplyr::summarise(across(everything(), list(sum)))
    dt_new3
    korespon = as.matrix(dt_new3[, -1])
    dimnames(korespon) = list(x = as.matrix(dt_new3[, 1]), y = colnames(dt_new3[,-1]))
    korespon.ca = ca(korespon)
    plot(korespon.ca, what = c("all", "all"), mass=TRUE, 
         contrib="relative", main="Analisis Korespondensi")
    
  },height = 500, width = 700)
  
  output$down_ca <- downloadHandler(
    #specify filename
    filename = function() {
      #biplot.png
      #biplot.pdf
      paste("correspondence","png", sep = ".")
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #  pdf(file)
      
      file_ca <- input$file_ca
      ext <- tools::file_ext(file_ca$datapath)
      
      req(file_ca)
      #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
      
      if (is.null(file_ca))
        return(NULL)
      
      data5 = read.csv(file_ca$datapath)
      data_ca = data5[,-(input$colcount_ca)]
      data_ca
      dt_new1 <- data.matrix(data.frame(unclass(data_ca))) 
      dt_new1
      dt_new2 <- as.matrix(data5[, input$colcount_ca])
      dt_new2
      final_dt <- data.frame(dt_new2, dt_new1)
      final_dt
      dt_new3 <- final_dt %>%
        group_by(dt_new2) %>% 
        dplyr::summarise(across(everything(), list(sum)))
      dt_new3
      korespon = as.matrix(dt_new3[, -1])
      dimnames(korespon) = list(x = as.matrix(dt_new3[, 1]), y = colnames(dt_new3[,-1]))
      korespon.ca = ca(korespon)
      plot(korespon.ca, what = c("all", "all"), mass=TRUE, 
           contrib="relative", main="Analisis Korespondensi")
      
      dev.off()
    }
  ) #end of download button for ca
  
  
  output$cetak_data6 <- renderPlot({
    
    file_hclus <- input$file_hclus
    
    if (is.null(file_hclus))
      return(NULL)
    
    data6 = read.csv(file_hclus$datapath)
    
    #choose cluster column
    features = data6[, -(input$colcount_hclus)]
    
    #normization of categorical data
    features1 <- data.matrix(data.frame(unclass(features)))
    
    #target data to matrix
    target <- as.matrix(data6[, input$colcount_hclus])
    
    #combine
    clus_final <- data.frame(target, features1)
    
    #grouping
    clus_final2 <- clus_final %>%
      group_by(target) %>%
      dplyr::summarise(across(everything(), list(sum)))
    
    #normalization all of data
    data_nor <- scale(clus_final2[, -1], scale = TRUE)
    
    #count distance
    euc <- dist(data_nor, method = "euclidean")
    
    #method
    if(input$radio_hclus == "SingleLinkage") {
    
      single <- hclust(euc, method = "single")
      plot(single, hang = -2, cex = 1, labels = clus_final2$target) 
      
      #count cluster
      rect.hclust(single, input$colcount_cluster)
    } else if (input$radio_hclus == "CompleteLinkage") {
      
      complete <- hclust(euc, method = "complete")
      plot(complete, hang = -2, cex = 1, labels = clus_final2$target) 
      
      #count cluster
      rect.hclust(complete, input$colcount_cluster)
    } else if (input$radio_hclus == "AverageLinkage") {
      
      average <- hclust(euc, method = "average")
      plot(average, hang = -2, cex = 1, labels = clus_final2$target) 
      
      #count cluster
      rect.hclust(average, input$colcount_cluster)
    }
    
    
    
    
  }) #end of cetak hclus
  
  output$table_hclus <- renderTable({
    file_hclus <- input$file_hclus
    ext <- tools::file_ext(file_hclus$datapath)
    
    req(file_hclus)
    #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
    
    if (is.null(file_hclus))
      return(NULL)
    
    data7 = read.csv(file_hclus$datapath)
    head(data7, n=10)
    
  }, height = 500, width = 700) #end of hclus
  
  
  output$down_hclus <- downloadHandler(
    #specify filename
    filename = function() {
      #biplot.png
      #biplot.pdf
      paste("cluster","png", sep = ".")
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #  pdf(file)
      
      file_hclus <- input$file_hclus
      
      if (is.null(file_hclus))
        return(NULL)
      
      data6 = read.csv(file_hclus$datapath)
      
      #choose cluster column
      features = data6[, -(input$colcount_hclus)]
      
      #normization of categorical data
      features1 <- data.matrix(data.frame(unclass(features)))
      
      #target data to matrix
      target <- as.matrix(data6[, input$colcount_hclus])
      
      #combine
      clus_final <- data.frame(target, features1)
      
      #grouping
      clus_final2 <- clus_final %>%
        group_by(target) %>%
        dplyr::summarise(across(everything(), list(sum)))
      
      #normalization all of data
      data_nor <- scale(clus_final2[, -1], scale = TRUE)
      
      #count distance
      euc <- dist(data_nor, method = "euclidean")
      
      #method
      if(input$radio_hclus == "SingleLinkage") {
        
        single <- hclust(euc, method = "single")
        plot(single, hang = -2, cex = 1, labels = clus_final2$target) 
        
        #count cluster
        rect.hclust(single, input$colcount_cluster)
      } else if (input$radio_hclus == "CompleteLinkage") {
        
        complete <- hclust(euc, method = "complete")
        plot(complete, hang = -2, cex = 1, labels = clus_final2$target) 
        
        #count cluster
        rect.hclust(complete, input$colcount_cluster)
      } else if (input$radio_hclus == "AverageLinkage") {
        
        average <- hclust(euc, method = "average")
        plot(average, hang = -2, cex = 1, labels = clus_final2$target) 
        
        #count cluster
        rect.hclust(average, input$colcount_cluster)
      }
      
      dev.off()
    }
  ) #end of download button for hcluster
  
  
  output$summary5 <- renderPrint({
    file_hclus <- input$file_hclus
    ext <- tools::file_ext(file_hclus$datapath)
    
    req(file_hclus)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_hclus))
      return(NULL)
    
    data9 = read.csv(file_hclus$datapath)
    
    #choose cluster column
    features = data9[, -(input$colcount_hclus)]
    
    #normization of categorical data
    features1 <- data.matrix(data.frame(unclass(features)))
    
    #target data to matrix
    target <- as.matrix(data9[, input$colcount_hclus])
    
    #combine
    clus_final <- data.frame(target, features1)
    
    #grouping
    clus_final2 <- clus_final %>%
      group_by(target) %>%
      dplyr::summarise(across(everything(), list(sum)))
    
    #normalization all of data
    data_nor <- scale(clus_final2[, -1], scale = TRUE)
    
    #count distance
    euc <- dist(data_nor, method = "euclidean")
    
    row.names(data_nor) <- 1:nrow(data_nor)
    data_nor
    #method
    if(input$radio_hclus == "SingleLinkage") {
      single <- hclust(euc, method = "single")
      
      internal <- clValid((data_nor), nClust = 2:input$colcount_cluster, 
                          clMethods = "agnes", 
                          validation = "stability", 
                          metric = "euclidean",
                          method = "single")
      
    } else if (input$radio_hclus == "CompleteLinkage") {
      complete <- hclust(euc, method = "complete")
      
      
      internal <- clValid((data_nor), nClust = 2:input$colcount_cluster, 
                          clMethods = "agnes", 
                          validation = "stability", 
                          metric = "euclidean",
                          method = "complete")
      
    } else if (input$radio_hclus == "AverageLinkage") {
      average <- hclust(euc, method = "average")
      
      
      internal <- clValid((data_nor), nClust = 2:input$colcount_cluster, 
                          clMethods = "agnes", 
                          validation = "stability", 
                          metric = "euclidean",
                          method = "average")
      
    }
    
    sld_method <- input$radio_hclus
    print(paste("Method Selected", sld_method))
    
    summary(internal)
    
    d1 <- dist(data_nor[, -1])
    
    d2 <- cophenetic(hclust(euc, method = "single"))
    corrr <- cor(d1, d2)
    
    d3 <- cophenetic(hclust(euc, method = "complete"))
    corrr1 <- cor(d1, d3)
    
    d4 <- cophenetic(hclust(euc, method = "average"))
    corrr2 <- cor(d1, d4)
    
    tb_cor <- data.frame(corrr, corrr1, corrr2)
    colnames(tb_cor) <- c("Single Linkage", "Complete Linkage", "Average Linkage")
    tb_cor
    
  })#end of summary regression 1

})
