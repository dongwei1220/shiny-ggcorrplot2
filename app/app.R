###############################################################################
# Author: Wei Dong                                                            #
# Emial: 1369852697@qq.com                                                    #
# Github: https://github.com/dongwei1220                                      #
# Blog: http://bioinfomics.top/                                               #
# Team: https://hiplot.com.cn                                                 #
# Version: R-4.0.2                                                            #
# Date: 2020-09-16                                                            #
###############################################################################
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("shiny")
# install.packages("bs4Dash")
# install.packages("DT")
# install.packages("psych")
# install.packages("devtools")
# devtools::install_github("caijun/ggcorrplot2")

# load required packages
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bs4Dash))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(psych))
#suppressPackageStartupMessages(library(ggsci))
suppressPackageStartupMessages(library(ggcorrplot2))
#suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(RColorBrewer))
#library(colourpicker)

# remove environment objects
rm(list=ls())

# Shiny App
shiny::shinyApp(
  # Define UI for application that visualize a correlation matrix
  ui = bs4DashPage(
    old_school = FALSE,
    sidebar_min = TRUE,
    sidebar_collapsed = FALSE,
    #controlbar_collapsed = TRUE,
    #controlbar_overlay = TRUE,
    title = "Shiny ggcorrplot2",
    
    #=== 1.bs4DashSidebar ===
    sidebar = bs4DashSidebar(
      skin = "light",
      status = "primary",
      title = "Wei Dong",
      brandColor = "primary",
      url = "http://bioinfomics.top/",
      src = "https://s1.imagehub.cc/images/2020/09/02/avatar.jpg",
      elevation = 3,
      opacity = 0.8,
      
      #=== bs4SidebarMenu ===
      bs4SidebarMenu(
        bs4SidebarHeader(h6("Menus")),
        bs4SidebarMenuItem(
          "Base Correlation",
          tabName = "baseplot",
          icon = "photo"
        ),
        bs4SidebarMenuItem(
          "Mixed Correlation",
          tabName = "mixplot",
          icon = "sitemap"
        ),
        bs4SidebarMenuItem(
          "DataSet",
          tabName = "dataset",
          icon = "table"
        ),
        br(),
        fileInput(
          inputId = "infile",
          label = 'Choose Input File',
          multiple = F,
          accept = c('txt', '.tsv', ''),
          width = NULL,
          buttonLabel = "Browse",
          placeholder = "Input Data Matrix"
        ),
        selectInput(
          inputId = "correlation", 
          label = "Correlation:", 
          choices = c("col","row"), 
          selected = "col",
          multiple = F,
          width = NULL
        ),
        selectInput(
          inputId = "method", 
          label = "Correlation Method:", 
          choices = c("pearson","spearman","kendall"), 
          selected = "pearson",
          multiple = F,
          width = NULL
        ),
        selectInput(
          inputId = "adjust", 
          label = "Correlation Adjust:", 
          choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), 
          selected = "none",
          multiple = F,
          width = NULL
        ),
        selectInput(
          inputId = "color",
          label = "Color Palette:",
          choices = c("YlGnBu", "YlOrBr", "YlOrRd", "BrBG", "PuBuGn",
                      "PiYG", "PRGn", "RdYlBu", "RdYlGn", "Spectral"),
          selected = "Spectral",
          multiple = F,
          width = NULL
        )
      )
    ),
    
    #=== 2.bs4DashNavbar ===
    navbar = bs4DashNavbar(
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = "bars",
      controlbarIcon = "th",
      fixed = FALSE,
      leftUi = bs4DropdownMenu(
        show = FALSE,
        align = "left",
        status = "warning",
        menuIcon = "envelope-open",
        src = NULL
      ),
      rightUi = bs4DropdownMenu(
        show = FALSE,
        status = "danger",
        src = "https://github.com/dongwei1220",
        bs4DropdownMenuItem(
          message = "message 1",
          from = "Wei Dong",
          src = "https://bioinfomics.top/images/avatar.gif",
          time = "today",
          status = "danger",
          type = "message"
        )
      )
    ),
    
    #=== 3.bs4DashControlbar ===
    #controlbar = bs4DashControlbar(
    #  skin = "light",
    #  title = "My right sidebar",
    #  sliderInput(
    #    inputId = "obs",
    #    label = "Number of observations:",
    #    min = 0,
    #    max = 1000,
    #    value = 500
    #  )
    #),
    
    #=== 4.bs4DashFooter ===
    footer = bs4DashFooter(
      copyrights = a(href = "https://github.com/dongwei1220",
                     target = "_blank", "Copyrights: @Wei Dong"
      ),
      right_text = "2020"
    ),
    
    #=== 5.bs4DashBody ===
    body = bs4DashBody(
      bs4TabItems(
        #== 1.BasePlot tabItem
        bs4TabItem(
          tabName = "baseplot",
          fluidRow(
            bs4Card(
              style="padding:7px",
              inputId = NULL,
              title = "Base Correlation",
              # footer = "Canvas",
              width = 3,
              status = "primary",
              elevation = 1,
              solidHeader = T,
              headerBorder = T,
              # gradientColor = "primary",
              collapsible = F,
              closable = F,
              maximizable = T,
              # labelText = "Window Options",
              # labelTooltip = "Like Windows",
              overflow = F,
              enable_sidebar = F,
              fluidRow(
                bs4Card(
                  style="padding:7px;",
                  inputId = NULL,
                  title = "Plot Download",
                  # footer = "Canvas",
                  width = 12,
                  status = "primary",
                  elevation = 1,
                  solidHeader = T,
                  headerBorder = T,
                  # gradientColor = "primary",
                  collapsible = TRUE,
                  collapsed = T,
                  closable = F,
                  maximizable = T,
                  # labelText = "Window Options",
                  # labelTooltip = "Like Windows",
                  overflow = F,
                  # enable_sidebar = T,
                  # sidebar_background = "dark",
                  # dropdownMenu = dropdownItemList(
                  #     dropdownItem(url = "https://github.com/dongwei1220", name = "Github")
                  # ),
                  column(
                    width = 12,
                    selectInput(
                      inputId = "plot_format1",
                      label = "Image Format",
                      choices = c(
                        "PDF" = "pdf",
                        "SVG" = "svg",
                        "PNG" = "png",
                        "JPEG" = "jpeg",
                        "TIFF" = "tiff",
                        "BMP" = "bmp"
                      ),
                      selected = "pdf",
                      multiple = F,
                      width = NULL
                    ),
                    downloadButton(
                      outputId = "baseplot_download",
                      label = "Download",
                      class = NULL
                    )
                  )
                ),
                bs4Card(
                  style="padding:7px;",
                  inputId = NULL,
                  title = "Parameter Options",
                  footer = "Canvas",
                  width = 12,
                  status = "primary",
                  elevation = 1,
                  solidHeader = T,
                  headerBorder = T,
                  # gradientColor = "primary",
                  collapsible = TRUE,
                  closable = F,
                  maximizable = T,
                  # labelText = "Window Options",
                  # labelTooltip = "Like Windows",
                  overflow = F,
                  # enable_sidebar = T,
                  # sidebar_background = "dark",
                  # dropdownMenu = dropdownItemList(
                  #     dropdownItem(url = "https://github.com/dongwei1220", name = "Github")
                  # ),
                  column(
                    width = 12,
                    selectInput(
                      inputId = "shape",
                      label = "Shape",
                      choices = c("circle", "square", "ellipse", "number"),
                      selected = "circle",
                      multiple = F,
                      width = NULL
                    ),
                    selectInput(
                      inputId = "type",
                      label = "Type",
                      choices = c("full", "lower", "upper"),
                      selected = "full",
                      multiple = F,
                      width = NULL
                    ),
                    # radioButtons(
                    #   inputId = "show_diag1",
                    #   label = "Show.diag",
                    #   choices = c("TRUE","FALSE"),
                    #   selected = "TRUE",
                    #   inline = T,
                    #   width = NULL
                    # ),
                    radioButtons(
                      inputId = "pmat1",
                      label = "P.mat",
                      choices = c("TRUE","FALSE"),
                      selected = "FALSE",
                      inline = T,
                      width = NULL
                    ),
                    selectInput(
                      inputId = "sig_lvl1",
                      label = "Sig.lvl",
                      choices = c("0.05", "0.01", "0.001"),
                      selected = "0.05",
                      multiple = T,
                      width = NULL
                    ),
                    selectInput(
                      inputId = "insig1",
                      label = "Insig",
                      choices = c("pch", "blank", "label_sig"),
                      selected = "pch",
                      multiple = F,
                      width = NULL
                    ),
                    sliderInput(
                      inputId = "pch_cex1",
                      label = "Pch.cex",
                      min = 1,
                      max = 10,
                      value = 5,
                      step = 1,
                      round = T,
                      ticks = T,
                      animate = T,
                      width = NULL,
                      timeFormat = T
                    )
                  )
                )
              )
            ),
            bs4Card(
              style = "padding:5px;
                         background: #ffffff;
                         border-radius: 0px;
                         box-shadow: inset 9.91px 9.91px 20px #D9DADE, inset -9.91px -9.91px 20px #FFFFFF;",
              inputId = NULL,
              title = "Plots",
              footer = "Canvas",
              width = 9,
              status = "danger",
              elevation = 1,
              solidHeader = T,
              headerBorder = T,
              # gradientColor = "primary",
              collapsible = TRUE,
              closable = T,
              maximizable = T,
              labelText = "Window Options",
              labelTooltip = "Like Windows",
              overflow = F,
              enable_sidebar = T,
              sidebar_background = "dark",
              dropdownMenu = dropdownItemList(
                dropdownItem(url = "https://github.com/dongwei1220", name = "Github")
              ),
              plotOutput("baseplot",height = 800)
            )
          )
        ),
        
        #== 2.MixPlot tabItem
        bs4TabItem(
          tabName = "mixplot",
          fluidRow(
            bs4Card(
              style="padding:7px",
              inputId = NULL,
              title = "Mixed Correlation",
              # footer = "Canvas",
              width = 3,
              status = "primary",
              elevation = 1,
              solidHeader = T,
              headerBorder = T,
              # gradientColor = "primary",
              collapsible = F,
              closable = F,
              maximizable = T,
              # labelText = "Window Options",
              # labelTooltip = "Like Windows",
              overflow = F,
              enable_sidebar = F,
              fluidRow(
                bs4Card(
                  style="padding:7px;",
                  inputId = NULL,
                  title = "Plot Download",
                  # footer = "Canvas",
                  width = 12,
                  status = "primary",
                  elevation = 1,
                  solidHeader = T,
                  headerBorder = T,
                  # gradientColor = "primary",
                  collapsible = TRUE,
                  collapsed = T,
                  closable = F,
                  maximizable = T,
                  # labelText = "Window Options",
                  # labelTooltip = "Like Windows",
                  overflow = F,
                  # enable_sidebar = T,
                  # sidebar_background = "dark",
                  # dropdownMenu = dropdownItemList(
                  #     dropdownItem(url = "https://github.com/dongwei1220", name = "Github")
                  # ),
                  column(
                    width = 12,
                    selectInput(
                      inputId = "plot_format2",
                      label = "Image Format",
                      choices = c(
                        "PDF" = "pdf",
                        "SVG" = "svg",
                        "PNG" = "png",
                        "JPEG" = "jpeg",
                        "TIFF" = "tiff",
                        "BMP" = "bmp"
                      ),
                      selected = "pdf",
                      multiple = F,
                      width = NULL
                    ),
                    downloadButton(
                      outputId = "mixplot_download",
                      label = "Download",
                      class = NULL
                    )
                  )
                ),
                bs4Card(
                  style="padding:7px;",
                  inputId = NULL,
                  title = "Parameter Options",
                  footer = "Canvas",
                  width = 12,
                  status = "primary",
                  elevation = 1,
                  solidHeader = T,
                  headerBorder = T,
                  # gradientColor = "primary",
                  collapsible = TRUE,
                  closable = F,
                  maximizable = T,
                  # labelText = "Window Options",
                  # labelTooltip = "Like Windows",
                  overflow = F,
                  # enable_sidebar = T,
                  # sidebar_background = "dark",
                  # dropdownMenu = dropdownItemList(
                  #     dropdownItem(url = "https://github.com/dongwei1220", name = "Github")
                  # ),
                  column(
                    width = 12,
                    selectInput(
                      inputId = "upper",
                      label = "Upper Shape",
                      choices = c("circle", "square", "ellipse", "number"),
                      selected = "circle",
                      multiple = F,
                      width = NULL
                    ),
                    selectInput(
                      inputId = "lower",
                      label = "Lower Shape",
                      choices = c("circle", "square", "ellipse", "number"),
                      selected = "circle",
                      multiple = F,
                      width = NULL
                    ),
                    radioButtons(
                      inputId = "pmat2",
                      label = "P.mat",
                      choices = c("TRUE","FALSE"),
                      selected = "FALSE",
                      inline = T,
                      width = NULL
                    ),
                    selectInput(
                      inputId = "sig_lvl2",
                      label = "Sig.lvl",
                      choices = c("0.05", "0.01", "0.001"),
                      selected = "0.05",
                      multiple = T,
                      width = NULL
                    ),
                    selectInput(
                      inputId = "insig2",
                      label = "Insig",
                      choices = c("pch", "blank", "label_sig"),
                      selected = "pch",
                      multiple = F,
                      width = NULL
                    ),
                    sliderInput(
                      inputId = "pch_cex2",
                      label = "Pch.cex",
                      min = 1,
                      max = 10,
                      value = 5,
                      step = 1,
                      round = T,
                      ticks = T,
                      animate = T,
                      width = NULL,
                      timeFormat = T
                    )
                  )
                )
              )
            ),
            bs4Card(
              style = "padding:5px;
                         background: #ffffff;
                         border-radius: 0px;
                         box-shadow: inset 9.91px 9.91px 20px #D9DADE, inset -9.91px -9.91px 20px #FFFFFF;",
              inputId = NULL,
              title = "Plots",
              footer = "Canvas",
              width = 9,
              status = "danger",
              elevation = 1,
              solidHeader = T,
              headerBorder = T,
              # gradientColor = "primary",
              collapsible = TRUE,
              closable = T,
              maximizable = T,
              labelText = "Window Options",
              labelTooltip = "Like Windows",
              overflow = F,
              enable_sidebar = T,
              sidebar_background = "dark",
              dropdownMenu = dropdownItemList(
                dropdownItem(url = "https://github.com/dongwei1220", name = "Github")
              ),
              plotOutput("mixplot",height = 800)
            )
          )
        ),
        
        #== 3.DataSet tabItem
        bs4TabItem(
          tabName = "dataset",
          bs4Card(
            style="padding:20px;",
            inputId = NULL,
            title = "Author Information",
            footer = "Canvas",
            width = 12,
            status = "primary",
            elevation = 1,
            solidHeader = T,
            headerBorder = T,
            #gradientColor = "warning",
            collapsible = TRUE,
            closable = T,
            maximizable = T,
            labelText = "Window Options",
            labelTooltip = "Like Windows",
            overflow = T,
            enable_sidebar = T,
            #sidebar_background = "dark",
            dropdownMenu = dropdownItemList(
              dropdownItem(url = "https://github.com/dongwei1220", name = "Github")
            ),
            h5("Author: Wei Dong",style="font-weight:bold"),
            h5("Github: https://github.com/dongwei1220",style="font-weight:bold"),
            h5("Email: 1369852697@qq.com",style="font-weight:bold"),
            h5("Blog: http://bioinfomics.top/",style="font-weight:bold")
          ),
          fluidPage(
            DTOutput(outputId = "dataset")
          )
        )
      )
    )
  ),
  
  # Define server logic required to draw gene arrow maps
  server = (function(input, output) {
    
    InputData <- reactive({
      if (is.null(input$infile)){
        data <- read.table("data/demo.txt",header = T,row.names = 1,
                           sep = "\t",check.names = F,stringsAsFactors = F)
      }else{
        data <- read.table(input$infile$datapath,header = T,row.names = 1,
                           sep = "\t",check.names = F,stringsAsFactors = F)
      }
      data <- as.matrix(data)
      return(data)
    })
    
    #############################################
    # output plot and download
    #############################################
    ### 1.BasePlot ###
    output$baseplot <- renderPlot({
      data <- InputData()
      
      # calculate the correlation
      if(input$correlation == "col"){
        ct <- corr.test(data, adjust = input$adjust,method = input$method)
        corr <- ct$r
        p.mat <- ct$p
      }else{
        data <- t(data)
        ct <- corr.test(data, adjust = input$adjust,method = input$method)
        corr <- ct$r
        p.mat <- ct$p
      }
      
      if(input$pmat1){
        p <- ggcorrplot(
          corr = corr,
          method = input$shape,
          type = input$type,
          p.mat = p.mat,
          sig.lvl = input$sig_lvl1,
          number.digits = 2,
          #show.diag = input$show_diag1,
          insig = input$insig1,
          pch = 4,
          pch.cex = input$pch_cex1
        )
        p + scale_color_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
          scale_fill_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
          theme(legend.title = element_blank(), legend.key.size = unit(1.2,"cm"))
      }else{
        p <- ggcorrplot(
          corr = corr,
          method = input$shape,
          type = input$type,
          number.digits = 2,
          #show.diag = input$show_diag1
        )
        p + scale_color_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
          scale_fill_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
          theme(legend.title = element_blank(),legend.key.size = unit(1.2,"cm"))
      }
    })
    
    # baseplot download
    output$baseplot_download <- downloadHandler(
      filename = function(){
        paste0("BasePlot-",Sys.Date(),".",input$plot_format1)
      },
      content = function(file){
        data <- InputData()
        
        if(input$correlation == "col"){
          ct <- corr.test(data, adjust = input$adjust,method = input$method)
          corr <- ct$r
          p.mat <- ct$p
        }else{
          data <- t(data)
          ct <- corr.test(data, adjust = input$adjust,method = input$method)
          corr <- ct$r
          p.mat <- ct$p
        }
        
        if (input$plot_format1 == "pdf"){
          pdf(file = file,width = 12, height = 8,
              family = "Times")
        }else if (input$plot_format1 == "svg"){
          svg(file = file,width = 12, height = 8,
              family = "Times")
        }else if (input$plot_format1 == "png"){
          png(file = file,width = 1000, height = 800,
              family = "Times")
        }else if (input$plot_format1 == "tiff"){
          tiff(file = file,width = 1000, height = 800,
               family = "Times")
        }else if (input$plot_format1 == "bmp"){
          bmp(file = file,width = 1000, height = 800,
              family = "Times")
        }else if (input$plot_format1 == "jpeg"){
          jpeg(file = file,width = 1000, height = 800,
               family = "Times")
        }
        if(input$pmat1){
          p <- ggcorrplot(
            corr = corr,
            method = input$shape,
            type = input$type,
            p.mat = p.mat,
            sig.lvl = input$sig_lvl1,
            number.digits = 2,
            #show.diag = input$show_diag1,
            insig = input$insig1,
            pch = 4,
            pch.cex = input$pch_cex1
          )
          plot <- p + scale_color_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
            scale_fill_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
            theme(legend.title = element_blank(), legend.key.size = unit(1.2,"cm"))
        }else{
          p <- ggcorrplot(
            corr = corr,
            method = input$shape,
            type = input$type,
            number.digits = 2,
            #show.diag = input$show_diag1
          )
          plot <- p + scale_color_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
            scale_fill_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
            theme(legend.title = element_blank(), legend.key.size = unit(1.2,"cm"))
        }
        print(plot)
        dev.off()
      })
    
    ### 2.MixPlot ###
    output$mixplot <- renderPlot({
      data <- InputData()
      
      if(input$correlation == "col"){
        ct <- corr.test(data, adjust = input$adjust,method = input$method)
        corr <- ct$r
        p.mat <- ct$p
      }else{
        data <- t(data)
        ct <- corr.test(data, adjust = input$adjust,method = input$method)
        corr <- ct$r
        p.mat <- ct$p
      }
      
      if(input$pmat2){
        p <- ggcorrplot.mixed(
          corr,
          upper = input$upper,
          lower = input$lower,
          p.mat = p.mat,
          sig.lvl = input$sig_lvl2,
          number.digits = 2,
          insig = input$insig2,
          pch = 4,
          pch.cex = input$pch_cex2
        )
        p + scale_color_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
          scale_fill_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
          theme(legend.title = element_blank(), legend.key.size = unit(1.2,"cm"))
      }else{
        p <- ggcorrplot.mixed(
          corr,
          upper = input$upper,
          lower = input$lower,
          number.digits = 2
        )
        p + scale_color_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
          scale_fill_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
          theme(legend.title = element_blank(), legend.key.size = unit(1.2,"cm"))
      }
    })
    
    # mixplot download
    output$mixplot_download <- downloadHandler(
      filename = function(){
        paste0("MixPlot-",Sys.Date(),".",input$plot_format2)
      },
      content = function(file){
        data <- InputData()
        
        if(input$correlation == "col"){
          ct <- corr.test(data, adjust = input$adjust,method = input$method)
          corr <- ct$r
          p.mat <- ct$p
        }else{
          data <- t(data)
          ct <- corr.test(data, adjust = input$adjust,method = input$method)
          corr <- ct$r
          p.mat <- ct$p
        }
        
        if (input$plot_format2 == "pdf"){
          pdf(file = file,width = 12, height = 8,
              family = "Times")
        }else if (input$plot_format2 == "svg"){
          svg(file = file,width = 12, height = 8,
              family = "Times")
        }else if (input$plot_format2 == "png"){
          png(file = file,width = 1000, height = 800,
              family = "Times")
        }else if (input$plot_format2 == "tiff"){
          tiff(file = file,width = 1000, height = 800,
               family = "Times")
        }else if (input$plot_format2 == "bmp"){
          bmp(file = file,width = 1000, height = 800,
              family = "Times")
        }else if (input$plot_format2 == "jpeg"){
          jpeg(file = file,width = 1000, height = 800,
               family = "Times")
        }
        if(input$pmat2){
          p <- ggcorrplot.mixed(
            corr,
            upper = input$upper,
            lower = input$lower,
            p.mat = p.mat,
            sig.lvl = input$sig_lvl2,
            number.digits = 2,
            insig = input$insig2,
            pch = 4,
            pch.cex = input$pch_cex2
          )
          plot <- p + scale_color_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
            scale_fill_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
            theme(legend.title = element_blank(), legend.key.size = unit(1.2,"cm"))
        }else{
          p <- ggcorrplot.mixed(
            corr,
            upper = input$upper,
            lower = input$lower,
            number.digits = 2
          )
          plot <- p + scale_color_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
            scale_fill_gradientn(colors = rev(brewer.pal(6,input$color)), limits = c(-1, 1)) +
            theme(legend.title = element_blank(), legend.key.size = unit(1.2,"cm"))
        }
        print(plot)
        dev.off()
      })
    
    ### 3.DataSet ###
    output$dataset <- renderDT({
      data <- InputData()
      data
    })
  })
)
