library(shiny)
<<<<<<< HEAD
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
=======
library(tidyverse)
library(gridExtra)
>>>>>>> 816e96c621999c2ee8985fa2ed40ec5a9414629b

anual_GCMs <- readRDS("data/anual_GCMs.rds")

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

ui <- fluidPage(
  titlePanel("Exploración de siete modelos climáticos globales (GCMs) para el cantón de Liberia, Guanacaste"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("GCMs", "Seleccionar GCMs:", 
                         choices = list("CCSM4" = "CCSM4",
                                        "CESM1-BGC" = "CESM1-BGC",
                                        "CNRM-CM5" = "CNRM-CM5",
                                        "MIROC5" = "MIROC5",
                                        "MPI-ESM-LR" = "MPI-ESM-LR",
                                        "MPI-ESM-MR" = "MPI-ESM-MR",
                                        "MRI-CGCM3" = "MRI-CGCM3"),
                       selected = c("CCSM4", "CESM1-BGC", "CNRM-CM5", "MIROC5", "MPI-ESM-LR",
                       "MPI-ESM-MR","MRI-CGCM3")),
      
      radioButtons("CPath", "Seleccionar esenario:",
                   choices = list("RCP 4.5" = "rcp45",
                                  "RCP 8.5" = "rcp85")),
      
<<<<<<< HEAD
      sliderInput("Year", "Seleccionar periodo de años:",
                  min = 2000, max = 2100, value = c(2010, 2050), step = 5, sep = ""),
      
      h5("Nota:"),
      p("- Los siete GCMs se escogieron con base en los mejores 30 GCMs del estudio 'Skill of CMIP5 climate models in reproducing 20th century basic climate features in Central America' (Hidalgo y Alfaro, 2015)."),
      p("- Datos tomados del set de datos NEX-GDDP, con resolución espacial de 0.25°."),
      br(),
      p("App elaborada en Shiny por Guillermo Durán, HIDROCEC-UNA.")
=======
      sliderInput("Year", "Seleccionar periodo de tiempo:",
                  min = 2000, max = 2100, value = c(2010, 2050), step = 5),
      
      p("Nota: Los valores de temperatura son los promedios anuales y los de lluvia son el total anual. Los siete GCMs se escogieron con base en los mejores 30 GCMs del estudio de Hidalgo y Alfaro (2015). Datos tomados del set de datos NEX-GDDP de la NASA, con resolución espacial de 0.25°.")
>>>>>>> 816e96c621999c2ee8985fa2ed40ec5a9414629b
      ),
    
    mainPanel(
              plotOutput("grafico1"))
    
  )
)


server <- function(input,output) {
  output$grafico1 <- renderPlot({
    seleccion <- anual_GCMs %>% 
      filter(Scenario == input$CPath & aNo >= input$Year[1] & aNo <= input$Year[2] & Modelo %in% input$GCMs)
    
    p1 <- ggplot(seleccion, aes(x = as.integer(aNo), y = tasmax)) + 
      geom_line(aes(color = Modelo)) + stat_smooth(method="loess", level=0.8) +
<<<<<<< HEAD
      labs(x = "Años", y = "Temperatura (C)") + 
      labs(
        title = paste("Promedio anual de temperatura máxima diaria")
      )
    p2 <- ggplot(seleccion, aes(x = as.integer(aNo), y = tasmin)) + 
      geom_line(aes(color = Modelo)) + stat_smooth(method="loess", level=0.8) +
      labs(x = "Años", y = "Temperatura (C)") +
      labs(
        title = paste("Promedio anual de temperatura mínima diaria")
      )
    p3 <- ggplot(seleccion, aes(x = as.integer(aNo), y = pr)) + 
      geom_line(aes(color = Modelo)) + stat_smooth(method="loess", level=0.8) +
      labs(x = "Años", y = "Lluvia (mm)") + 
      labs(
        title = paste("Total de precipitación anual")
      )
    
    grid_arrange_shared_legend(p1, p2, p3)
  }, width = "auto", height = 700)
  
  output$grafico2 <- renderPlot({
    
  })
=======
      labs(x = "", y = "Temp. máxima (C)") + 
      theme(axis.text.x = element_blank())
    p2 <- ggplot(seleccion, aes(x = as.integer(aNo), y = tasmin)) + 
      geom_line(aes(color = Modelo)) + stat_smooth(method="loess", level=0.8) +
      labs(x = "", y = "Temp. mínima (C)") +
      theme(axis.text.x = element_blank())
    p3 <- ggplot(seleccion, aes(x = as.integer(aNo), y = pr)) + 
      geom_line(aes(color = Modelo)) + stat_smooth(method="loess", level=0.8) +
      labs(x = "Años", y = "Lluvia (mm)")
    
    grid_arrange_shared_legend(p1, p2, p3)
  }, width = "auto", height = 700) 
>>>>>>> 816e96c621999c2ee8985fa2ed40ec5a9414629b
}

shinyApp(ui = ui, server = server)
