library(shiny)
library(tidyverse)
library(gridExtra)

anual_GCMs <- readRDS("data/anual_GCMs.rds")

ui <- fluidPage(
  titlePanel("Exploración de siete GCMSs para el cantón de Liberia, Guanacaste"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("GCMs", "Seleccionar GCMs para graficar", 
                         choices = list("CCSM4" = "CCSM4",
                                        "CESM1-BGC" = "CESM1-BGC",
                                        "CNRM-CM5" = "CNRM-CM5",
                                        "MIROC5" = "MIROC5",
                                        "MPI-ESM-LR" = "MPI-ESM-LR",
                                        "MPI-ESM-MR" = "MPI-ESM-MR",
                                        "MRI-CGCM3" = "MRI-CGCM3"),
                       selected = c("CCSM4", "CESM1-BGC", "CNRM-CM5", "MIROC5", "MPI-ESM-LR",
                       "MPI-ESM-MR","MRI-CGCM3")),
      
      radioButtons("CPath", "Seleccionar el concentration pathway",
                   choices = list("RCP 4.5" = "rcp45",
                                  "RCP 8.5" = "rcp85")),
      
      sliderInput("Year", "Seleccionar periodo de tiempo",
                  min = 2000, max = 2100, value = c(2010, 2050), step = 5)
      ),
    
    mainPanel("Valores de los ensambles de GCMs",
              plotOutput("graficoTmax"))
    
  )
)


server <- function(input,output) {
  output$graficoTmax <- renderPlot({
    seleccion <- anual_GCMs %>% 
      filter(Scenario == input$CPath & aNo >= input$Year[1] & aNo <= input$Year[2] & Model %in% input$GCMs)
    p1 <- ggplot(seleccion, aes(x = as.integer(aNo), y = tasmax)) + 
      geom_line(aes(color = Model)) + stat_smooth(method="loess", level=0.8) +
      labs(x = "Años", y = "Temp. máxima (C)")
    p2 <- ggplot(seleccion, aes(x = as.integer(aNo), y = tasmin)) + 
      geom_line(aes(color = Model)) + stat_smooth(method="loess", level=0.8) +
      labs(x = "Años", y = "Temp. mínima (C)")
    p3 <- ggplot(seleccion, aes(x = as.integer(aNo), y = pr)) + 
      geom_line(aes(color = Model)) + stat_smooth(method="loess", level=0.8) +
      labs(x = "Años", y = "Lluvia (mm)")
    grid.arrange(p1,p2,p3, ncol=1)
  }) 
}

shinyApp(ui = ui, server = server)
