library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

anual_GCMs <- readRDS("data/anual_GCMs.rds")
anual_GCMs$aNo <- as.integer(anual_GCMs$aNo)

#funciones
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

grafico <- function(selec_aNo, selec, s_Y) {
  ggplot() + 
    geom_line(data = selec_aNo, aes_string(x = "aNo", y = s_Y, group = "Modelo"), colour = alpha("grey", 0.7)) + 
    geom_line(data = selec, aes_string(x = "aNo", y = s_Y, colour = "Modelo"))
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
                       selected = NULL),
      
      radioButtons("CPath", "Seleccionar esenario:",
                   choices = list("RCP 4.5" = "rcp45",
                                  "RCP 8.5" = "rcp85")),
      
      sliderInput("Year", "Seleccionar periodo de años:",
                  min = 2000, max = 2100, value = c(2010, 2050), step = 5, sep = ""),
      
      checkboxInput("loess", "Mostrar línea de tendencia", value = F),
      
      h5("Nota:"),
      p("- Los siete GCMs se escogieron con base en los mejores 30 GCMs del estudio 'Skill of CMIP5 climate models in reproducing 20th century basic climate features in Central America' de Hidalgo y Alfaro (2015)."),
      p("- Datos tomados del set de datos NEX-GDDP, con resolución espacial de 0.25°."),
      br(),
      p("App elaborada en Shiny por Guillermo Durán, HIDROCEC-UNA.")
      ),
    
    mainPanel(
              plotOutput("grafico1"))
    
  )
)


server <- function(input,output) {
  output$grafico1 <- renderPlot({
    seleccionanual <- anual_GCMs %>% 
      filter(Scenario == input$CPath & aNo >= input$Year[1] & aNo <= input$Year[2])
    seleccion <- seleccionanual %>% 
      filter(Modelo %in% input$GCMs)
    
    p1 <- grafico(seleccionanual, seleccion, "tasmax") +
      stat_smooth(method="loess", level=0.8) +
      labs(x = "Años", y = "Temperatura (C)") + 
      labs(
        title = paste("Promedio de temperatura máxima diaria")
      )
    p2 <- grafico(seleccionanual, seleccion, "tasmin") +
      labs(x = "Años", y = "Temperatura (C)") +
      labs(
        title = paste("Promedio de temperatura mínima diaria")
      )
    p3 <- grafico(seleccionanual, seleccion, "pr") +
      labs(x = "Años", y = "Lluvia (mm)") + 
      labs(
        title = paste("Total de precipitación anual")
      )
    p4 <- grafico(seleccionanual, seleccion, "tasmax_dia") +
      labs(x = "Años", y = "Temperatura (C)") + 
      labs(
        title = paste("Máxima temperatura diaria por año")
      )
    p5 <- grafico(seleccionanual, seleccion, "tasmin_max_dia") +
      labs(x = "Años", y = "Temperatura (C)") +
      labs(
        title = paste("Máxima temperatura mínima por año")
      )
    p6 <- grafico(seleccionanual, seleccion, "prmax_dia") + 
      stat_smooth(method="loess", level=0.8) +
      labs(x = "Años", y = "Lluvia (mm)") + 
      labs(
        title = paste("Máximo de lluvia diario en cada año")
      )
    
    grid_arrange_shared_legend(p1, p4, p2, p5, p3, p6)
  }, width = "auto", height = 750)
}

shinyApp(ui = ui, server = server)
