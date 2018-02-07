library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

anual_GCMs <- readRDS("data/anual_GCMs.rds")
mensual_GCMs <- readRDS("data/mensual_GCMs.rds")
anual_GCMs$aNo <- as.integer(anual_GCMs$aNo)
mensual_GCMs$aNo <- as.integer(mensual_GCMs$aNo)

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
  titlePanel("Exploración gráfica de siete modelos climáticos globales (GCMs) para el cantón de Liberia, Guanacaste, Costa Rica."),
  
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
      
      checkboxInput("loess", "Mostrar línea de tendencia para los GCMs seleccionados.", value = F),
      br(),
      h5("Nota:"),
      p("- Los siete GCMs se escogieron con base en los mejores 30 GCMs del estudio 'Skill of CMIP5 climate models in reproducing 20th century basic climate features in Central America' de Hidalgo y Alfaro (2015)."),
      p("- Datos tomados del set de datos NEX-GDDP, con resolución espacial de 0.25°."),
      p("- Línea de tendencia calculada por medio de una regresión local (LOESS)."),
      br(),
      p("App elaborada en R y Shiny por Guillermo Durán, HIDROCEC-UNA.")
      ),
    
    mainPanel(
              plotOutput("grafico1"))
    
  )
)

server <- function(input,output) {
  output$grafico1 <- renderPlot({

#funciones de selección
    seleccionanual <- anual_GCMs %>% 
      filter(Scenario == input$CPath & aNo >= input$Year[1] & aNo <= input$Year[2])
    seleccion <- seleccionanual %>% 
      filter(Modelo %in% input$GCMs)
    
    seleccionanual_mes <- mensual_GCMs %>% 
      filter(Scenario == input$CPath & aNo >= input$Year[1] & aNo <= input$Year[2])
    seleccion_mes <- seleccionanual_mes %>% 
      filter(Modelo %in% input$GCMs)
    
#funciones generales de gráfico 
    grafico <- function(s_Y) {
      ggplot() + 
        geom_line(data = seleccionanual, aes_string(x = "aNo", y = s_Y, group = "Modelo"), colour = alpha("grey", 0.7)) + 
        geom_line(data = seleccion, aes_string(x = "aNo", y = s_Y, colour = "Modelo"))
    }
    
    grafico_mes <- function(s_Y) {
      ggplot() + 
        geom_violin(data = seleccionanual_mes, aes_string(x = "mes", y = s_Y), colour = alpha("grey", 0.7)) +
        geom_violin(data = seleccion_mes, aes_string(x = "mes", y = s_Y, fill = "Modelo"))
    }
    #selección sin GCMs
    if (is.null(input$GCMs)){
      #todos los gráficos en gris
      p1 <- grafico("tasmax") +
        labs(x = "Años", y = "Temperatura (C)") + 
        labs(
          title = paste("Promedio de temperatura máxima diaria")
        )
      p2 <- grafico("tasmin") +
        labs(x = "Años", y = "Temperatura (C)") +
        labs(
          title = paste("Promedio de temperatura mínima diaria")
        )
      p3 <- grafico("pr") +
        labs(x = "Años", y = "Lluvia (mm)") + 
        labs(
          title = paste("Total de precipitación anual")
        )
      p4 <- grafico("TempDif") +
        labs(x = "Años", y = "Grados celsius (C)") + 
        labs(
          title = paste("Promedio del rango de temperatura diaria")
        )
      p5 <- grafico_mes("tasmax_mes") +
        labs(x = "Mes", y = "Temperatura (C)") +
        labs(
          title = paste("Temperaturas máximas mensuales")
        )
      p6 <- grafico_mes("tasmin_mes") + 
        labs(x = "Mes", y = "Temperatura (C)") +
        labs(
          title = paste("Temperaturas mínimas mensuales")
        )
      p7 <- grafico_mes("pr_mes") +
        labs(x = "Mes", y = "Lluvia (mm)") +
        labs(
          title = paste("Total de lluvias mensuales")
        )
      grid.arrange(p1, p2, p4, p3, p5, p6, p7)
    } else if (input$loess == T) {  #con tendencia seleccionada
      p1 <- grafico("tasmax") +
        stat_smooth(data=seleccion, method="loess", level=0.8, se = F, aes_string(x = "aNo", y = "tasmax")) +
        labs(x = "Años", y = "Temperatura (C)") + 
        labs(
          title = paste("Promedio de temperatura máxima diaria")
        )
      p2 <- grafico("tasmin") +
        labs(x = "Años", y = "Temperatura (C)") +
        stat_smooth(data=seleccion, method="loess", level=0.8, se = F, aes_string(x = "aNo", y = "tasmin")) +
        labs(
          title = paste("Promedio de temperatura mínima diaria")
        )
      p3 <- grafico("pr") +
        labs(x = "Años", y = "Lluvia (mm)") + 
        stat_smooth(data=seleccion, method="loess", level=0.8, se = F, aes_string(x = "aNo", y = "pr")) +
        labs(
          title = paste("Total de precipitación anual")
        )
      p4 <- grafico("TempDif") +
        stat_smooth(data=seleccion, method="loess", level=0.8, se = F, aes_string(x = "aNo", y = "TempDif")) +
        labs(x = "Años", y = "Grados celsius (C)") + 
        labs(
          title = paste("Promedio del rango de temperatura diaria")
        )
      p5 <- grafico_mes("tasmax_mes") +
        labs(x = "Mes", y = "Temperatura (C)") +
        labs(
          title = paste("Temperaturas máximas mensuales")
        )
      p6 <- grafico_mes("tasmin_mes") + 
        labs(x = "Mes", y = "Temperatura (C)") +
        labs(
          title = paste("Temperaturas mínimas mensuales")
        )
      p7 <- grafico_mes("pr_mes") +
        labs(x = "Mes", y = "Lluvia (mm)") +
        labs(
          title = paste("Total de lluvias mensuales")
        )
      grid_arrange_shared_legend(p1, p2, p4, p3, p5, p6, p7)
    } else {  #sin tendencia
    p1 <- grafico("tasmax") +
      stat_smooth(method="loess", level=0.8) +
      labs(x = "Años", y = "Temperatura (C)") + 
      labs(
        title = paste("Promedio de temperatura máxima diaria")
      )
    p2 <- grafico("tasmin") +
      labs(x = "Años", y = "Temperatura (C)") +
      labs(
        title = paste("Promedio de temperatura mínima diaria")
      )
    p3 <- grafico("pr") +
      labs(x = "Años", y = "Lluvia (mm)") + 
      labs(
        title = paste("Total de precipitación anual")
      )
    p4 <- grafico("TempDif") +
      labs(x = "Años", y = "Grados celsius (C)") + 
      labs(
        title = paste("Promedio del rango de temperatura diaria")
      )
    p5 <- grafico_mes("tasmax_mes") +
      labs(x = "Mes", y = "Temperatura (C)") +
      labs(
        title = paste("Temperaturas máximas mensuales")
      )
    p6 <- grafico_mes("tasmin_mes") + 
      labs(x = "Mes", y = "Temperatura (C)") +
      labs(
        title = paste("Temperaturas mínimas mensuales")
      )
    p7 <- grafico_mes("pr_mes") +
      labs(x = "Mes", y = "Lluvia (mm)") +
      labs(
        title = paste("Total de lluvias mensuales")
      )
    grid_arrange_shared_legend(p1, p2, p4, p3, p5, p6, p7)
    }
    },width = "auto", height = 700)
}

shinyApp(ui = ui, server = server)