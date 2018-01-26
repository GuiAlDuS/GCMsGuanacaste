library(shiny)

ui <- fluidPage(
  titlePanel("Exploración de siete GCMSs para el cantón de Liberia, Guanacaste"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("GCMs", "Seleccionar GCMs para graficar", 
                         choices = list("GCM 1" = "GCM1",
                                        "GCM 2" = "GCM2",
                                        "GCM 3" = "GCM3",
                                        "GCM 4" = "GCM4"),
                       selected = NULL),
      
      radioButtons("CPath", "Seleccionar el concentration pathway",
                   choices = list("RCP4.5" = "RCP4.5",
                                  "RCP8.5" = "RCP8.5")),
      
      sliderInput("Year", "Seleccionar periodo de tiempo",
                  min = 1950, max = 2100, value = c(2000, 2050), step = 5)
      ),
    
    mainPanel("Valores del ensamble de GCMs",
              plotOutput("grafico"))
  )
)


server <- function(input,output) {
  
}

shinyApp(ui = ui, server = server)
