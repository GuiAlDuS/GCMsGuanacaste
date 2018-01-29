library(shiny)
anualGCMs <- readRDS("data/anual_GCMs.rds")

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
                       selected = NULL),
      
      radioButtons("CPath", "Seleccionar el concentration pathway",
                   choices = list("RCP 4.5" = "rcp45",
                                  "RCP 8.5" = "rcp85")),
      
      sliderInput("Year", "Seleccionar periodo de tiempo",
                  min = 1950, max = 2100, value = c(2000, 2050), step = 5)
      ),
    
    mainPanel("Valores del ensamble de GCMs",
              plotOutput("grafico"))
  )
)


server <- function(input,output) {
  
  output$grafico <- renderPlot({
    seleccion <- anual_GCMs %>% filter(Model == input$GCMs & Scenario == input$CPath)
    ggplot(seleccion, aes(x = aNo, y = tasmax)) + 
      geom_point(aes(color = input$GCMs))
  })
}

shinyApp(ui = ui, server = server)
