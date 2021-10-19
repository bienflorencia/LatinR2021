library(tidyverse)
library(sf)
library(leaflet)
library(shiny)

source('R/funciones.R', local = TRUE, encoding = 'UTF-8')
mis_etiquetas <- readLines('data/mis_etiquetas.txt')
Uruguay <- readRDS('data/Uruguay_map.rds')
datos <- readRDS('data/datos.rds')
# Hay que ver qué hacer con algunos grupos. Por lo menos con Chromista y
# Protozoa da errores
grac <- c("Todos", "Aves", "Mammalia", "Amphibia", "Animalia", "Plantae",
          "Mollusca", "Insecta", "Arachnida", "Fungi", "Reptilia", 
          "Actinopterygii", "Chromista", "Protozoa")


# ui <- bootstrapPage(
#   div(
#     class="outer",
#     leafletOutput("map", width="100%", height="100%"),
#     absolutePanel(id = 'controls', class = 'panel panel-default',
#                   top = 75, right = 55, width = 350, )
#   )
# )

ui <- fluidPage(
  
  # Titulo
  fluidPage(titlePanel("PRUEBA")),
  sidebarLayout(
    position = 'right',
    mainPanel = mainPanel(
      leafletOutput(outputId = 'map', height = '80vh'),
      # Link a datos de origen
      helpText("BLABLA",
               a("blabla",
                 href = "https://github.com/bienflorencia/LatinR2021"),
               "." )
    ),
    sidebarPanel = sidebarPanel(
      selectInput('grupo', label = NULL, choices = grac, selected = "Todos")
    )
))
  
server <- function(input, output) {
  
  # Filtrar los datos para quedarnos sólo con los del año seleccionado:
  # datos_anio <- reactive({
  #   datos %>%
  #     filter(anio == input$ano) %>% 
  #     arrange(nomdepto)
  #   # Este objeto se usará tanto para el popup del mapa como para la tabla.
  # })
  
  datos_grupo <- reactive({
    d <- datos %>% 
      data_filter(grupo = input$grupo) %>% 
      dplyr::mutate(etiqueta = domain2labels(indice_prioridad, 
                                             labels = mis_etiquetas))
  }) 
  
  # Construcción del mapa:  
  output$map <- renderLeaflet({
    
    d <- datos_grupo()
    print(table(d$etiqueta))
    pal <- colorFactor('RdYlBu', d$etiqueta, reverse = TRUE)
    
    mapa <- 
      leaflet(d) %>%
      clearBounds() %>% 
      addTiles(group = "Open Street Map") %>% 
      addProviderTiles(providers$OpenTopoMap,
                       options = providerTileOptions(noWrap = TRUE),
                       group = 'Open Topo Map') %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE),
                       group = 'Imagen') %>%
      addPolygons(
        weight = .5, 
        color = 'white',
        fillColor = ~pal(etiqueta),
        fillOpacity = .5,
        popup = ~mkpopup(grid_id, etiqueta, input$grupo),
        highlightOptions = highlightOptions(color = "white", 
                                            weight = 5, 
                                            fillOpacity = .8,
                                            bringToFront = TRUE),
        group = "Grilla"
      ) %>% 
      # Control de capas:
      addLayersControl(
        baseGroups = c("Open Street Map", "Open Topo Map", "Imagen"),
        overlayGroups = c("Grilla"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      # Leyenda de índice de prioridad
      addLegend(pal = pal, 
                # values = ~ranking, 
                values = ~etiqueta, 
                opacity = .5,
                position = 'bottomright', 
                title = 'Prioridad',
                group = 'Grilla')
    
    print(mapa)
  })
  
  # Preparar la tabla para la app:
  # output$tabla <- DT::renderDataTable({
  #   datos_anio() %>% 
  #     select(-anio, Departamento = nomdepto, Gini = gini) %>% 
  #   DT::datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 19), 
  #                                                  pageLength = 19)) %>% 
  #     formatPercentage('Gini', 2)
  #     
  # })
}

shinyApp(ui = ui, server = server)
