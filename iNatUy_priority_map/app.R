# GLOBAL ----
library(tidyverse)
library(sf)
library(leaflet)
library(shiny)

# Documentación para leaflet en shiny:
# https://rstudio.github.io/leaflet/shiny.html

source('R/funciones.R', local = TRUE, encoding = 'UTF-8')
mis_etiquetas <- readLines('data/mis_etiquetas.txt')
# Uruguay <- readRDS('data/Uruguay_map.rds')
# grid_join <- readRDS('data/grid_join.rds')
datos <- readRDS('data/datos.rds')
# Hay que ver qué hacer con algunos grupos. Por lo menos con Chromista y
# Protozoa da errores
grac <- c("Todos", "Aves", "Mammalia", "Amphibia", "Animalia", "Plantae",
          "Mollusca", "Insecta", "Arachnida", "Fungi", "Reptilia", 
          "Actinopterygii"
          # "Chromista", "Protozoa")
)

# spp_counts <- grid_join %>% 
#   sf::st_drop_geometry() %>% 
#   count(grid_id, species) %>% 
#   arrange(grid_id, desc(n))

mapa_base <- leaflet() %>%
  setView(-51.4, -32.6, zoom = 6) %>% 
  # fitBounds(-58.8, -35.2, -52.8, -29.9) %>% 
  addProviderTiles(providers$OpenTopoMap,
                   options = providerTileOptions(noWrap = TRUE),
                   group = 'Open Topo Map') %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(noWrap = TRUE),
                   group = 'Imagen') %>% 
  addTiles(group = "Open Street Map") %>% 
  # Control de capas:
  addLayersControl(
    position = 'bottomleft',
    baseGroups = c("Open Street Map", "Open Topo Map", "Imagen"),
    overlayGroups = c("Grilla"),
    options = layersControlOptions(collapsed = FALSE)
  )

# UI ----
ui <- bootstrapPage(
  tags$head(includeCSS('styles.css')),
  leafletOutput(outputId = 'map', height = '100vh'),
  div(
    class = 'outer',
    absolutePanel(
      width = 400, height = 'auto',
      class = "panel panel-default",
      id = 'controls',
      draggable = TRUE,
      top = 10, right = 10,
      selectInput('grupo', label = 'Grupo', choices = grac, selected = "Todos"),
      tabsetPanel(type = "pills",
                  tabPanel("Celda", htmlOutput('info_celda')),
                  tabPanel("Metricas (toy example)", textOutput("metricas")),
                  tabPanel("graficos (toy example)", plotOutput("graficos"))
      )
    )))
# Link a datos de origen
# helpText("Repositorio",
#          a("Enlace al repositorio",
#            href = "https://github.com/bienflorencia/LatinR2021"),
#          "." )

# SERVER ----
server <- function(input, output) {
  
  datos_grupo <- reactive({
    d <- datos %>% 
      data_filter(grupo = input$grupo) %>% 
      dplyr::mutate(etiqueta = domain2labels(indice_prioridad, 
                                             labels = mis_etiquetas))
  }) 
  
  # Construcción del mapa:  
  output$map <- renderLeaflet({
    mapa_base 
  })
  
  colorpal <- reactive({
    colorFactor('RdYlBu', datos_grupo()$etiqueta, reverse = TRUE)
  })
  
  
  datos_celda <- reactive({
    gid <- input$map_shape_click$id
    datos_grupo() %>% 
      sf::st_drop_geometry() %>% 
      dplyr::filter(grid_id == gid)
  })  
  
  # Pestañas -----
  output$info_celda <- renderText({
    if (is.null(input$map_shape_click$id))
      return("No hay celda seleccionada")
    # # Debug:
    # dc <- datos %>% 
    #   data_filter('Todos') %>% 
    #   filter(grid_id == 178) %>% 
    #   sf::st_drop_geometry()
    out <- tags$h4("Información sobre la celda seleccionada:")
    dc <- datos_celda()
    
    out <- paste(
      sep = '</br>',
      out,
      paste0('ID de la Celda: ', input$map_shape_click$id),
      paste0('Indice de prioridad: ', round(dc$indice_prioridad, 3)),
      paste0('Riqueza de especies: ', as.integer(dc$species_richness)),
      paste0('# Especies nuevas registradas en el último año: ', 
             dc$n_new_species_last_year),
      paste0('% Especies nuevas en último año, en relación a la riqueza: ', 
             scales::percent(dc$prop_new_species_last_year))
    )
    HTML(out)
  })
  
  output$metricas <- renderText({
    "Documentación sobre las métricas usadas"
  })
  
  output$graficos <- renderCachedPlot(
    cacheKeyExpr = input$grupo, {
      plot(cars)
    })
  
  grid_id <- reactiveValues()
  
  # Map input (debug)-----
  # observe({
  #   x <- input$map_shape_click
  #   print(x)
  #   
  # })
  
  observe({
    # print(table(d$etiqueta)) # debug
    pal <- colorpal()
    
    leafletProxy("map", data = datos_grupo()) %>%
      clearShapes() %>% 
      addPolygons(
        noClip = TRUE,
        layerId = ~grid_id,
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
      ) 
  })
  
  observe({
    d <- datos_grupo()
    proxy <- leafletProxy("map", data = d)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (isTruthy(input$grupo)) {
      pal <- colorpal()
      proxy %>% 
        addLegend(
          data = d, 
          pal = pal,
          values = ~etiqueta,
          opacity = .5,
          position = 'bottomright',
          title = 'Prioridad',
          group = 'Grilla'
        )
    }
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

# Run app ----
shinyApp(ui = ui, server = server)
