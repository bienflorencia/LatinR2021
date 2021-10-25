# GLOBAL ----
library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
# Documentación para leaflet en shiny:
# https://rstudio.github.io/leaflet/shiny.html

# Demo:
# https://jumanbar.shinyapps.io/iNatUy_priority_map/

mis_etiquetas <- readLines('data/mis_etiquetas.txt')
datos <- readRDS('data/datos.rds')

# Grupos (iconic_taxon_name):
# 
# # Código para obtener grac (ordenados por cantidad de registros):
# datos %>% 
#   dplyr::filter(iconic_taxon_name == input$grupo) %>% 
#   sf::st_drop_geometry() %>%
#   group_by(iconic_taxon_name) %>%
#   tally(n_registros) %>%
#   arrange(desc(n)) %>%
#   pull(1) %>%
#   setNames(NULL) %>%
#   dput

grac <- c("Todos", "Plantae", "Aves", "Insecta", "Arachnida", "Reptilia", 
          "Amphibia", "Mammalia", "Fungi", "Mollusca", "Animalia", 
          "Actinopterygii", "Protozoa")
# "Chromista", "Protozoa")

source('R/funciones.R', local = TRUE, encoding = 'UTF-8')

mapa_base <- leaflet() %>%
  setView(-51.4, -32.6, zoom = 6) %>% 
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
ui <- navbarPage(
  title = 'Example', theme = shinythemes::shinytheme('sandstone'),
  tabPanel("Map", 
           div(class = 'outer',
               tags$head(includeCSS('styles.css')),
               leafletOutput('map', height = '91.9vh'),
               absolutePanel(
                 id = 'controls',
                 class = "panel panel-default",
                 fixed = TRUE,
                 top = 75, right = 10, left = "auto", bottom = "auto",
                 width = 400, height = 'auto',
                 draggable = TRUE,
                 selectInput('grupo', label = 'Grupo',
                             choices = grac,
                             selected = "Todos")
                 , tags$hr()
                 , tabsetPanel(type = "pills",
                               tabPanel("Celda", htmlOutput('info_celda')),
                               # tabPanel("Metricas", textOutput("metricas")),
                               tabPanel("graficos",
                                        # h4("Ejemplo de juguete..."),
                                        plotOutput("graficos")))
                 )
               )
           )
  ,     tabPanel("Índice de Prioridad"
                 # , "sfdadsfasfsdf"
                 # , includeHTML('www/indice_prioridad.html')
                 , includeMarkdown('www/indice_prioridad.Rmd')
                 )
)

# SERVER ----
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    mapa_base
  })
  
  datos_grupo <- reactive({
    datos %>% 
      dplyr::filter(iconic_taxon_name == input$grupo)
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
  # 
  # . Celda ----
  output$info_celda <- renderText({
    if (is.null(input$map_shape_click$id))
      return("No hay celda seleccionada")
    # # Debug:
    # dc <- datos %>% 
    #   dplyr::filter(iconic_taxon_name == 'Todos') 
    #   filter(grid_id == 178) %>%
    #   sf::st_drop_geometry()
    # out <- tags$h4("Información sobre la celda seleccionada:")
    dc <- datos_celda()
    # print(dc) # debug
    titulo <- paste0('<strong>Indice de prioridad: ', 
                     round(dc$indice_prioridad, 3),
                     '</strong>')
    
    subtitulo <- ifelse(dc$etiqueta == mis_etiquetas[length(mis_etiquetas)],
                        paste0(dc$etiqueta, '!'), 
                        paste0('Prioridad:', dc$etiqueta))
    out <- paste(
      sep = '</br>',
      titulo,
      paste0('(percentil ', scales::percent(dc$indice_prioridad), ')'),
      paste0(tags$strong(subtitulo)),
      '</br>',
      paste0('ID de la Celda: ', input$map_shape_click$id),
      paste0('Área: ', round(dc$area), ' Km2'),
      paste0('Cantidad de egistros: ', dc$n_registros),
      paste0('Intensidad espacial: ', round(dc$spatial_intensity, 3)),
      paste0('Intensidad temporal: ', round(dc$temporal_intensity, 3)),
      paste0('Riqueza de especies: ', as.integer(dc$species_richness)),
      paste0('\u21b3 Último año: ', dc$n_new_species_last_year, ' (', 
             scales::percent(dc$prop_new_species_last_year), ')')
    )
    HTML(out)
  })
  
  # . Gráficos ----
  output$graficos <- renderCachedPlot(
    cacheKeyExpr = input$grupo, {
      pal <- colorpal()
      datos_grupo() %>%
      # d %>%
        ggplot() +
        aes(n_registros, species_richness, color = etiqueta) +
        geom_point() +
        scale_color_manual('Prioridad', 
                           values = pal(mis_etiquetas),
                           breaks = mis_etiquetas) +
        theme(legend.position = 'bottom') +
        ylab('Riqueza de especies (S)') +
        xlab('Número de registros (n)') +
        ggtitle(
          label = paste('Grupo:', input$grupo),
          subtitle = 'Riqueza vs Registros por celda'
          )
    })
  
  grid_id <- reactiveValues()
  
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
}

# Run app ----
shinyApp(ui = ui, server = server)

