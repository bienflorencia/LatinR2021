library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
# library(maptools)

# setwd("R/MapaGini/")

load('../data/grid_iNatUY_GIS.RData')

datos <- 
  grid_iNatUY_GIS %>% 
  mutate(indice_prioridad = temporalIntensity + spatialIntensity) %>% 
  select(-spsList) %>% 
  st_transform(crs = 4326)

Uruguay <- 
  read_sf('../data/Uruguay.shp') %>% 
  st_transform(crs = 4326)

ui <- fluidPage(

  # Titulo
  titlePanel("PRUEBA"),
  
  # Link a datos de origen
  helpText("BLABLA",
           a("blabla", 
             href = "https://github.com/bienflorencia/LatinR2021"),
           "." ),
  
  # Barra para seleccionar anio
  # sliderInput("ano", NULL,
  #             min = min(datos$anio), max = max(datos$anio),
  #             step = 1,
  #             value = 2015,
  #             sep = ""),
  # Leaflet map
  mainPanel(
    # Mapa:
    leafletOutput(outputId = 'map'),
    
    tags$hr()
  )
    # Tabla:
    # DT::dataTableOutput("tabla"))
  )
  

server <- function(input, output) {
  
  # Filtrar los datos para quedarnos sólo con los del año seleccionado:
  # datos_anio <- reactive({
  #   datos %>%
  #     filter(anio == input$ano) %>% 
  #     arrange(nomdepto)
  #   # Este objeto se usará tanto para el popup del mapa como para la tabla.
  # })
  
  # Construcción del mapa:  
  output$map <- renderLeaflet({
    
    # Esto es necesario para que el orden de los gini coincida con el orden de
    # los departamentos en el mapa:
    # giniano <- left_join(nombresDeptos, datos_anio()) %>%
    #   pull(gini)
    
    # El objeto popup es un vector character con el código HTML
    popup <- paste0("<strong>GridID: </strong>", 
                    datos$grid_ID, 
                    "<br><strong>Índice de prioridad: </strong>", 
                    datos$indice_prioridad)

    # Objeto (función) para determinar el coloreado de los deptos
    # pal <- colorBin(palette = "Purples", domain = datos$gini, bins = 5)
    pal <- colorBin(palette = "Spectral", 
                    na.color = "#79002d",
                    domain = datos$indice_prioridad, 
                    bins = 8)
    
    # Comandos para renderear la apariecia el mapa:
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      # setView(lng = -56.04, lat = -32.6, zoom = 7) %>% 
      clearBounds() %>% 
      addPolygons(
        data = Uruguay, fillColor = NA, weight = 2, color = 'black',
        fillOpacity = 0,
      ) %>% 
      addPolygons(
        color = 'white',
        data = datos,
        weight = .5, 
        fillColor = ~pal(indice_prioridad),
        fillOpacity = .5,
        popup = popup,
        highlightOptions = highlightOptions(color = "white", 
                                            weight = 2, 
                                            bringToFront = TRUE)
        )

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
