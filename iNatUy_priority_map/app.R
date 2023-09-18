# GLOBAL ----
require(tidyverse)
require(sf)
require(leaflet)
require(shiny)
require(magrittr)
require(ggplot2)
require(dplyr)
# Documentación para leaflet en shiny:
# https://rstudio.github.io/leaflet/shiny.html

# Demo:
# https://bienflorencia.shinyapps.io/iNatUy_priority_map/

mis_etiquetas <- readLines("data/mis_etiquetas.txt")
datos <- readRDS("data/datos.rds")

# Grupos (iconic_taxon_name):
grac <- c("Todos", "Plantae", "Aves", "Insecta", "Arachnida", "Reptilia",
          "Amphibia", "Mammalia", "Fungi", "Mollusca", "Animalia",
          "Actinopterygii", "Protozoa")

source("R/funciones.R", local = TRUE, encoding = "UTF-8")

mapa_base <- leaflet() %>%
  setView(-55, -32.6, zoom = 7) %>%
  addProviderTiles(providers$OpenTopoMap,
                   options = providerTileOptions(noWrap = TRUE),
                   group = "Open Topo Map") %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(noWrap = TRUE),
                   group = "Imagen") %>%
  addTiles(group = "Open Street Map") %>%
  # Control de capas:
  addLayersControl(
    position = "bottomleft",
    baseGroups = c("Open Street Map", "Open Topo Map", "Imagen"),
    overlayGroups = c("Grilla"),
    options = layersControlOptions(collapsed = FALSE)
  )

# UI ----
ui <- navbarPage(
  collapsible = TRUE, # Para pantallas de tablets o celulares
  title = paste("NaturalistaUY: ¿dónde hay más oportunidades",
                "de llenar vacíos de información?"),
  theme = shinythemes::shinytheme("sandstone"),

  # ui - Mapa -----
  tabPanel(
    "Mapa",
    div(
      class = "outer",
      tags$head(includeCSS("styles.css")),
      leafletOutput("map", height = "89.5vh"),
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        top = 75, right = 10, left = "auto", bottom = "auto",
        width = 300, height = "auto",
        draggable = TRUE,
        selectInput(
          "grupo",
          label = "Grupo",
          choices = grac,
          selected = "Todos"
        )
        , tags$hr()
        , tabsetPanel(
          type = "pills",
          tabPanel("Celda", htmlOutput("info_celda")),
          tabPanel("graficos", plotOutput("graficos"))
        )
      )
    )
  )

  # ui - Doc ----
  , tabPanel(
    "Documentación",
    div(
      class = "documento",
      column(12, withMathJax(includeMarkdown("www/documentacion.md")))
    )
  )

  # ui - Contacto ----
  , tabPanel(
    "Contacto",
    div(
      class = "documento",
      column(12, withMathJax(includeMarkdown("www/contacto.md")))
    )
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
    colorFactor("RdYlBu", datos_grupo()$etiqueta, reverse = TRUE)
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
    dc <- datos_celda()
    titulo <- paste0("<strong>Indice de prioridad: ",
                     round(dc$indice_prioridad, 3),
                     "</strong>")

    subtitulo <- ifelse(dc$etiqueta == mis_etiquetas[length(mis_etiquetas)],
                        paste0("¡", dc$etiqueta, "!"),
                        paste0("Prioridad: ", dc$etiqueta))
    out <- paste(
      sep = "</br>",
      titulo,
      paste0("(percentil ", scales::percent(dc$indice_prioridad), ")"),
      "</br>",
      paste0(tags$strong(subtitulo)),
      "</br>",
      paste0("ID de la Celda: ", input$map_shape_click$id),
      paste0("Área: ", round(dc$area), " Km<sup>2</sup> "),
      paste0("Cantidad de registros: ", dc$n_registros),
      paste0("Intensidad espacial: ", round(dc$spatial_intensity, 3)),
      paste0("Intensidad temporal: ", round(dc$temporal_intensity, 3)),
      paste0("Riqueza de especies: ", as.integer(dc$species_richness)),
      paste0("\u21b3 Último año: ", dc$n_new_species_last_year, " (",
             scales::percent(dc$prop_new_species_last_year), ")")
    )
    HTML(out)
  })

  # . Gráficos ----
  output$graficos <- renderCachedPlot(
    cacheKeyExpr = input$grupo, {
      pal <- colorpal()
      datos_grupo() %>%
        ggplot() +
        aes(n_registros, species_richness, color = etiqueta) +
        geom_point() +
        scale_color_manual("Prioridad",
                           values = pal(mis_etiquetas),
                           breaks = mis_etiquetas) +
        theme(legend.position = "bottom") +
        ylab("Riqueza de especies (S)") +
        xlab("Número de registros (n)") +
        ggtitle(
          label = paste("Grupo:", input$grupo),
          subtitle = "Riqueza vs Registros por celda"
        )
    }
  )

  grid_id <- reactiveValues()

  observe({
    pal <- colorpal()

    leafletProxy("map", data = datos_grupo()) %>%
      clearShapes() %>%
      addPolygons(
        noClip = TRUE,
        layerId = ~grid_id,
        weight = .5,
        color = "white",
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
          position = "bottomright",
          title = "Prioridad",
          group = "Grilla"
        )
    }
  })
}

# Run app ----
shinyApp(ui = ui, server = server)

