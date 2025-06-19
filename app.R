
# -------------------------------- #
#        Librerías a cargar        #
# -------------------------------- #
library(shiny)
library(tidyverse)
library(bslib)
library(dplyr)
library(leaflet)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata) 
library(DT)
library(scales)

# ------------------------------------------------------------- #
#        Preparación de Datos (Cargar y unir una sola vez)      #
# ------------------------------------------------------------- #

df_mapaProdFAO <- read.csv("df_mapaProdFAO.csv")

# Bloque de traducción de nombres
df_mapaProdFAO <- df_mapaProdFAO %>%
  mutate(Área = case_when(
    Área == "Brasil" ~ "Brazil",
    Área == "Belice" ~ "Belize",
    Área == "Canadá" ~ "Canada",
    Área == "República Dominicana" ~ "Dominican Republic",
    Área == "Estados Unidos de América" ~ "United States of America",
    Área == "Venezuela (República Bolivariana de)" ~ "Venezuela",
    Área == "Bolivia (Estado Plurinacional de)" ~ "Bolivia",
    Área == "Alemania" ~ "Germany",
    Área == "Bélgica" ~ "Belgium",
    Área == "Dinamarca" ~ "Denmark",
    Área == "España" ~ "Spain",
    Área == "Francia" ~ "France",
    Área == "Grecia" ~ "Greece",
    Área == "Irlanda" ~ "Ireland",
    Área == "Italia" ~ "Italy",
    Área == "Países Bajos (Reino de los)" ~ "Netherlands",
    Área == "Países Bajos" ~ "Netherlands",
    Área == "Polonia" ~ "Poland",
    Área == "Portugal" ~ "Portugal",
    Área == "Reino Unido de Gran Bretaña e Irlanda del Norte" ~ "United Kingdom",
    Área == "Suecia" ~ "Sweden",
    Área == "Suiza" ~ "Switzerland",
    #Área == "Federación de Rusia" ~ "Russian Federation",
    Área == "Chequia" ~ "Czechia",
    Área == "Türkiye" ~ "Turkey",
    Área == "China, continental" ~ "China",
    Área == "Egipto" ~ "Egypt",
    Área == "Filipinas" ~ "Philippines",
    Área == "Japón" ~ "Japan",
    Área == "Marruecos" ~ "Morocco",
    Área == "Viet Nam" ~ "Vietnam",
    Área == "Irán (República Islámica del)" ~ "Iran",
    Área == "República Unida de Tanzanía" ~ "United Republic of Tanzania",
    Área == "Congo" ~ "Republic of the Congo",
    Área == "Côte d'Ivoire" ~ "Ivory Coast",
    Área == "Swazilandia" ~ "eSwatini",
    Área == "Federación de Rusia" ~ "Russia",
    TRUE ~ Área
  ))

world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(sovereignt, geometry)

# Valores para los filtros
lista_paises <- c("Todos los países", sort(unique(df_mapaProdFAO$Área)))
lista_productos <- c("Todos los productos", sort(unique(df_mapaProdFAO$Producto)))
# AHORA NECESITAMOS AMBAS LISTAS DE AÑOS
lista_anios_seleccion <- c("Todos los años", sort(unique(df_mapaProdFAO$Año), decreasing = TRUE))
min_anio <- min(df_mapaProdFAO$Año, na.rm = TRUE)
max_anio <- max(df_mapaProdFAO$Año, na.rm = TRUE)

# -------------------------------- #
#     Interfaz de Usuario (UI)     #
# -------------------------------- #

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "litera"),#litera
  
  div(style="padding: 15px; border-bottom: 1px solid #eee;",
      h5("| Visor de Producción Mundial de Alimentos |", style = "margin-bottom: 0;"),
      h6("Secretaría de Comercio e Inversiones Presidencia El Salvador/Dirección de Inteligencia Comercial Agroindustrial -DICA-", style = "margin-top: 5px; color: #666;"),
      #h6("Dirección de Inteligencia Comercial Agroindustrial -DICA-", style = "margin-top: 5px; color: #666;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtros de Selección"),
      selectInput("pais_seleccionado", "País:", choices = lista_paises),
      selectInput("producto_seleccionado", "Producto:", choices = lista_productos),
      
      # --- NUEVO: Selector para elegir el tipo de filtro de año ---
      radioButtons("tipo_filtro_anio", "Filtrar por:",
                   choices = c("Año Específico", "Rango de Años"),
                   selected = "Año Específico", inline = TRUE),
      
      # --- NUEVO: Panel Condicional para el AÑO ESPECÍFICO ---
      conditionalPanel(
        condition = "input.tipo_filtro_anio == 'Año Específico'",
        selectInput("anio_seleccionado", "Seleccione un Año:", choices = lista_anios_seleccion)
      ),
      
      # --- NUEVO: Panel Condicional para el RANGO DE AÑOS ---
      conditionalPanel(
        condition = "input.tipo_filtro_anio == 'Rango de Años'",
        sliderInput("rango_anios", "Seleccione un Rango de Años:",
                    min = min_anio,
                    max = max_anio,
                    value = c(min_anio, max_anio),
                    sep = "")
      ),
      
      hr(),
      
      div(style = "text-align: center; padding: 10px;",
          tags$a(href="https://www.presidencia.gob.sv/presidente-de-la-republica/", target="_blank",
                 tags$img(src = "https://i.postimg.cc/G36BghFf/Logo.png", height = "95px")
          )
      ),
      div(style = "text-align: center; padding: 10px;",
          tags$a(href="https://www.fao.org/home/es", target="_blank",
                 tags$img(src = "https://www.fao.org/images/corporatelibraries/fao-logo/fao-logo-archive/fao-logo-black-3lines-es.svg", height = "35px")
          ),
          hr(),
          h6("Fuente:Elaboración Propia DICA con información de FAO STAT actualizada al 18.06.2025",style = "margin-top: 5px; color: #666;"),
      ),
      
      
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(width = 8,
               h4(textOutput("titulo_mapa")),
               leafletOutput("mapa_produccion", height = "500px")
        ),
        column(width = 4,
               h4(textOutput("titulo_grafico")),
               plotlyOutput("grafico_top5", height = "500px")
        )
      ),
      fluidRow(
        column(width = 12,
               hr(),
               h4("Detalle de Datos por Producto"),
               DT::dataTableOutput("tabla_datos")
        )
      ),
      
      width = 9
    )
  )
)

# -------------------------------- #
#      Lógica del Servidor (Server) #
# -------------------------------- #
server <- function(input, output, session) {
  
  # Función reactiva base para filtrar datos
  datos_filtrados_base <- reactive({
    
    df_temp <- df_mapaProdFAO
    
    # --- ACTUALIZADO: Lógica de filtro de año que considera ambos controles ---
    if (input$tipo_filtro_anio == "Año Específico") {
      req(input$anio_seleccionado) # Requiere que el dropdown exista
      if (input$anio_seleccionado != "Todos los años") {
        df_temp <- df_temp %>% filter(Año == as.numeric(input$anio_seleccionado))
      }
    } else { # Si es "Rango de Años"
      req(input$rango_anios) # Requiere que el slider exista
      df_temp <- df_temp %>% filter(between(Año, input$rango_anios[1], input$rango_anios[2]))
    }
    
    # Filtros de País y Producto (se aplican después del filtro de año)
    if (input$pais_seleccionado != "Todos los países") {
      df_temp <- df_temp %>% filter(Área == input$pais_seleccionado)
    }
    if (input$producto_seleccionado != "Todos los productos") {
      df_temp <- df_temp %>% filter(Producto == input$producto_seleccionado)
    }
    
    return(df_temp)
  })
  
  # Flujo 1: Datos AGREGADOS (sin cambios)
  datos_agregados <- reactive({
    datos_filtrados_base() %>%
      group_by(Área) %>%
      summarise(
        Valor = sum(Valor, na.rm = TRUE),
        Unidad = first(Unidad),
        .groups = 'drop'
      ) %>%
      filter(Valor > 0)
  })
  
  # Flujo 2: Datos DETALLADOS (sin cambios)
  datos_para_tabla <- reactive({
    datos_filtrados_base()
  })
  
  datos_para_mapa <- reactive({
    world_map %>% left_join(datos_agregados(), by = c("sovereignt" = "Área"))
  })
  
  output$titulo_mapa <- renderText({"Mapa de Producción Agregada"})
  output$titulo_grafico <- renderText({
    if(input$pais_seleccionado != "Todos los países") {
      paste("Producción de", input$pais_seleccionado)
    } else { "Top-5 Países/Regiones Producción" }
  })
  
  output$mapa_produccion <- renderLeaflet({
    leaflet(world_map) %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = 0, lat = 30, zoom = 2)
  })
  
  observe({
    map_data <- datos_para_mapa()
    if (nrow(datos_agregados()) == 0) {
      leafletProxy("mapa_produccion", data = map_data) %>% clearShapes() %>% clearControls()
      return()
    }
    pal <- colorNumeric(palette = "YlOrRd", domain = datos_agregados()$Valor, na.color = "transparent")
    popup_content <- paste0("<strong>País: </strong>", map_data$sovereignt, "<br>", "<strong>Producción Agregada: </strong>", format(map_data$Valor, big.mark = ",", scientific = FALSE), " ", map_data$Unidad)
    proxy <- leafletProxy("mapa_produccion", data = map_data) %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(fillColor = ~pal(Valor), weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7, highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE), label = ~lapply(popup_content, htmltools::HTML)) %>%
      addLegend(pal = pal, values = ~Valor, opacity = 0.7, title = "Producción Agregada", position = "bottomright")
    if (input$pais_seleccionado != "Todos los países") {
      pais_seleccionado_sf <- map_data %>% filter(sovereignt == input$pais_seleccionado)
      if (nrow(pais_seleccionado_sf) > 0 && !all(is.na(pais_seleccionado_sf$geometry))) {
        bbox <- sf::st_bbox(pais_seleccionado_sf)
        proxy %>% flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
      }
    } else {
      proxy %>% setView(lng = 0, lat = 30, zoom = 2)
    }
  })
  
  output$grafico_top5 <- renderPlotly({
    top_data <- datos_agregados() %>% arrange(desc(Valor)) %>% slice_head(n = 5)
    if (nrow(top_data) == 0) return(NULL)
    plot_ly(data = top_data, x = ~Valor, y = ~reorder(Área, Valor), type = 'bar', orientation = 'h', marker = list(color = ~Valor, colorscale = 'YlOrRd'), hovertemplate = paste('<b>%{y}</b><br>Producción: %{x:,.0f}<extra></extra>')) %>%
      layout(xaxis = list(title = paste("Producción Agregada en", unique(top_data$Unidad))), yaxis = list(title = "", categoryorder = "total ascending"), showlegend = FALSE) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d'))
  })
  
  output$tabla_datos <- DT::renderDataTable({
    datos_para_tabla() %>%
      select(País = Área, Producto, Producción = Valor, Unidad, Año) %>%
      arrange(desc(Producción)) %>%
      DT::datatable(
        options = list(pageLength = 10, responsive = TRUE, searching = TRUE, scrollX = TRUE),
        rownames = FALSE,
        filter = "top"
      )
    
    
    
  })
  
  #bs_themer()
}

# -------------------------------- #
#        Ejecutar la App           #
# -------------------------------- #
shinyApp(ui = ui, server = server)
