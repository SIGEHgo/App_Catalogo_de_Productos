library(shiny)   
library(bslib)          # Sidebar
library(DT)             # Tabla
library(plotly)         # Grafica interactivos
library(ggplot2)        # Graficos 2d
library(treemapify)     # Grafico de treemapify


datos = "https://docs.google.com/spreadsheets/d/e/2PACX-1vShNJnh442gay0BXd5hnYPUK1cC4j3JgKRvdHadU5DincbhNmCbWVmz2GlkqStUq8Ci--bGzlP5LfLg/pub?gid=0&single=true&output=csv" |> read.csv()

variables = names(datos)[!names(datos) %in% c("CLAVE", "Producto", "Descripción")]
tipo_graficas = c("Barras", "Pastel", "Treemapify")

diccionario = setNames(
  variables, 
  variables |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_replace_all("\\.", " ") |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    tools::toTitleCase()
)




ui <- page_sidebar(
  
  tags$style(
    HTML("
    html, body, .container-fluid {
      height: 100%;
      margin: 0; !important;
      padding: 0;!important;
    }
    .main.bslib-gap-spacing.html-fill-container {
      margin: 0 !important;
      padding: 0 !important;
    }
    
    .full-screen {
            position: fixed;
            height: 100vh !important;
            width: 100vw !important;
            left: 0;
            top: 0;
            z-index: 9999;
            overflow: hidden;
    }
  ")
  ),
  
  sidebar = sidebar(
    tags$img(
      src = "https://raw.githubusercontent.com/Eduardo-Alanis-Garcia/Js/main/Planeacion_dorado.png",     
      height = "37px",      
      width = "auto",       
      style = "display: block; margin: 0 auto;" # Centrar en el sidebar
    ),
    
    selectInput(
      inputId = "columna",
      label = "Selecciona una variable",
      choices = diccionario,
      selectize = F
    ),
    
    selectInput(
      inputId = "tipo_grafica",
      label = "Seleccione un tipo de grafica",
      choices = tipo_graficas,
      selectize = F
    ),
  ),
  
  div(
    style = "display: flex; flex-direction: column; height: 100vh; width: 100%;",
    div(
      style = "flex: 0 0 60%; width: 100%; overflow-y: auto;",
      DTOutput("tabla", height = "100%", width = "100%")
    ),
    div(
      style = "flex: 0 0 40%; width: 100%;",
      plotlyOutput("grafica", height = "100%", width = "100%")
    )
  )
  
)



server <- function(input, output) {
  
  data <- reactive({ datos })
  
  datos_filtrados = reactive({
    require(data(), input$columna)
    
    datos = data()
    
    df = datos[[input$columna]] |>  as.data.frame()
    names(df)[1] = "variable_seleccionada"
    
    df = df |> 
      dplyr::mutate(variable_seleccionada = dplyr::if_else(condition = variable_seleccionada == "", true = "Sin dato", false = variable_seleccionada)) |> 
      dplyr::group_by(variable_seleccionada) |> 
      dplyr::summarise(frecuencia = dplyr::n()) |> 
      dplyr::ungroup() |> 
      dplyr::arrange(dplyr::desc(frecuencia)) |> 
      dplyr::mutate(variable_seleccionada = factor(variable_seleccionada, levels = variable_seleccionada))
    
    return(df)
  })
  
  click_grafica <- reactive({
    click = event_data("plotly_click")
    print(click)
    if (is.null(click)) return(NULL)
    
    if (input$tipo_grafica == "Barras" && !is.null(click$x)) {
      return(click$x)
    }
    if (input$tipo_grafica %in% c("Pastel", "Treemapify") && !is.null(click$pointNumber)) {
      return(click$pointNumber + 1)  # Añadir un +1 para tener misma congruencia que barras y data.frame
    }
    return(NULL)
  })
  
  # Mostrar click, solo es para andar checando en consola
  observeEvent(click_grafica(), {
    cat("Clic en:", click_grafica(), "\n")
  })
  
  output$tabla = renderDT({    
    req(data(), datos_filtrados())
    
    indice_columna = which(names(data()) == input$columna) + 1 # Dado que datatable inicializa con uno mas
    filtros = vector(mode = "list", ncol(data()))
    filtros = lapply(filtros, function(x) list(search = ""))
    
    if (!is.null(click_grafica())) {
      filtros[[indice_columna]] <- list(search = datos_filtrados()$variable_seleccionada[click_grafica()] |>  as.character())
    }
    cat("Vamos a imprimir filtros: ", "\n")
    print(filtros)
  

    datatable(data(), filter = "top", 
              options = list( pageLength = 5, searchCols = filtros)
              )
  })
  

  output$grafica <- renderPlotly({
    seleccion_grafica = input$tipo_grafica
    cat("Se ha seleccionado la gráfica de tipo:", seleccion_grafica, "\n")
    

    
    df = datos_filtrados()
    cat("Tenemos que datos filtrados es: ", "\n")
    print(df)
    
    if (seleccion_grafica == "Barras") {
      
      gg = ggplot(data = df, aes(x = variable_seleccionada, y = frecuencia)) + 
        geom_bar(stat = "identity", fill = "#691c32", color = "#b38e5d",
                 aes(text = paste0(
                   "<b>Variable seleccionada:</b> ", input$columna |> stringr::str_replace_all("_", " ") |> stringr::str_replace_all("\\.", " ") |>
                     stringr::str_squish() |> stringr::str_to_lower() |> tools::toTitleCase(), 
                   "<br><b>Frecuencia:</b> ", frecuencia)
                 )) +
        geom_text(aes(label = frecuencia, y = frecuencia/2),
                  position = position_dodge(width = 0.9),
                  size = 4,
                  color = "white") +
        labs(x = gsub("_", " ", input$columna) |> stringr::str_replace_all("_", " ") |> stringr::str_replace_all("\\.", " ") |>
               stringr::str_squish() |> stringr::str_to_lower() |> tools::toTitleCase(),  
             y = "Frecuencia") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", face = "bold"),
              axis.title.x = element_text(color = "black", face = "bold"))
      
      
      plotly::ggplotly(gg, tooltip = "text") |> 
        plotly::config(
          modeBarButtonsToRemove = list("select2d", "lasso2d","hoverClosestCartesian", "hoverCompareCartesian","toggleSpikelines"),
          scrollZoom = TRUE,
          displaylogo = FALSE,
          doubleClick = "reset",
          locale = "es",
          displayModeBar = TRUE
        )
      
    }else if(seleccion_grafica == "Pastel"){
      
      plot_ly(
        data = df,
        labels = ~variable_seleccionada,
        values = ~frecuencia,
        type = 'pie',
        textinfo = 'label+percent',
        insidetextorientation = 'radial',
        marker = list(line = list(color = '#FFFFFF', width = 1)),
        text = ~paste0(
          "<b>", variable_seleccionada, "</b>",
          "<br><b>Frecuencia: </b>", frecuencia,
          "<br><b>Porcentaje: </b>", round(frecuencia / sum(frecuencia) * 100, 1), "%"
        ),
        hoverinfo = "text"
      )  |> 
        layout(
          legend = list(
            orientation = "v",
            x = 1,
            y = 0.5
          )
        ) |> 
        config(
          modeBarButtonsToRemove = list("select2d", "lasso2d","hoverClosestCartesian", "hoverCompareCartesian","toggleSpikelines"),
          scrollZoom = TRUE,
          displaylogo = FALSE,
          doubleClick = "reset",
          locale = "es"
        )
      
      
    }else if(seleccion_grafica == "Treemapify"){
      
      plot_ly(
        data = df,
        type = "treemap",
        labels = ~variable_seleccionada,
        values = ~frecuencia,
        parents = "",
        textinfo = "none",  
        texttemplate = "%{label}<br> <b>Frecuencia: </b>%{value}<br><b> Porcentaje: </b> %{percentEntry:.0%}",  # Personalizado el texto
        hovertemplate = "<b>%{label}</b><br>Frecuencia: %{value}<br>Porcentaje: %{percentEntry:.0%}<extra></extra>",
        textposition = "middle center"
      ) |> plotly::config(
        modeBarButtonsToRemove = list("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines"),
        scrollZoom = TRUE,
        displaylogo = FALSE,
        doubleClick = "reset",
        locale = "es"
      )
      
    }else{
      cat("Seleccione algo")
    }
  })
}  





shinyApp(ui = ui, server = server)
