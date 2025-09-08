library(shiny)
library(DT)

ui <- fluidPage(
  textInput("buscarSepal", "Buscar en Sepal.Length:", ""),
  textInput("buscarSpecies", "Buscar en Species:", ""),
  actionButton("simular", "Simular búsqueda"),
  DTOutput("tabla")
)

server <- function(input, output, session) {
  output$tabla <- renderDT({
    datatable(
      iris,
      options = list(pageLength = 5),
      filter = "top"  # activa filtros de columna
    )
  })
  
  proxy <- dataTableProxy("tabla")
  
  # Cuando presionas el botón, se simulan los filtros de columnas
  observeEvent(input$simular, {
    updateSearch(
      proxy,
      keywords = list(
        columns = list(
          "",
          input$buscarSepal,  # columna 1: Sepal.Length
          "",                 # columna 2: Sepal.Width
          "",                 # columna 3: Petal.Length
          "",                 # columna 4: Petal.Width
          input$buscarSpecies # columna 5: Species
        )
      )
    )
  })
}

shinyApp(ui, server)
