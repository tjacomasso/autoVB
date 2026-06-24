library(shiny) # carrega o pacote Shiny
library(bslib)
library(reactable)


# Define UI for application that draws a histogram
page_fillable(

  title = "autoVB",

  # Application title
  titlePanel("autoVB"),

  navset_bar(

    nav_panel(
      title = "Etiquetas para\nContraprovas",

      # Sidebar with a slider input for number of bins
      layout_sidebar(

        sidebar = sidebar(
          width = "25%",
          
          HTML("<b>1. Atualize a planilha de registros do VB (final do número do laudo e resultados do KM e do IC);</b>"),
          
          uiOutput(
            outputId = "lista_vbs"
          ),

          actionButton(inputId = "vai", label = "Gerar etiquetas", disabled = TRUE),

          htmlOutput("how_to_print")
        ),

        # Mostra o .pdf gerado com as etiquetas

        layout_columns(
          card(
            uiOutput("pdfview")
          ),
          card(
            reactableOutput("results_table")
          )
        )


      )
    ),

    nav_panel(
      title = "Registro de resultados (ainda não funciona)",

      tableOutput("resultsTable")
    ),

    nav_panel(
      title = "Indicadores do setor (ainda não funciona)"
    )
  )


)