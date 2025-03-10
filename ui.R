library(shiny) # carrega o pacote Shiny

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("autoSEVEB - Etiquetas para as contraprovas das pesquisas de sangue"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          HTML("<b>1. Atualize a planilha de registros do VB (final do número do laudo e resultados do KM e IC);</b>"),
          br(),
          br(),
          fileInput("regs", "2. Selecione o arquivo de registros (.xlsx):", accept = ".xlsx", buttonLabel = "Buscar..."),
          "Apenas casos com os resultados preenchidos estarão disponíveis para seleção.",
          br(),
          br(),
          uiOutput(
            outputId = "lista_vbs"
          ),
          actionButton(inputId = "vai", label = "Gerar etiquetas", disabled = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          uiOutput("pdfview"),
          textOutput("distPlot")
        )
    )
)
