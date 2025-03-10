library(shiny) # carrega o pacote Shiny

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("autoSEVEB - Etiquetas para as contraprovas das pesquisas de sangue"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          fileInput("regs", "Selecione o arquivo de registros corrente", accept = ".xlsx"),
          uiOutput(
            outputId = "lista_vbs"
          ),
          actionButton(inputId = "vai", label = "Gerar etiquetas", ),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          uiOutput("pdfview"),
          textOutput("distPlot")
        )
    )
)
