library(shiny) # carrega o pacote Shiny
library(bslib)

# Define UI for application that draws a histogram
page_fillable(
  
  title = "autoVB",
  
  # Application title
  titlePanel("autoVB - Etiquetas para as contraprovas das pesquisas de sangue"),
  
  # Sidebar with a slider input for number of bins
  layout_sidebar(
    height = "100%",
    fillable = TRUE,
    
    sidebar = sidebar(
      width = "30%",
      
      HTML("<b>1. Atualize a planilha de registros do VB (final do número do laudo e resultados do KM e do IC);</b>"),
      
      fileInput("regs", "2. Selecione o arquivo de registros (.xlsx):", accept = ".xlsx", buttonLabel = "Buscar..."),
      "Apenas casos com os resultados preenchidos estarão disponíveis para seleção.",
      
      uiOutput(
        outputId = "lista_vbs"
      ),
      
      actionButton(inputId = "vai", label = "Gerar etiquetas", disabled = TRUE),
      
      htmlOutput("how_to_print")
    ),
    
    # Mostra o .pdf gerado com as etiquetas
    uiOutput("pdfview")
    
  )
)
