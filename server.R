library(shiny)
# library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(rlang)
library(readxl)
library(googledrive)
library(grid)

name_cols <- function(cols){
  sapply(cols, function(x) set_names(x, nm = substr(x, 1, nchar(x) - 1)))
}

drive_download(
  "https://docs.google.com/spreadsheets/d/1hY_9oqwzM6LWlcKg5JhJmYbqN-yMJGOI/edit", overwrite = TRUE, path = "www/vb_tmp.xlsx"
  )

registros_vb <- read_xlsx("www/vb_tmp.xlsx") |> 
  filter(Pesquisa == "Sangue", !is.na(`sg km`))

registros <- registros_vb  |>
  select("Ordem VB", "Caso Sirsaelp", "Nº Laudo", "Pesquisa", `sg km`:`sgh forense`) |>
  mutate(across(`sg km`:`sgh forense`, function(x) str_split(x, pattern = ",")),
         across(`sg km`:`sgh forense`, name_cols)) |>
  unnest_wider(`sg km`:`sgh forense`, names_sep = "_") |>
  pivot_longer(
    `sg km_1`:`sgh forense_4`,
    names_to = "teste_item",
    values_to = "resultado",
    values_transform = function(x) substr(x, nchar(x), nchar(x))
    ) |> 
  separate_wider_delim(teste_item, delim = "_", names = c("teste_item_1", "item")) |> 
  filter(!is.na(resultado)) |> 
  pivot_wider(names_from = teste_item_1, values_from = resultado) |> 
  mutate(across(`sg km`:`sgh forense`, function(x) str_replace(x, item, ""))) |> 
  mutate(resultado_ic = case_when(`sgh clínico` == "p" | `sgh forense` == "p" ~ "IC +",
                               TRUE ~ "IC -"),
         resultado_km = ifelse(`sg km` == "p", "KM +", "KM -"),
         conclusao = ifelse(resultado_ic == "IC +", "SgH +", "SgH -"))

# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$lista_vbs <- renderUI(
    tagList(
      selectizeInput(
        inputId = "vb",
        label = "Selecione o(s) caso(s):",
        choices = unique(registros$`Ordem VB`),
        multiple = TRUE,
        size = 20
      )
    )
  )
  
  observeEvent(eventExpr = input$vai, handlerExpr = {
    
  registros <- registros |> filter(`Ordem VB` %in% input$vb)
    
    pdf(
      file = "www/0.pdf",
      width = 2.55906,
      height = 0.984252,
      onefile = TRUE
    )
    
    for (i in seq_along(registros$`Ordem VB`)) {
      grid.text(
        registros$`Ordem VB`[i],
        x = 0.05,
        y = 0.8,
        hjust = 0,
        vjust = .5,
        gp = gpar(
          fontsize = 16,
          col = "black",
          fontface = "bold"
        )
      )
      grid.text(
        paste("Item", registros$item[i]),
        x = 0.9,
        y = 0.8,
        hjust = 1,
        vjust = .5,
        gp = gpar(fontsize = 13, col = "black")
      )
      grid.text(
        registros$conclusao[i],
        x = 0.05,
        y = 0.5,
        hjust = 0,
        vjust = .5,
        gp = gpar(fontsize = 15, col = "black")
      )
      grid.text(
        paste(registros$resultado_km[i], "|", registros$resultado_ic),
        x = 0.5,
        y = 0.5,
        hjust = 0,
        vjust = .5,
        gp = gpar(fontsize = 10, col = "black")
      )
      grid.text(
        paste("LP", paste(
          registros$`Caso Sirsaelp`[i],
          registros$`Nº Laudo`[i],
          sep = "."
        )),
        x = 0.05,
        y = 0.15,
        hjust = 0,
        vjust = 0,
        gp = gpar(fontsize = 10, col = "black")
      )
      if (i != max(seq_along(registros$`Ordem VB`))) {
        grid.newpage()
      }
    }
    
    dev.off()
    
    output$pdfview <- renderUI({
      
      tags$iframe(style="height:600px; width:100%", src="0.pdf")
      
    })
    
  })
  
  
}
