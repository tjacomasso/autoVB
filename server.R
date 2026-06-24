library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(rlang)
library(readxl)
library(grid)
library(reactable)


# Define server logic required to draw a histogram
function(input, output, session) {

  # state usado como proteção CSRF simples no fluxo OAuth
  oauth_state <- paste0(sample(c(letters, 0:9), 24, replace = TRUE), collapse = "")

  rv <- reactiveValues(
    access_token   = NULL,
    email          = NULL,
    autorizado     = NULL,
    erro_drive     = NULL
  )

  # Captura o "code" devolvido pelo Google na própria URL do app
  observe({
    query <- shiny::parseQueryString(session$clientData$url_search)

    if (!is.null(query$code) && is.null(rv$access_token)) {

      # (validação simples do state — em produção, persista o state gerado
      #  por sessão em vez de comparar só dentro da mesma sessão reativa)
      token_resp <- tryCatch(
        exchange_code_for_token(query$code),
        error = function(e) NULL
      )

      if (!is.null(token_resp)) {
        rv$access_token <- token_resp$access_token
        rv$email        <- get_user_email(rv$access_token)
        rv$autorizado   <- endsWith(rv$email, paste0("@", DOMINIO_PERMITIDO))
      }
    }
  })

  registros <- reactive({

    req(isTRUE(rv$autorizado))

    tryCatch(
      {
        caminho <- download_drive_file(rv$access_token, DRIVE_FILE_ID)

        rv$erro_drive <- NULL

        readxl::read_excel(caminho) |>
          filter(str_detect(Pesquisa, "[Ss]angue"), !is.na(`sg km`))  |>
          select("Ordem VB", "Caso Sirsaelp", "Nº Laudo", "Pesquisa", `sg km`:`sgh forense`) |>
          mutate(across(`sg km`:`sgh forense`, function(x) str_split(x, pattern = ",")),
                 across(`sg km`:`sgh forense`, name_cols)) |>
          unnest_wider(`sg km`:`sgh forense`, names_sep = "_") |>
          pivot_longer(
            `sg km_1`:last_col(),
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
      },

      drive_sem_permissao = function(e) {
        rv$erro_drive <- conditionMessage(e)
        NULL
      }
    )

  })


  output$lista_vbs <- renderUI({
    if (is.null(rv$access_token)) {
      url_login <- build_auth_url(oauth_state)
      return(
        tagList(
          HTML("2. Faça o login no Google com o seu perfil institucional. Se solicitado, autorize o acesso ao seu Drive (o acesso é solicitado para todo o Drive, mas o app só baixa os registros)."),
          tags$a(
            href = url_login,
            class = "btn btn-primary",
            "Fazer login no Google Drive..."
          )
        )
      )
    }
    
    if (!isTRUE(rv$autorizado)) {
      return(
        h4(paste0(
          "Acesso negado para ", rv$email,
          ". Use sua conta @", DOMINIO_PERMITIDO, "."
        ))
      )
    }
    
    # Força a avaliação de registros() para popular rv$erro_drive antes de decidir
    # o que mostrar (sem isso, erro_drive só seria atualizado quando a tabela
    # já estivesse tentando renderizar)
    isolate(registros())
    
    if (!is.null(rv$erro_drive)) {
      return(
        tagList(
          HTML(paste("Bem-vindo,", rv$email)),
          div(class = "alert alert-warning", rv$erro_drive)
        )
      )
    }
    
    tagList(
      HTML(paste("Acessando como", rv$email)),
      selectizeInput(
        inputId = "vb",
        label = "2. Selecione o(s) caso(s) e clique em 'Gerar etiquetas':",
        choices = unique(registros()$`Ordem VB`),
        multiple = TRUE,
        size = 10
      )
    )
  })

  observe({
    req(input$vb)
    updateActionButton(inputId = "vai", disabled = FALSE)
  })

  observeEvent(eventExpr = input$vai, handlerExpr = {
    req(registros())

    registros <- registros() |> filter(`Ordem VB` %in% input$vb)

    pdf(
      file = "www/0.pdf",
      width = 2.55906,
      height = 0.984252,
      onefile = TRUE,
      title = 'Contraprovas VB'
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
        paste(registros$resultado_km[i], "|", registros$resultado_ic[i]),
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

      tags$iframe(style="height:100%; width:100%", src="0.pdf")

    })

    output$how_to_print <- renderText("<b>3. Imprimir o .pdf gerado utilizando a ETIQUETADORA. A impressora deve estar configurada para imprimir as etiquetas do Sirsaelp.</b>")

  })

  observeEvent(eventExpr = input$vai, handlerExpr = {
    req(registros())
    req(input$vb)

    resultados <-
      registros() |>
      filter(`Ordem VB` %in% input$vb)

    casos <-
      resultados[, c("Ordem VB", "Caso Sirsaelp", "Nº Laudo")] |>
      unique()


    output$results_table <-
      renderReactable(
        casos |>
          reactable(
            details = function(indice){
              resultados_caso <- resultados[resultados$`Ordem VB` == casos$`Ordem VB`[indice], c("item", "sg km", "sgh clínico", "sgh forense", "conclusao")]

              htmltools::div(style = "padding: 1rem",
                             reactable(resultados_caso, outlined = TRUE))
            }
          )
      )
  })
}
