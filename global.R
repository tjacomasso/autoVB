library(httr2)
library(readxl)

GOOGLE_CLIENT_ID     <- "SEU_CLIENT_ID.apps.googleusercontent.com"
GOOGLE_CLIENT_SECRET <- "SEU_CLIENT_SECRET"
REDIRECT_URI         <- "https://seuapp.shinyapps.io/meuapp/"
DOMINIO_PERMITIDO    <- "suainstituicao.com.br"
DRIVE_FILE_ID        <- "ID_DO_ARQUIVO_NO_DRIVE"
SCOPES               <- "https://www.googleapis.com/auth/drive.readonly openid email"

AUTH_URL  <- "https://accounts.google.com/o/oauth2/v2/auth"
TOKEN_URL <- "https://oauth2.googleapis.com/token"

oauth_client <- httr2::oauth_client(
  id = GOOGLE_CLIENT_ID, secret = GOOGLE_CLIENT_SECRET,
  token_url = TOKEN_URL, name = "meu-app-institucional"
)

build_auth_url <- function(state) {
  httr2::url_modify(
    AUTH_URL,
    query = list(
      client_id             = GOOGLE_CLIENT_ID,
      redirect_uri          = REDIRECT_URI,
      response_type         = "code",
      scope                 = SCOPES,
      access_type           = "online",
      include_granted_scopes = "true",
      prompt                = "select_account",
      state                 = state
    )
  )
}

# Troca o "code" recebido no redirect por um access_token
exchange_code_for_token <- function(code) {
  resp <- httr2::request(TOKEN_URL) |>
    httr2::req_body_form(
      grant_type    = "authorization_code",
      code          = code,
      client_id     = GOOGLE_CLIENT_ID,
      client_secret = GOOGLE_CLIENT_SECRET,
      redirect_uri  = REDIRECT_URI
    ) |>
    httr2::req_perform()
  
  httr2::resp_body_json(resp)  # lista com access_token, expires_in, id_token...
}

# Consulta o e-mail do usuário logado
get_user_email <- function(access_token) {
  resp <- httr2::request("https://www.googleapis.com/oauth2/v2/userinfo") |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_perform()
  
  httr2::resp_body_json(resp)$email
}

# Baixa o arquivo .xlsx do Drive usando o access_token (API REST direta).
# Como é o token do PRÓPRIO usuário (não uma service account), a API do
# Drive já aplica a permissão dele sobre o arquivo: se ele não tiver acesso,
# a chamada retorna 403/404. Isso é repassado como condição "drive_sem_permissao"
# para ser tratado na camada reativa.
download_drive_file <- function(access_token, file_id) {
  tmp <- tempfile(fileext = ".xlsx")
  
  tryCatch(
    {
      httr2::request(sprintf(
        "https://www.googleapis.com/drive/v3/files/%s?alt=media", file_id
      )) |>
        httr2::req_auth_bearer_token(access_token) |>
        httr2::req_perform(path = tmp)
      
      tmp
    },
    httr2_http_403 = function(e) {
      rlang::abort("Usuário não tem permissão para acessar o arquivo no Drive.",
                   class = "drive_sem_permissao")
    },
    httr2_http_404 = function(e) {
      rlang::abort("Arquivo não encontrado (ou usuário sem permissão para vê-lo).",
                   class = "drive_sem_permissao")
    }
  )
}