#' @title Padronizar Colunas de Data para ISO 8601
#'
#' @description Converte dinamicamente colunas de data em formatos
#'   regionais variados (dia/mes/ano, mes/dia/ano, ano/mes/dia, com
#'   separadores distintos) para o padrao internacional ISO 8601
#'   (`AAAA-MM-DD`), usando as ordens de parsing declaradas no YAML de
#'   configuracao de cada planilha.
#'
#' @details Para cada entrada em `date_columns` no YAML, a funcao localiza
#'   a coluna original (`original_column`), tenta interpretar cada valor
#'   usando, em ordem, os formatos listados em `formats` (ex:
#'   `c("dmy", "mdy", "ymd")`) via [lubridate::parse_date_time()], e grava
#'   o resultado formatado como texto ISO 8601 em uma nova coluna cujo
#'   nome e a chave do bloco (tipicamente o proprio termo Darwin Core,
#'   ex: `eventDate`). Datas que nao podem ser interpretadas por nenhum
#'   formato se tornam `NA`, e um aviso agregado e emitido informando a
#'   quantidade de valores nao convertidos, para que o usuario possa
#'   auditar a planilha original -- o pipeline nunca falha silenciosamente.
#'
#' @param df `tibble` ja limpo, tipicamente a saida de [clean_dataset()].
#' @param date_columns Lista vinda do YAML (`cfg$date_columns`), no formato
#'   `list(eventDate = list(original_column = "Data_Coleta",
#'   formats = c("dmy","ymd")))`. Pode ser `NULL` se a planilha nao tiver
#'   colunas de data.
#'
#' @return O `tibble` de entrada com colunas adicionais (uma por entrada em
#'   `date_columns`) contendo as datas padronizadas em ISO 8601 como texto.
#'
#' @importFrom lubridate parse_date_time
#' @export
standardize_dates <- function(df, date_columns) {
  if (is.null(date_columns) || length(date_columns) == 0) {
    return(df)
  }

  for (nome_saida in names(date_columns)) {
    spec <- date_columns[[nome_saida]]
    coluna_original <- spec$original_column
    formatos <- spec$formats

    if (is.null(coluna_original) || !coluna_original %in% names(df)) {
      warning(sprintf(
        "Coluna de data original '%s' nao encontrada no dataset; '%s' sera preenchida com NA.",
        coluna_original %||% "<nao especificada>", nome_saida
      ))
      df[[nome_saida]] <- NA_character_
      next
    }

    if (is.null(formatos) || length(formatos) == 0) {
      formatos <- c("ymd", "dmy", "mdy")
    }

    datas_parseadas <- suppressWarnings(
      lubridate::parse_date_time(df[[coluna_original]], orders = formatos)
    )

    n_falhas <- sum(is.na(datas_parseadas) & !is.na(df[[coluna_original]]))
    if (n_falhas > 0) {
      warning(sprintf(
        "%d valor(es) da coluna '%s' nao puderam ser convertidos para data e viraram NA.",
        n_falhas, coluna_original
      ))
    }

    df[[nome_saida]] <- format(as.Date(datas_parseadas), "%Y-%m-%d")
  }

  df
}
