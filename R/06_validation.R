#' @title Gerar Relatório de Validação e Estatísticas Descritivas
#'
#' @description Produz um resumo estatístico do dataset limpo e do dataset
#'   mapeado para Darwin Core, útil para auditoria e controle de qualidade.
#'   O relatório inclui número de linhas e colunas, contagem de NAs,
#'   valores únicos (para colunas categóricas) e estatísticas básicas para
#'   colunas numéricas (mínimo, máximo, média, desvio padrão).
#'
#' @param df_cleaned `tibble` limpo (saída de `clean_dataset()`).
#' @param df_dwc `tibble` mapeado para DwC (saída de `map_to_dwc()`).
#' @param config_path Caminho do arquivo de configuração (para referência).
#' @param raw_file Nome do arquivo bruto (para identificação).
#'
#' @return Uma lista com dois elementos: `cleaned_stats` e `dwc_stats`,
#'   cada um contendo estatísticas por coluna. A lista também é gravada em
#'   um arquivo JSON no diretório de saída, se `output_dir` for fornecido.
#'
#' @importFrom dplyr summarise across everything n
#' @importFrom jsonlite toJSON
#' @export
generate_validation_report <- function(df_cleaned, df_dwc, config_path, raw_file, output_dir = NULL) {
  # Função auxiliar para calcular estatísticas de uma coluna
  col_stats <- function(col, col_name) {
    tipo <- class(col)[1]
    n_na <- sum(is.na(col))
    n_total <- length(col)
    stats <- list(
      name = col_name,
      type = tipo,
      total = n_total,
      na_count = n_na,
      na_percent = round(n_na / n_total * 100, 2)
    )
    if (is.numeric(col)) {
      stats$min <- min(col, na.rm = TRUE)
      stats$max <- max(col, na.rm = TRUE)
      stats$mean <- mean(col, na.rm = TRUE)
      stats$sd <- sd(col, na.rm = TRUE)
    } else if (is.character(col) || is.factor(col)) {
      unique_vals <- unique(col[!is.na(col)])
      stats$unique_count <- length(unique_vals)
      if (length(unique_vals) <= 10) {
        stats$unique_values <- unique_vals
      }
    }
    stats
  }
  
  # Aplica a cada coluna de um data frame
  stats_df <- function(df, nome) {
    stats_list <- lapply(names(df), function(nm) {
      col_stats(df[[nm]], nm)
    })
    names(stats_list) <- names(df)
    list(
      dataset = nome,
      n_rows = nrow(df),
      n_cols = ncol(df),
      columns = stats_list
    )
  }
  
  cleaned_stats <- stats_df(df_cleaned, "cleaned")
  dwc_stats <- stats_df(df_dwc, "dwc")
  
  resultado <- list(
    config = basename(config_path),
    raw_file = raw_file,
    timestamp = Sys.time(),
    cleaned = cleaned_stats,
    dwc = dwc_stats
  )
  
  # Grava em JSON se output_dir for especificado
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    base_name <- tools::file_path_sans_ext(basename(raw_file))
    json_path <- file.path(output_dir, paste0(base_name, "_validation.json"))
    jsonlite::write_json(resultado, json_path, auto_unbox = TRUE, pretty = TRUE)
    message(sprintf("Relatório de validação gravado em: %s", json_path))
  }
  
  invisible(resultado)
}