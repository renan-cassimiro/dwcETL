#' @title Executar o Pipeline para uma Unica Configuracao
#'
#' @description Executa o fluxo completo de ETL (leitura -> limpeza ->
#'   padronizacao de datas -> mapeamento Darwin Core -> derivacao -> gravacao)
#'   para uma unica planilha bruta, descrita por um arquivo `.yaml`.
#'
#' @param config_path Caminho do arquivo `.yaml` de configuracao da planilha.
#' @param raw_dir Diretorio contendo os arquivos brutos (`dados/raw/`).
#' @param processed_dir Diretorio de saida (`dados/processed/`).
#' @param output_delimiter Delimitador opcional para os arquivos de saida.
#'
#' @return (Invisivel) Uma lista com os `tibble`s `cleaned` e `dwc` gerados.
#'
#' @importFrom readr write_delim
#' @importFrom tools file_path_sans_ext
#' @export
run_single_config <- function(config_path,
                              raw_dir = "dados/raw",
                              processed_dir = "dados/processed",
                              output_delimiter = NULL) {
  mensagem_etapa("Lendo configuracao", config_path)
  cfg <- read_pipeline_config(config_path)
  
  if (is.null(output_delimiter)) {
    output_delimiter <- cfg$output_delimiter %||% ","
  }
  
  raw_path <- file.path(raw_dir, cfg$raw_file)
  mensagem_etapa("Lendo dado bruto", raw_path)
  df_raw <- read_raw_data(raw_path, cfg)
  
  mensagem_etapa("Limpando e tipando dados", basename(raw_path))
  df_cleaned <- clean_dataset(df_raw, cfg)
  
  mensagem_etapa("Padronizando datas (ISO 8601)", basename(raw_path))
  df_cleaned_com_datas <- standardize_dates(df_cleaned, cfg$date_columns)
  
  mensagem_etapa("Mapeando para Darwin Core", basename(raw_path))
  df_dwc <- map_to_dwc(df_cleaned_com_datas, cfg)
  
  # --- NOVO: Derivação automática de termos ---
  mensagem_etapa("Derivando termos Darwin Core", basename(raw_path))
  df_dwc <- derive_dwc_terms(df_dwc, cfg)
  
  if (!dir.exists(processed_dir)) {
    dir.create(processed_dir, recursive = TRUE)
  }
  
  base_nome <- tools::file_path_sans_ext(basename(cfg$raw_file))
  caminho_cleaned <- file.path(processed_dir, paste0(base_nome, "_cleaned.csv"))
  caminho_dwc <- file.path(processed_dir, paste0(base_nome, "_dwc.csv"))
  
  colunas_derivadas <- setdiff(names(df_cleaned_com_datas), names(df_cleaned))
  df_cleaned_saida <- df_cleaned_com_datas[, setdiff(names(df_cleaned_com_datas), colunas_derivadas), drop = FALSE]
  
  readr::write_delim(df_cleaned_saida, caminho_cleaned, delim = output_delimiter, na = "")
  readr::write_delim(df_dwc, caminho_dwc, delim = output_delimiter, na = "")
  
  mensagem_etapa("Gerando relatório de validação", basename(raw_path))
  generate_validation_report(
    df_cleaned = df_cleaned_saida,
    df_dwc = df_dwc,
    config_path = config_path,
    raw_file = cfg$raw_file,
    output_dir = processed_dir
  )
  
  mensagem_etapa("Gerando relatório PDF", basename(raw_path))
  generate_pdf_report(
    df_raw = df_raw,
    df_cleaned = df_cleaned_saida,
    df_dwc = df_dwc,
    cfg = cfg,
    output_dir = processed_dir,
    base_name = base_nome
  )
  
  mensagem_etapa("Concluido", sprintf("%s -> %s | %s | validation.json | report.pdf",
                                      basename(raw_path),
                                      basename(caminho_cleaned),
                                      basename(caminho_dwc)))
  
  invisible(list(
    cleaned = df_cleaned_saida,
    dwc = df_dwc,
    cleaned_path = caminho_cleaned,
    dwc_path = caminho_dwc
  ))
}

#' @title Executar o Pipeline para Todas as Configuracoes
#'
#' @inheritParams run_single_config
#' @export
run_pipeline <- function(config_dir = "config",
                         raw_dir = "dados/raw",
                         processed_dir = "dados/processed",
                         output_delimiter = NULL) {
  arquivos_config <- list.files(config_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  
  if (length(arquivos_config) == 0) {
    warning(sprintf("Nenhum arquivo .yaml encontrado em '%s'.", config_dir))
    return(invisible(data.frame(config = character(), status = character(), mensagem = character())))
  }
  
  resultados <- lapply(arquivos_config, function(cp) {
    resultado <- tryCatch(
      {
        run_single_config(cp, raw_dir = raw_dir, processed_dir = processed_dir, output_delimiter = output_delimiter)
        list(status = "OK", mensagem = "")
      },
      error = function(e) list(status = "ERRO", mensagem = conditionMessage(e))
    )
    data.frame(
      config = basename(cp),
      status = resultado$status,
      mensagem = resultado$mensagem,
      stringsAsFactors = FALSE
    )
  })
  
  resumo <- do.call(rbind, resultados)
  
  cat("\n=========== RESUMO DO PIPELINE ===========\n")
  for (i in seq_len(nrow(resumo))) {
    cat(sprintf("[%s] %s %s\n", resumo$status[i], resumo$config[i],
                if (resumo$status[i] == "ERRO") paste0("- ", resumo$mensagem[i]) else ""))
  }
  cat("============================================\n")
  
  invisible(resumo)
}

#' @title Imprimir Mensagem de Etapa do Pipeline
#' @description Funcao auxiliar interna de logging simples e consistente,
#'   usada para tornar a execucao do pipeline auditavel no console/log,
#'   sem introduzir dependencias externas de logging.
#' @param etapa Nome curto da etapa em execucao.
#' @param detalhe Detalhe adicional (ex: nome de arquivo).
#' @return (Invisivel) `NULL`. Efeito colateral: imprime no console.
#' @keywords internal
mensagem_etapa <- function(etapa, detalhe = "") {
  cat(sprintf("[%s] %s: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), etapa, detalhe))
  invisible(NULL)
}