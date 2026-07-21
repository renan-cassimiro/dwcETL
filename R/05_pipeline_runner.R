#' @title Executar o Pipeline para uma Unica Configuracao
#'
#' @description Executa o fluxo completo de ETL (leitura -> limpeza ->
#'   padronizacao de datas -> mapeamento Darwin Core -> gravacao) para uma
#'   unica planilha bruta, descrita por um arquivo `.yaml`.
#'
#' @details Esta funcao e puramente determinística: dado o mesmo par
#'   (arquivo bruto, arquivo de configuracao), ela sempre produz os mesmos
#'   dois arquivos de saida, byte a byte (exceto por timestamps de sistema
#'   de arquivos), sem gerar residuos. Os arquivos de saida sao
#'   sobrescritos a cada execucao, nunca acumulados com sufixos de versao,
#'   garantindo idempotencia.
#'
#' @param config_path Caminho do arquivo `.yaml` de configuracao da
#'   planilha.
#' @param raw_dir Diretorio contendo os arquivos brutos (`dados/raw/`).
#' @param processed_dir Diretorio de saida (`dados/processed/`).
#'
#' @return (Invisivel) Uma lista com os `tibble`s `cleaned` e `dwc`
#'   gerados, e os caminhos dos arquivos escritos, util para inspecao
#'   interativa ou testes.
#'
#' @importFrom readr write_csv
#' @importFrom tools file_path_sans_ext
#' @export
run_single_config <- function(config_path,
                               raw_dir = "dados/raw",
                               processed_dir = "dados/processed") {
  mensagem_etapa("Lendo configuracao", config_path)
  cfg <- read_pipeline_config(config_path)

  raw_path <- file.path(raw_dir, cfg$raw_file)
  mensagem_etapa("Lendo dado bruto", raw_path)
  df_raw <- read_raw_data(raw_path, cfg)

  mensagem_etapa("Limpando e tipando dados", basename(raw_path))
  df_cleaned <- clean_dataset(df_raw, cfg)

  mensagem_etapa("Padronizando datas (ISO 8601)", basename(raw_path))
  df_cleaned_com_datas <- standardize_dates(df_cleaned, cfg$date_columns)

  mensagem_etapa("Mapeando para Darwin Core", basename(raw_path))
  df_dwc <- map_to_dwc(df_cleaned_com_datas, cfg)

  if (!dir.exists(processed_dir)) {
    dir.create(processed_dir, recursive = TRUE)
  }

  base_nome <- tools::file_path_sans_ext(basename(cfg$raw_file))
  caminho_cleaned <- file.path(processed_dir, paste0(base_nome, "_cleaned.csv"))
  caminho_dwc <- file.path(processed_dir, paste0(base_nome, "_dwc.csv"))

  # A saida "_cleaned" preserva a estrutura original (sem as colunas de data
  # derivadas adicionadas apenas para o mapeamento DwC), garantindo
  # rastreabilidade 1:1 com o dado bruto.
  colunas_derivadas <- setdiff(names(df_cleaned_com_datas), names(df_cleaned))
  df_cleaned_saida <- df_cleaned_com_datas[, setdiff(names(df_cleaned_com_datas), colunas_derivadas), drop = FALSE]

  readr::write_csv(df_cleaned_saida, caminho_cleaned, na = "")
  readr::write_csv(df_dwc, caminho_dwc, na = "")

  mensagem_etapa("Concluido", sprintf("%s -> %s | %s", basename(raw_path), basename(caminho_cleaned), basename(caminho_dwc)))

  invisible(list(
    cleaned = df_cleaned_saida,
    dwc = df_dwc,
    cleaned_path = caminho_cleaned,
    dwc_path = caminho_dwc
  ))
}

#' @title Executar o Pipeline para Todas as Configuracoes Disponiveis
#'
#' @description Varre um diretorio de configuracoes `.yaml` e executa
#'   [run_single_config()] para cada uma, isolando falhas individuais para
#'   que um erro em uma planilha nao interrompa o processamento das
#'   demais. Ao final, imprime um resumo consolidado de sucessos e falhas.
#'
#' @details Esta e a funcao de entrada recomendada para uso em producao ou
#'   em jobs agendados. Ela nao possui nenhum conhecimento sobre planilhas
#'   especificas: basta adicionar um novo arquivo `.yaml` em `config/` e
#'   o correspondente arquivo bruto em `dados/raw/` para que uma nova
#'   planilha seja processada, sem qualquer alteracao no codigo R (secao 3
#'   das diretrizes).
#'
#' @param config_dir Diretorio contendo os arquivos `.yaml` (padrao:
#'   `"config"`).
#' @param raw_dir Diretorio dos arquivos brutos (padrao: `"dados/raw"`).
#' @param processed_dir Diretorio de saida (padrao: `"dados/processed"`).
#'
#' @return (Invisivel) Um `data.frame` resumo com uma linha por
#'   configuracao processada, contendo `config`, `status` e `mensagem`.
#'
#' @export
run_pipeline <- function(config_dir = "config",
                          raw_dir = "dados/raw",
                          processed_dir = "dados/processed") {
  arquivos_config <- list.files(config_dir, pattern = "\\.ya?ml$", full.names = TRUE)

  if (length(arquivos_config) == 0) {
    warning(sprintf("Nenhum arquivo .yaml encontrado em '%s'.", config_dir))
    return(invisible(data.frame(config = character(), status = character(), mensagem = character())))
  }

  resultados <- lapply(arquivos_config, function(cp) {
    resultado <- tryCatch(
      {
        run_single_config(cp, raw_dir = raw_dir, processed_dir = processed_dir)
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
