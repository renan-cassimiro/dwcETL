#' @title Mapear um Dataset Limpo para o Padrao Darwin Core
#'
#' @description Aplica o mapeamento De/Para declarado em `dwc_mapping` no
#'   YAML de configuracao (coluna original -> termo Darwin Core oficial),
#'   adiciona os campos `verbatim*` (valores originais preservados sem
#'   nenhuma transformacao, via [extract_verbatim()]) e os metadados fixos
#'   declarados em `fixed_metadata` (ex: `basisOfRecord`, `datasetName`), e
#'   devolve um `tibble` contendo estritamente os termos DwC resultantes --
#'   nenhuma coluna fora do padrao e mantida na saida `_dwc.csv`.
#'
#' @details A funcao e agnostica ao dominio: ela nao conhece de antemao
#'   nenhum termo do Darwin Core especifico, apenas aplica literalmente o
#'   mapeamento fornecido pelo YAML. Isso garante que o motor R permaneca
#'   generico e que toda regra de negocio (quais colunas existem, para
#'   quais termos elas mapeiam) fique isolada na camada de configuracao,
#'   conforme a secao 3 das diretrizes do pipeline.
#'
#'   Se `standardize_dates()` ja tiver sido executada antes desta funcao,
#'   as colunas de data ja padronizadas (ex: `eventDate`) devem ser
#'   referenciadas em `dwc_mapping` usando o proprio nome de saida como
#'   "coluna original" (ver `config/template.yaml`).
#'
#'   Campos `verbatim*` (ex: `verbatimEventDate`, `verbatimLatitude`) sao
#'   tratados separadamente de `dwc_mapping`: eles vem do snapshot
#'   normalizado-mas-nao-tipado gerado por [clean_dataset()], nunca do
#'   valor ja convertido/padronizado. Isso garante que, por exemplo,
#'   `eventDate` seja `"2021-03-15"` (ISO 8601) enquanto
#'   `verbatimEventDate` permanece `"15/03/2021"`, exatamente como
#'   registrado na planilha original. Veja [extract_verbatim()] para como
#'   configurar esses campos no YAML.
#'
#'   **Tratamento especial para `dynamicProperties`**: colunas mapeadas para
#'   termos com prefixo `dynamicProperties.` (ex: `dynamicProperties.altura`)
#'   são agrupadas em uma única coluna `dynamicProperties` contendo um
#'   objeto JSON com as propriedades. Isso segue o padrão Darwin Core.
#'
#' @param df `tibble` limpo e com datas padronizadas, tipicamente a saida
#'   de [standardize_dates()] aplicada sobre a saida de [clean_dataset()].
#' @param cfg Lista de configuracao lida por [read_pipeline_config()].
#'
#' @return Um `tibble` contendo os termos Darwin Core mapeados, os campos
#'   verbatim configurados e os metadados fixos, prontos para gravacao em
#'   `*_dwc.csv`.
#'
#' @importFrom dplyr select rename all_of mutate bind_cols
#' @importFrom tibble as_tibble
#' @importFrom jsonlite toJSON
#' @export
map_to_dwc <- function(df, cfg) {
  mapeamento <- cfg$dwc_mapping
  metadados_fixos <- cfg$fixed_metadata
  
  if (is.null(mapeamento) || length(mapeamento) == 0) {
    stop("Configuracao sem 'dwc_mapping': impossivel gerar a saida Darwin Core.")
  }
  
  colunas_origem <- names(mapeamento)
  ausentes <- setdiff(colunas_origem, names(df))
  if (length(ausentes) > 0) {
    warning(sprintf(
      "As seguintes colunas declaradas em 'dwc_mapping' nao foram encontradas no dataset e serao ignoradas: %s",
      paste(ausentes, collapse = ", ")
    ))
  }
  
  mapeamento_valido <- mapeamento[colunas_origem %in% names(df)]
  
  df_dwc <- df[, names(mapeamento_valido), drop = FALSE]
  names(df_dwc) <- unlist(mapeamento_valido, use.names = FALSE)
  df_dwc <- tibble::as_tibble(df_dwc)
  
  # --- Tratamento especial para dynamicProperties ---
  # Identifica colunas cujo nome começa com "dynamicProperties."
  dyn_cols <- grep("^dynamicProperties\\.", names(df_dwc), value = TRUE)
  if (length(dyn_cols) > 0) {
    # Extrai os nomes das propriedades (parte após o ponto)
    prop_names <- gsub("^dynamicProperties\\.", "", dyn_cols)
    # Seleciona apenas essas colunas
    df_dyn <- df_dwc[, dyn_cols, drop = FALSE]
    # Constrói uma coluna JSON por linha
    df_dwc$dynamicProperties <- apply(df_dyn, 1, function(row) {
      # Converte para lista nomeada, removendo NAs para não incluí-los no JSON
      lst <- as.list(row)
      names(lst) <- prop_names
      # Filtra valores NA (opcional: pode-se manter como null)
      lst <- lst[!is.na(lst)]
      # Converte para JSON compacto, sem escape Unicode para legibilidade
      jsonlite::toJSON(lst, auto_unbox = TRUE, na = "null")
    })
    # Remove as colunas originais
    df_dwc <- df_dwc[, !names(df_dwc) %in% dyn_cols, drop = FALSE]
  }
  
  # Adiciona campos verbatim* (valores originais, sem nenhuma transformacao)
  df_verbatim <- extract_verbatim(df, cfg)
  if (ncol(df_verbatim) > 0) {
    colunas_duplicadas <- intersect(names(df_dwc), names(df_verbatim))
    if (length(colunas_duplicadas) > 0) {
      warning(sprintf(
        "Termo(s) verbatim colidem com termos ja mapeados em 'dwc_mapping' e serao ignorados: %s",
        paste(colunas_duplicadas, collapse = ", ")
      ))
      df_verbatim <- df_verbatim[, setdiff(names(df_verbatim), colunas_duplicadas), drop = FALSE]
    }
    df_dwc <- dplyr::bind_cols(df_dwc, df_verbatim)
  }
  
  # Adiciona metadados fixos como colunas constantes (ex: basisOfRecord)
  if (!is.null(metadados_fixos) && length(metadados_fixos) > 0) {
    for (termo_dwc in names(metadados_fixos)) {
      df_dwc[[termo_dwc]] <- metadados_fixos[[termo_dwc]]
    }
  }
  
  df_dwc
}