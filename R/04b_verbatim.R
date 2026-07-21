#' @title Extrair Campos Verbatim (Darwin Core)
#'
#' @description Constroi as colunas `verbatim*` do Darwin Core (ex:
#'   `verbatimEventDate`, `verbatimLatitude`, `verbatimElevation`) a partir
#'   dos valores **originais, apenas normalizados** (espacos aparados,
#'   ruido oculto removido), sem NENHUMA conversao de tipo, arredondamento
#'   ou padronizacao -- ao contrario dos campos "oficiais" correspondentes
#'   (ex: `eventDate`, `decimalLatitude`), que passam por tipagem e/ou
#'   padronizacao (ISO 8601, `as.numeric`, etc.).
#'
#' @details Duas fontes alimentam o resultado, ambas configuraveis via
#'   YAML e totalmente opcionais:
#'
#'   1. `cfg$verbatim_mapping`: um De/Para explicito, no mesmo espirito de
#'      `cfg$dwc_mapping`, no formato
#'      `"coluna original": "termoVerbatimDwC"` (ex:
#'      `"Latitude": "verbatimLatitude"`). Cobre qualquer campo que o
#'      usuario queira preservar verbatim (coordenadas, elevacao,
#'      profundidade, sistema de referencia, etc.).
#'
#'   2. `cfg$date_columns`: quando uma entrada de data nao desativa
#'      explicitamente com `keep_verbatim: false`, seu valor original
#'      (antes do parsing) e automaticamente incluido sob o termo indicado
#'      em `verbatim_term` (padrao: `verbatimEventDate` para a chave
#'      `eventDate`, ou `verbatim<Nome>` para outras chaves). Isso cobre o
#'      caso mais comum de campo verbatim exigido pelo padrao DwC sem
#'      exigir configuracao extra.
#'
#'   Um mesmo termo declarado nas duas fontes usa a entrada explicita de
#'   `verbatim_mapping` (ela tem precedencia sobre a auto-derivacao de
#'   `date_columns`).
#'
#' @param df_cleaned O `tibble` retornado por [clean_dataset()], do qual
#'   sera lido o atributo interno `"pre_typing"` (snapshot normalizado mas
#'   nao tipado). Caso o atributo nao exista (uso fora do fluxo padrao do
#'   pipeline), a propria `df_cleaned` e usada como melhor esforco, com um
#'   aviso.
#' @param cfg Lista de configuracao lida por [read_pipeline_config()].
#'
#' @return Um `tibble` com uma coluna por termo verbatim resultante, todas
#'   como character, na mesma ordem de linhas de `df_cleaned` -- pronto
#'   para ser combinado (`dplyr::bind_cols`) com a saida de
#'   [map_to_dwc()]. Se nao houver nenhum campo verbatim configurado,
#'   retorna um `tibble` com zero colunas e o mesmo numero de linhas.
#'
#' @importFrom dplyr mutate across everything
#' @importFrom tibble as_tibble tibble
#' @export
extract_verbatim <- function(df_cleaned, cfg) {
  pre_typing <- attr(df_cleaned, "pre_typing")
  if (is.null(pre_typing)) {
    warning(
      "Atributo 'pre_typing' ausente em df_cleaned (esperado quando vindo de clean_dataset()); ",
      "usando os valores ja tipados como melhor esforco para os campos verbatim."
    )
    pre_typing <- df_cleaned
  }

  mapeamento_verbatim <- cfg$verbatim_mapping
  if (is.null(mapeamento_verbatim)) mapeamento_verbatim <- list()

  mapeamento_verbatim <- .auto_derivar_verbatim_de_datas(mapeamento_verbatim, cfg$date_columns)

  if (length(mapeamento_verbatim) == 0) {
    return(tibble::tibble(.rows = nrow(pre_typing)))
  }

  colunas_origem <- names(mapeamento_verbatim)
  ausentes <- setdiff(colunas_origem, names(pre_typing))
  if (length(ausentes) > 0) {
    warning(sprintf(
      "As seguintes colunas declaradas para campos verbatim nao foram encontradas no dataset e serao ignoradas: %s",
      paste(ausentes, collapse = ", ")
    ))
  }

  mapeamento_valido <- mapeamento_verbatim[colunas_origem %in% names(pre_typing)]
  if (length(mapeamento_valido) == 0) {
    return(tibble::tibble(.rows = nrow(pre_typing)))
  }

  df_verbatim <- pre_typing[, names(mapeamento_valido), drop = FALSE]
  names(df_verbatim) <- unlist(mapeamento_valido, use.names = FALSE)

  # Garante que todo campo verbatim seja gravado como texto puro, sem
  # nenhuma conversao -- e literalmente o valor original, so aparado.
  df_verbatim <- dplyr::mutate(df_verbatim, dplyr::across(dplyr::everything(), as.character))

  tibble::as_tibble(df_verbatim)
}

#' @title Derivar Automaticamente o Mapeamento Verbatim de Colunas de Data
#' @description Funcao auxiliar interna: para cada entrada em
#'   `date_columns` do YAML que nao desativa explicitamente com
#'   `keep_verbatim: false`, adiciona ao mapeamento verbatim uma entrada
#'   `coluna_original -> termo_verbatim`, a menos que a coluna original ja
#'   esteja coberta por um mapeamento explicito em `verbatim_mapping`
#'   (que tem precedencia).
#' @param mapeamento_verbatim Lista nomeada ja existente (tipicamente
#'   `cfg$verbatim_mapping`, possivelmente vazia).
#' @param date_columns Lista `cfg$date_columns`, possivelmente `NULL`.
#' @return Lista nomeada `coluna_original -> termo_verbatim` combinada.
#' @keywords internal
.auto_derivar_verbatim_de_datas <- function(mapeamento_verbatim, date_columns) {
  if (is.null(date_columns) || length(date_columns) == 0) {
    return(mapeamento_verbatim)
  }

  colunas_ja_mapeadas <- names(mapeamento_verbatim)

  for (nome_saida in names(date_columns)) {
    spec <- date_columns[[nome_saida]]
    coluna_original <- spec$original_column
    manter_verbatim <- is.null(spec$keep_verbatim) || isTRUE(spec$keep_verbatim)

    if (is.null(coluna_original) || !manter_verbatim) next
    if (coluna_original %in% colunas_ja_mapeadas) next

    termo_verbatim <- spec$verbatim_term
    if (is.null(termo_verbatim)) {
      termo_verbatim <- paste0(
        "verbatim",
        toupper(substring(nome_saida, 1, 1)),
        substring(nome_saida, 2)
      )
    }

    mapeamento_verbatim[[coluna_original]] <- termo_verbatim
  }

  mapeamento_verbatim
}
