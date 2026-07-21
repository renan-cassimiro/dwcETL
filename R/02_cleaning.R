#' @title Limpar um Dataset Bruto de Forma Estrita
#'
#' @description Aplica normalizacao estrita a um `data.frame` recem-lido:
#'   remove espacos em branco nas bordas de strings, remove caracteres de
#'   controle/metadados ocultos, remove colunas e linhas totalmente vazias,
#'   e aplica tipagem explicita conforme declarado em `column_types` no
#'   YAML de configuracao. NAO renomeia colunas nem aplica nenhuma regra
#'   de mapeamento Darwin Core -- essa e a etapa seguinte do pipeline.
#'
#' @details A tipagem explicita cobre, no minimo, coordenadas geograficas
#'   (`as.numeric`) e identificadores de ocorrencia (`as.character`),
#'   conforme exigido pelas diretrizes. Colunas listadas em
#'   `cfg$column_types` sao convertidas para o tipo declarado
#'   (`numeric`, `integer`, `character`). Colunas nao listadas permanecem
#'   como character, aparadas e limpas.
#'
#' @param df Um `data.frame`/`tibble` bruto, tipicamente vindo de
#'   [read_raw_data()].
#' @param cfg Lista de configuracao lida por [read_pipeline_config()].
#'
#' Adicionalmente, a funcao guarda, como atributo `"pre_typing"` do
#'   `tibble` retornado, uma copia dos dados **normalizados mas ainda nao
#'   tipados** (strings aparadas, sem ruido oculto, porem com a formatacao
#'   textual original intacta -- ex: `"-3.10"` continua `"-3.10"`, nao vira
#'   `-3.1`). Esse snapshot e a base dos campos `verbatim*` do Darwin Core
#'   (ex: `verbatimEventDate`, `verbatimLatitude`), que por definicao devem
#'   preservar o valor exatamente como registrado na fonte original, sem
#'   nenhuma reformatacao ou padronizacao aplicada aos campos "oficiais"
#'   correspondentes. Veja [extract_verbatim()].
#'
#' @param df Um `data.frame`/`tibble` bruto, tipicamente vindo de
#'   [read_raw_data()].
#' @param cfg Lista de configuracao lida por [read_pipeline_config()].
#'
#' @return Um `tibble` limpo, com a mesma estrutura de colunas do original,
#'   strings aparadas e tipos corrigidos. Contem o atributo interno
#'   `"pre_typing"` usado por [extract_verbatim()].
#'
#' @importFrom dplyr mutate across everything all_of
#' @importFrom stringr str_trim str_replace_all
#' @importFrom tibble as_tibble
#' @export
clean_dataset <- function(df, cfg) {
  df <- .normalizar_estrutura(df)

  # Snapshot pos-normalizacao, pre-tipagem: esta e a fonte fiel para os
  # campos verbatim (mantidos "como o original", so com ruido/espacos
  # removidos, sem nenhuma conversao de tipo ou padronizacao).
  pre_typing <- df

  # Tipagem explicita conforme declarado no YAML (coordenadas, IDs, etc.)
  df <- .aplicar_tipagem_explicita(df, cfg$column_types)

  attr(df, "pre_typing") <- pre_typing
  df
}

#' @title Normalizar Estrutura e Texto de um Dataset Bruto
#' @description Funcao auxiliar interna que executa a normalizacao comum
#'   a `clean_dataset()`: apara nomes de coluna, remove linhas/colunas
#'   vazias, remove metadados ocultos e apara espacos em valores, SEM
#'   aplicar nenhuma tipagem. E o unico lugar onde essa logica existe,
#'   reaproveitado tanto para a saida `_cleaned.csv` (apos tipagem) quanto
#'   para os campos `verbatim*` (antes da tipagem).
#' @param df `data.frame`/`tibble` de entrada.
#' @return `tibble` normalizado, todas as colunas ainda como character.
#' @importFrom dplyr mutate across everything na_if
#' @importFrom stringr str_trim
#' @importFrom tibble as_tibble
#' @keywords internal
.normalizar_estrutura <- function(df) {
  df <- tibble::as_tibble(df)

  # 0. Apara espacos nos NOMES das colunas (cabecalhos), residuo comum em
  #    exportacoes de planilha (ex: " Coletor" em vez de "Coletor").
  names(df) <- stringr::str_trim(names(df))

  # 1. Remove linhas e colunas 100% vazias (residuo comum em exportacoes de planilha)
  df <- .remover_linhas_colunas_vazias(df)

  # 2. Remove metadados ocultos: caracteres de controle, BOM, espacos nao separaveis
  df <- dplyr::mutate(
    df,
    dplyr::across(dplyr::everything(), ~ .limpar_metadados_ocultos(.x))
  )

  # 3. Remove espacos em branco nas bordas de todas as colunas de texto
  df <- dplyr::mutate(
    df,
    dplyr::across(dplyr::everything(), ~ stringr::str_trim(.x, side = "both"))
  )

  # 4. Normaliza strings vazias remanescentes para NA explicito
  df <- dplyr::mutate(
    df,
    dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, ""))
  )

  df
}

#' @title Remover Metadados Ocultos de uma String
#' @description Funcao auxiliar interna: remove caracteres de controle
#'   invisiveis (ex: BOM `\uFEFF`, caracteres nulos, marcas de formatacao
#'   Unicode) que frequentemente contaminam planilhas exportadas de
#'   sistemas legados, sem alterar o conteudo textual visivel.
#' @param x Vetor de caracteres.
#' @return Vetor de caracteres limpo.
#' @importFrom stringr str_replace_all
#' @keywords internal
.limpar_metadados_ocultos <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  # Remove BOM, caracteres de controle C0/C1 e espacos nao separaveis (NBSP -> espaco comum)
  x <- stringr::str_replace_all(x, "\uFEFF", "")
  x <- stringr::str_replace_all(x, "[\u0001-\u0008\u000B\u000C\u000E-\u001F]", "")
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x
}

#' @title Remover Linhas e Colunas Inteiramente Vazias
#' @description Funcao auxiliar interna que descarta linhas e colunas cujo
#'   conteudo e integralmente `NA` ou string vazia, um residuo comum de
#'   exportacoes de planilhas com formatacao extra.
#' @param df `data.frame`/`tibble` de entrada.
#' @return `tibble` sem linhas/colunas totalmente vazias.
#' @keywords internal
.remover_linhas_colunas_vazias <- function(df) {
  vazio <- function(v) all(is.na(v) | trimws(as.character(v)) == "")

  colunas_vazias <- vapply(df, vazio, logical(1))
  df <- df[, !colunas_vazias, drop = FALSE]

  if (nrow(df) > 0) {
    linhas_vazias <- apply(df, 1, function(l) all(is.na(l) | trimws(as.character(l)) == ""))
    df <- df[!linhas_vazias, , drop = FALSE]
  }

  tibble::as_tibble(df)
}

#' @title Aplicar Tipagem Explicita a Colunas Declaradas no YAML
#' @description Funcao auxiliar interna que converte colunas para os tipos
#'   declarados em `column_types` no arquivo de configuracao (tipicamente
#'   `numeric` para coordenadas e `character` para identificadores de
#'   ocorrencia). Colunas com valores nao conversiveis geram `NA` com aviso,
#'   nunca erro fatal, preservando a idempotencia do pipeline.
#' @param df `tibble` de entrada, ja limpo.
#' @param column_types Lista nomeada `coluna -> tipo` vinda do YAML. Pode
#'   ser `NULL` se nenhuma tipagem explicita for necessaria.
#' @return `tibble` com as colunas convertidas.
#' @keywords internal
.aplicar_tipagem_explicita <- function(df, column_types) {
  if (is.null(column_types) || length(column_types) == 0) {
    return(df)
  }

  for (coluna in names(column_types)) {
    if (!coluna %in% names(df)) next

    tipo <- column_types[[coluna]]
    df[[coluna]] <- switch(
      tolower(tipo),
      "numeric"   = suppressWarnings(as.numeric(df[[coluna]])),
      "integer"   = suppressWarnings(as.integer(df[[coluna]])),
      "character" = as.character(df[[coluna]]),
      {
        warning(sprintf("Tipo '%s' desconhecido para coluna '%s'; mantido como character.", tipo, coluna))
        as.character(df[[coluna]])
      }
    )
  }

  df
}
