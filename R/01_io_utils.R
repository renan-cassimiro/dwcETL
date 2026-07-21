#' @title Ler Configuração YAML de uma Planilha
#'
#' @description Carrega e valida um arquivo de configuração `.yaml` que
#'   descreve como uma planilha bruta específica deve ser lida, limpa e
#'   mapeada para o padrao Darwin Core.
#'
#' @details Esta funcao nao contem nenhuma regra de negocio fixa: ela apenas
#'   le a estrutura declarada no YAML e devolve uma lista R equivalente.
#'   Toda a logica especifica de cada planilha vive no arquivo de
#'   configuracao, nunca no codigo R (ver secao 3 das diretrizes do
#'   pipeline).
#'
#' @param config_path Caminho para o arquivo `.yaml` de configuracao.
#'
#' @return Uma lista R com os elementos do YAML (`raw_file`, `file_type`,
#'   `fixed_metadata`, `column_types`, `date_columns`, `dwc_mapping`, etc.).
#'
#' @importFrom yaml read_yaml
#' @export
read_pipeline_config <- function(config_path) {
  if (!file.exists(config_path)) {
    stop(sprintf("Arquivo de configuracao nao encontrado: %s", config_path))
  }

  cfg <- yaml::read_yaml(config_path)

  campos_obrigatorios <- c("raw_file", "file_type", "dwc_mapping")
  faltantes <- setdiff(campos_obrigatorios, names(cfg))
  if (length(faltantes) > 0) {
    stop(sprintf(
      "Configuracao '%s' esta incompleta. Campos obrigatorios ausentes: %s",
      basename(config_path), paste(faltantes, collapse = ", ")
    ))
  }

  cfg
}

#' @title Ler um Arquivo de Dados Bruto de Forma Generica
#'
#' @description Despacha a leitura de um arquivo bruto (`dados/raw/`) para o
#'   parser apropriado de acordo com o campo `file_type` declarado no YAML
#'   de configuracao, retornando sempre um `data.frame`/`tibble` de
#'   colunas de texto (tipagem e feita depois, na etapa de limpeza).
#'
#' @details Suporta `csv`, `tsv` e `xlsx` de forma robusta. Suporte a `pdf`
#'   e oferecido em modo "best effort": assume-se que o PDF contem uma
#'   tabela simples extraivel via `pdftools`, adequada apenas para casos
#'   pouco complexos, conforme indicado nas diretrizes. Planilhas em PDF
#'   com layouts complexos devem ser convertidas manualmente para CSV/XLSX
#'   antes de entrarem no pipeline.
#'
#' @param raw_path Caminho completo do arquivo em `dados/raw/`.
#' @param cfg Lista de configuracao lida por [read_pipeline_config()].
#'
#' @return Um `tibble` com os dados brutos, todas as colunas como
#'   character, e nomes de coluna preservados exatamente como no arquivo
#'   original.
#'
#' @importFrom readr read_delim locale
#' @importFrom readxl read_excel
#' @export
read_raw_data <- function(raw_path, cfg) {
  if (!file.exists(raw_path)) {
    stop(sprintf("Arquivo bruto nao encontrado: %s", raw_path))
  }

  tipo <- tolower(cfg$file_type)
  encoding <- cfg$encoding %||% "UTF-8"

  df <- switch(
    tipo,
    "csv" = readr::read_delim(
      raw_path,
      delim = cfg$delimiter %||% ",",
      locale = readr::locale(encoding = encoding),
      col_types = readr::cols(.default = readr::col_character()),
      trim_ws = FALSE,
      show_col_types = FALSE
    ),
    "tsv" = readr::read_delim(
      raw_path,
      delim = "\t",
      locale = readr::locale(encoding = encoding),
      col_types = readr::cols(.default = readr::col_character()),
      trim_ws = FALSE,
      show_col_types = FALSE
    ),
    "xlsx" = .read_xlsx_as_character(raw_path, sheet = cfg$sheet %||% 1),
    "pdf" = .read_pdf_table_best_effort(raw_path),
    stop(sprintf("file_type '%s' nao suportado. Use csv, tsv, xlsx ou pdf.", tipo))
  )

  tibble::as_tibble(df)
}

#' @title Ler XLSX Forcando Todas as Colunas como Texto
#' @description Funcao auxiliar interna: le uma planilha `.xlsx` inteira
#'   como character, para que a tipagem explicita ocorra apenas na etapa
#'   de limpeza (evita que o Excel "adivinhe" tipos de forma inconsistente
#'   entre arquivos).
#' @param path Caminho do arquivo `.xlsx`.
#' @param sheet Nome ou indice da aba a ser lida.
#' @return Um `data.frame` com todas as colunas como character.
#' @importFrom readxl read_excel
#' @keywords internal
.read_xlsx_as_character <- function(path, sheet = 1) {
  readxl::read_excel(path, sheet = sheet, col_types = "text")
}

#' @title Extrair Tabela Simples de um PDF (Best Effort)
#' @description Funcao auxiliar interna para planilhas biologicas
#'   fornecidas em PDF com estrutura tabular simples (linhas separadas por
#'   quebra de linha, colunas separadas por multiplos espacos ou tabs).
#'   Nao substitui uma revisao manual do resultado.
#' @param path Caminho do arquivo `.pdf`.
#' @return Um `data.frame` com a melhor extracao possivel da tabela.
#' @importFrom stringr str_split str_squish
#' @keywords internal
.read_pdf_table_best_effort <- function(path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("O pacote 'pdftools' e necessario para ler arquivos PDF. Instale com install.packages('pdftools').")
  }

  texto <- pdftools::pdf_text(path)
  linhas <- unlist(strsplit(texto, "\n"))
  linhas <- linhas[stringr::str_squish(linhas) != ""]

  linhas_split <- lapply(linhas, function(l) {
    stringr::str_split(stringr::str_squish(l), "\\s{2,}|\t")[[1]]
  })

  n_col <- max(vapply(linhas_split, length, integer(1)))
  linhas_split <- lapply(linhas_split, function(x) {
    length(x) <- n_col
    x
  })

  mat <- do.call(rbind, linhas_split)
  cabecalho <- mat[1, ]
  corpo <- mat[-1, , drop = FALSE]

  df <- as.data.frame(corpo, stringsAsFactors = FALSE)
  names(df) <- cabecalho
  df
}

#' @title Operador de Coalescencia Nula
#' @description Retorna `y` quando `x` e `NULL`, caso contrario retorna `x`.
#'   Usado para aplicar valores padrao a campos opcionais do YAML.
#' @param x Valor primario.
#' @param y Valor padrao, usado se `x` for `NULL`.
#' @return `x` ou `y`.
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
