#' @title Derivar Termos Darwin Core Automaticamente
#'
#' @description Após o mapeamento De/Para para Darwin Core, esta função aplica
#'   regras de derivação determinísticas para enriquecer o dataset com campos
#'   complementares, sem necessidade de configuração adicional.
#'
#' @details As derivações são aplicadas **exclusivamente** sobre colunas já
#'   mapeadas para termos Darwin Core (ex: `eventDate`, `scientificName`).
#'   Nenhuma coluna original é utilizada diretamente, garantindo que as regras
#'   sejam independentes da estrutura da planilha fonte.
#'
#'   **Regras implementadas:**
#'
#'   1. **Derivação de data (`eventDate`)**:
#'      Se a coluna `eventDate` existir e estiver no formato ISO 8601
#'      (`YYYY-MM-DD`), são criadas as colunas `year`, `month` e `day` com
#'      os valores extraídos. Caso `eventDate` seja `NA`, os três campos
#'      também serão `NA`.
#'
#'   2. **Estrutura taxonômica (`scientificName`)**:
#'      Se a coluna `scientificName` existir, o pipeline garante a existência
#'      das colunas `genus`, `specificEpithet`, `infraspecificEpithet`,
#'      `scientificNameAuthorship` e `taxonRank`. Se alguma delas ainda não
#'      existir, é criada preenchida com `"[A PREENCHER]"` para todas as linhas.
#'      **Não** é realizado nenhum parsing do nome científico nesta versão.
#'
#' @param df_dwc `tibble` já mapeado para termos Darwin Core (saída de
#'   [map_to_dwc()]).
#' @param cfg Lista de configuração lida por [read_pipeline_config()].
#'
#' @return O `tibble` original com as colunas derivadas adicionadas.
#'
#' @importFrom lubridate year month day
#' @export
derive_dwc_terms <- function(df_dwc, cfg) {
  # 1. Derivação de ano/mês/dia a partir de eventDate
  if ("eventDate" %in% names(df_dwc)) {
    # Converte para Date, garantindo que seja ISO
    datas <- as.Date(df_dwc$eventDate, format = "%Y-%m-%d")
    df_dwc$year <- lubridate::year(datas)
    df_dwc$month <- lubridate::month(datas)
    df_dwc$day <- lubridate::day(datas)
  }
  
  # 2. Garantia de colunas taxonômicas estruturais
  if ("scientificName" %in% names(df_dwc)) {
    colunas_tax <- c("genus", "specificEpithet", "infraspecificEpithet",
                     "scientificNameAuthorship", "taxonRank")
    for (col in colunas_tax) {
      if (!col %in% names(df_dwc)) {
        df_dwc[[col]] <- "[A PREENCHER]"
      }
    }
  }
  
  df_dwc
}