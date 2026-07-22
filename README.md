# Pipeline de ETL — Darwin Core (DwC)

Pipeline genérico, modular e reutilizável em R para transformar planilhas
biológicas heterogêneas em dados padronizados no formato **Darwin Core**,
seguindo princípios de Ciência Aberta (dados abertos, rastreáveis e
reprodutíveis).

## 1. Ideia central

O código R **nunca conhece** nomes de colunas específicos de nenhuma
planilha. Toda regra de negócio — quais colunas existem, para qual termo
DwC cada uma mapeia, qual o formato de data, quais metadados fixos aplicar
— vive em um arquivo `.yaml` por planilha, em `config/`. Para adicionar uma
nova fonte de dados, basta:

1. Colocar o arquivo bruto em `dados/raw/`.
2. Criar um `.yaml` em `config/` (copie `config/template.yaml`).
3. Rodar `Rscript main.R`.

Nenhuma linha de código R precisa ser tocada.

## 2. Estrutura de pastas

```
.
├── main.R                     # ponto de entrada
├── R/                         # motor genérico (imutável, documentado em Roxygen2)
│   ├── 01_io_utils.R          # leitura de csv/tsv/xlsx/pdf + leitura de config
│   ├── 02_cleaning.R          # limpeza estrita e tipagem explícita
│   ├── 03_date_standardize.R  # datas -> ISO 8601 (lubridate)
│   ├── 04_dwc_mapper.R        # mapeamento De/Para -> Darwin Core
│   ├── 04b_verbatim.R         # extração dos campos verbatim* (valores originais preservados)
│   └── 05_pipeline_runner.R   # orquestração (run_pipeline / run_single_config)
├── config/
│   └── template.yaml          # modelo de configuração comentado
├── dados/
│   ├── raw/                   # ENTRADA — somente leitura, nunca editado manualmente
│   └── processed/             # SAÍDA — gerada e sobrescrita pelo pipeline
└── logs/
```

## 3. Fluxo de dados

```
dados/raw/arquivo.csv  ─┐
                         │  1. read_raw_data()       (dispatch por file_type)
config/arquivo.yaml  ───┤  2. clean_dataset()        (trim, tipagem, remove ruído)
                         │  3. standardize_dates()    (ISO 8601)
                         │  4. map_to_dwc()           (De/Para + metadados fixos)
                         ▼
dados/processed/arquivo_cleaned.csv   ← dado bruto limpo, colunas originais
dados/processed/arquivo_dwc.csv       ← estritamente termos Darwin Core
```

Cada arquivo bruto processado gera exatamente **dois** arquivos de saída,
garantindo rastreabilidade 1:1 entre bruto → limpo → padronizado.

## 4. Como executar

Dependências R: `readr`, `readxl`, `dplyr`, `stringr`, `lubridate`, `yaml`,
`tibble` (e opcionalmente `pdftools` para planilhas em PDF).

```bash
Rscript main.R
```

Isso varre todos os `.yaml` em `config/`, processa cada planilha
correspondente e imprime um resumo de sucessos/erros ao final. Uma falha
em uma planilha **não** interrompe o processamento das demais.

Para processar/depurar uma única planilha interativamente:

```r
source("R/01_io_utils.R"); source("R/02_cleaning.R")
source("R/03_date_standardize.R"); source("R/04_dwc_mapper.R")
source("R/05_pipeline_runner.R")

resultado <- run_single_config("config/template.yaml")
resultado$dwc      # tibble já mapeado para Darwin Core
resultado$cleaned  # tibble limpo, estrutura original
```

## 5. Anatomia de um arquivo de configuração (`config/*.yaml`)

Veja `config/template.yaml` para um exemplo comentado, cobrindo:

- `raw_file`, `file_type`, `delimiter`, `encoding`, `sheet` — como ler o bruto.
- `fixed_metadata` — colunas constantes adicionadas na saída DwC (ex.: `basisOfRecord`).
- `column_types` — tipagem explícita (`numeric`, `integer`, `character`) por coluna do bruto.
- `date_columns` — de qual coluna original e em qual ordem de formatos (`dmy`, `mdy`, `ymd`) interpretar cada data.
- `verbatim_mapping` — De/Para explícito para campos `verbatim*` (ex.: `verbatimLatitude`), preservados **exatamente como o original** (só com espaços aparados), sem nenhuma conversão de tipo.
- `dwc_mapping` — o De/Para final: `"coluna original": "termoDarwinCore"`.

### Campos verbatim (`verbatim*`)

O Darwin Core exige, para vários termos, uma contraparte "verbatim" que
preserva o valor **exatamente como registrado na fonte original** — sem
nenhuma padronização, conversão de tipo ou arredondamento — ao lado do
termo "oficial" já tratado. Duas formas de gerar esses campos, ambas
opcionais:

1. **Automática para datas**: toda entrada em `date_columns` gera, por
   padrão, um campo verbatim correspondente (`eventDate` →
   `verbatimEventDate`) contendo o texto original antes do parsing (ex.:
   `"15/03/2021"`, preservado tal como escrito, enquanto `eventDate` vira
   `"2021-03-15"`). Para desativar, use `keep_verbatim: false`; para
   renomear o termo gerado, use `verbatim_term: "outroTermo"`.

2. **Explícita via `verbatim_mapping`**: para qualquer outro campo que
   precise ser preservado como veio (coordenadas, elevação, profundidade,
   sistema de referência, etc.), declare o De/Para diretamente, por
   exemplo:
   ```yaml
   verbatim_mapping:
     "Latitude": "verbatimLatitude"
     "Longitude": "verbatimLongitude"
   ```
   Assim, `decimalLatitude` recebe o valor numérico convertido
   (`-3.1`), enquanto `verbatimLatitude` mantém o texto original
   (`-3.10`, com o zero à direita preservado).

Os campos verbatim aparecem apenas em `*_dwc.csv` (nunca em
`*_cleaned.csv`, que preserva a estrutura de colunas original).

## 6. Garantias de qualidade

- **Idempotência**: mesma entrada ⇒ mesma saída, byte a byte; saídas são
  sempre sobrescritas, nunca acumuladas com sufixos de versão.
- **Normalização estrita**: `stringr::str_trim` em valores *e* cabeçalhos,
  remoção de caracteres de controle/BOM ocultos, remoção de linhas/colunas
  totalmente vazias, tipagem explícita de coordenadas e identificadores.
- **Datas**: parsing dinâmico multi-formato via `lubridate::parse_date_time`,
  saída sempre em `AAAA-MM-DD`; valores não interpretáveis viram `NA` com
  aviso agregado (nunca falha silenciosa).
- **Falhas isoladas por planilha**: `run_pipeline()` usa `tryCatch` por
  configuração, então um YAML malformado ou coluna ausente em uma planilha
  não derruba o lote inteiro.
- **Documentação**: todas as funções exportadas e auxiliares seguem
  Roxygen2 (`@title`, `@description`, `@details`, `@param`, `@return`,
  `@importFrom`), validado com `roxygen2::parse_file()`.

## 7. Testado neste ambiente

O pipeline foi executado de ponta a ponta com uma planilha de exemplo
(`dados/raw/planilha_exemplo.csv`, contendo espaços acidentais em valores
e cabeçalhos, coordenadas vazias e datas em três formatos distintos:
`dmy`, `ymd`, `mdy`), confirmando:

- limpeza correta (trim de valores e nomes de coluna);
- tipagem numérica de coordenadas;
- conversão correta das três variações de data para ISO 8601;
- geração determinística e idêntica em execuções repetidas (idempotência
  verificada via `md5sum`).
  
### Atualizações 
Adicionar seções sobre:

- Dynamic Properties: Mapeamento de colunas para dynamicProperties.* e o comportamento de agrupamento JSON.
- Validação: o pipeline Validação em arquivo *_validation.json com estatísticas descritivas.
- Separador de saída: Configuração do output_delimiter no YAML ou via argumento da função.
