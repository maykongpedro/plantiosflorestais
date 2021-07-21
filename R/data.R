#' Dados sobre os mapeamentos em nível municipal de plantios florestais no Brasil
#'
#' Base de dados contendo os mapeamentos de plantios florestais no Brasil,
#' a nível de município.
#'
#' @format Uma tibble com 32,588 linhas e 12 colunas, contendo:
#' \describe{
#'   \item{mapeamento}{Nome do mapeamento ou relatório de origem}
#'   \item{fonte}{Fonte dos dados, não é necessariamente o responsável pelo
#'   relatório}
#'   \item{ano_base}{Ano base do mapeamento, diferere do ano que o relatório foi
#'   publicado, geralmente}
#'   \item{uf}{Unidade Federativa da União}
#'   \item{estado}{Nome do Estado da União}
#'   \item{municipio}{Nome do município}
#'   \item{code_muni}{Código único do município pelo IBGE}
#'   \item{genero}{Gênero do plantio florestal}
#'   \item{area_ha}{Área plantada (ha)}
#' }
#' @name mapeamentos_municipios
#' @source Diversos - \url{https://github.com/maykongpedro/2021-07-04-extracao-mapeamentos-plantios-florestais}
#' @examples head(mapeamentos_municipios)
"mapeamentos_municipios"
