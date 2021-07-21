#' Dados sobre os mapeamentos em nivel municipal de plantios florestais no Brasil
#'
#' Base de dados contendo os mapeamentos de plantios florestais no Brasil,
#' a nivel de municipio.
#'
#' @format Uma tibble com 32,588 linhas e 12 colunas, contendo:
#' \describe{
#'   \item{mapeamento}{Nome do mapeamento ou relatorio de origem}
#'   \item{fonte}{Fonte dos dados, nao e necessariamente o responsavel pelo
#'   relatorio}
#'   \item{ano_base}{Ano base do mapeamento, difere do ano em que o relatorio foi
#'   publicado, geralmente}
#'   \item{uf}{Unidade Federativa da Uniao}
#'   \item{estado}{Nome do Estado da Uniao}
#'   \item{municipio}{Nome do municipio}
#'   \item{code_muni}{Codigo unico do municipio pelo IBGE}
#'   \item{genero}{Genero do plantio florestal}
#'   \item{area_ha}{Area plantada em hectares}
#' }
#' @name mapeamentos_municipios
#' @source Para detalhes, veja o seguinte repositorio: - \url{https://github.com/maykongpedro/2021-07-04-extracao-mapeamentos-plantios-florestais}
#' @examples head(mapeamentos_municipios)
"mapeamentos_municipios"
