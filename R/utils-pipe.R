#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


# Definir itens usados nas funções com dplyr como variáveis globais
utils::globalVariables(c(
    "mapeamento",
    "fonte",
    "base",
    "genero",
    "ano_base",
    "uf",
    "area_ha"
))
