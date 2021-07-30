
#' Mapeamentos/Relatorios disponiveis dentro do pacote
#'
#' Usando as duas bases existentes no pacote, resume quais sao os mapeamentos e
#' relatorios disponiveis, alem de informar qual a fonte dos mesmos.
#'
#' @return Uma tibble com 3 colunas
#' @export
#'
#' @examples
mapeamentos_disponiveis <- function(){

    municipais <- plantiosflorestais::mapeamentos_municipios %>%
        dplyr::distinct(mapeamento, fonte) %>%
        dplyr::mutate(base = "mapeamentos_municipios")

    estaduais <- plantiosflorestais::mapeamentos_estados %>%
        dplyr::distinct(mapeamento, fonte) %>%
        dplyr::mutate(base = "mapeamentos_estados")


    tabela <- dplyr::bind_rows(municipais, estaduais) %>%
        dplyr::relocate(base, .before = mapeamento)
    tabela

}


#'Area dos Mapeamentos/Relatorios disponiveis para determinada unidade
#'federativa (estado)
#'
#' @param unidade_federativa  Sigla da Unidade federativa do Brasil, se nao for
#' definida nenhuma, a funcao ira retornar os mapeamentos para o Parana - PR.
#'
#' @return Retorna uma tible organizada por ano, do mais recente para o mais antigo,
#' com areas totais por mapeamento, informando qual a abordagem do mesmo.
#' @export
#'
#' @examples
mapeamento_existente_uf  <- function(unidade_federativa = "PR"){

    # Check de input
    if (stringr::str_detect(unidade_federativa, "[A-Z]{2}") == FALSE) {
        stop("Por gentileza, preencha uma UF v\u00E1lida.", call. = FALSE)

    }

    # Mapeamentos de municipios
    if(unidade_federativa == "RS") {
        # Filtro necessário para os municipios não ficarem duplicados na área
        muni <- plantiosflorestais::mapeamentos_municipios %>%
            dplyr::filter(uf == unidade_federativa,
                          genero == "Todos") %>%
            dplyr::group_by(ano_base, mapeamento) %>%
            dplyr::summarise(area_total_ha = sum(area_ha, na.rm = TRUE)) %>%
            dplyr::mutate(abordagem = "Por munic\u00CDpio")


    } else {
        muni <- plantiosflorestais::mapeamentos_municipios %>%
            dplyr::filter(uf == unidade_federativa) %>%
            dplyr::group_by(mapeamento, ano_base) %>%
            dplyr::summarise(area_total_ha = sum(area_ha, na.rm = TRUE)) %>%
            dplyr::mutate(abordagem = "Por munic\u00CDpio")

    }

    # Mapeamento de estados
    estado <- plantiosflorestais::mapeamentos_estados %>%
        dplyr::filter(uf == unidade_federativa) %>%
        dplyr::group_by(mapeamento, ano_base) %>%
        dplyr::summarise(area_total_ha = sum(area_ha, na.rm = TRUE)) %>%
        dplyr::mutate(
            abordagem = dplyr::case_when(
                mapeamento == "AGEFLOR - A ind\u00FAstria de base florestal no Rio Grande do Sul 2017" ~ "Por coredes",
                mapeamento == "APRE - Estudo Setorial 2020" ~ "Por regi\u00E3o",
                mapeamento == "ACR - Anu\u00E1rio Estatistico 2019" ~ "Por regi\u00E3o",
                TRUE ~ "Estadual"
            )
        )

    # Juntar os dois resultados
    df_final <- dplyr::bind_rows(muni, estado) %>%
        dplyr::arrange(dplyr::desc(ano_base)) %>%
        dplyr::relocate(ano_base, .before = mapeamento)

    # Testar se existe resultado no output
    linhas_df <- nrow(df_final)

    # Return
    if (linhas_df > 0) {
        return(df_final)

    } else{
        stop("Por gentileza, preencha uma UF v\u00E1lida.", call.= FALSE)
    }

}


#' Generos existentes nos mapeamentos disponiveis
#'
#' Resume quais sao os generos dos plantios florestais existentes nas bases utilizadas
#' no pacote. Opcionalmente pode exibir o genero por mapeamento.
#'
#' @param exibir_nome_mapeamento padrao = FALSE, se TRUE exibe o nome do mapeamento
#'
#' @return Uma tibble com os generos disponiveis ou com genero por mapeamento
#' @export
#'
#' @examples
generos_plantios_disponiveis <- function(exibir_nome_mapeamento = FALSE){

    exibir_nome_mapeamento = FALSE
    map_muni <- plantiosflorestais::mapeamentos_municipios

    if (exibir_nome_mapeamento == TRUE) {
        generos_muni <- map_muni %>%
            dplyr::distinct(genero, mapeamento)

    } else {
        generos_muni <-map_muni %>%
            dplyr::distinct(genero)
    }

    # preciso alterar para gerar uma base com tudo e depois decide se exibe o nome
    # do mapeamento
    map_uf <- plantiosflorestais::mapeamentos_estados

    if (exibir_nome_mapeamento == TRUE) {
        generos_uf <- map_uf %>%
            dplyr::distinct(genero, mapeamento)

    } else {
        generos_uf <- map_uf %>%
            dplyr::distinct(genero)
            #dplyr::mutate(base = "mapeamentos_estados")
    }


    # Empilhar as duas bases
    genero_disponivel <- dplyr::bind_rows(generos_muni, generos_uf) %>%
        dplyr::filter(genero != "Todos") %>%
        dplyr::distinct()

    genero_disponivel

}


#' Area dos Mapeamentos/Relatorios com series historicas por ano
#'
#' @return Uma tibble com os mapeamentos que possuem uma serie historica,
#' com a respectiva area para cada ano-base
#' @export
#'
#' @examples
serie_historicas_disponiveis <- function(){

    # Desativar warning do get dupes
    options("get_dupes.grouped_warning" = FALSE)

    # Mapeamentos de municipios
    map_muni <- plantiosflorestais::mapeamentos_municipios
    muni <- map_muni %>%
        dplyr::group_by(ano_base, mapeamento) %>%
        dplyr::summarise(area_total_ha = sum(area_ha, na.rm = TRUE)) %>%
        janitor::get_dupes(mapeamento) %>%
        dplyr::select(-dupe_count) %>%
        dplyr::mutate(base ="mapeamentos_municipios") %>%
        dplyr::relocate(base, .before = mapeamento)
    muni


    # Mapeamento de estados
    map_uf <- plantiosflorestais::mapeamentos_estados
    estado <- map_uf %>%
        dplyr::group_by(ano_base, mapeamento) %>%
        dplyr::summarise(area_total_ha = sum(area_ha, na.rm = TRUE)) %>%
        janitor::get_dupes(mapeamento) %>%
        dplyr::select(-dupe_count) %>%
        dplyr::mutate(base ="mapeamentos_estados") %>%
        dplyr::relocate(base, .before = mapeamento)
    estado

    # Juntar os dois resultados
    df_final <- dplyr::bind_rows(muni, estado)
    df_final
}
