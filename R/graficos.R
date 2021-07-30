


plotar_historico_brasil_iba <- function(abrangecia_uf = NULL, exibir_generos = TRUE){

    # Base
    map_uf <- plantiosflorestais::mapeamentos_estados

    # Definir filtro de região - Base SNIF
    abrangecia_uf = NULL

    if (is.null(abrangecia_uf)) {

        iba_snif <- map_uf %>%
            dplyr::filter(mapeamento == "IBÁ - Não identificado")

    } else if(stringr::str_detect(abrangecia_uf, "[A-Z]{2}") == FALSE) {

        stop("Por gentileza, preencha uma UF v\u00E1lida.", call. = FALSE)

    } else{

        iba_snif <- map_uf %>%
            dplyr::filter(
                mapeamento == "IBÁ - Não identificado",
                uf == abrangecia_uf
            )

    }

    # Fazer sumarização da base do SNIF

        dplyr::filter(mapeamento == "IBÁ - Não identificado") %>%
        dplyr::group_by(ano_base, genero) %>%
        dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE)) %>%
        dplyr::filter(ano_base %in% c("2006", "2007", "2008"))


    # Fazer sumarização da base do relatório 2020
    iba_relatorio <- map_uf %>%
        dplyr::filter(mapeamento == "IB\u00C1 - Relat\u00F3rio Anual 2020") %>%
        dplyr::group_by(ano_base, genero) %>%
        dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE))

    # Juntar as duas bases
    iba <- dplyr::bind_rows(iba_snif, iba_relatorio) %>%
        dplyr::mutate(
            genero = factor(
                genero,
                levels = c("Outros",
                           "Pinus",
                           "Eucalyptus"),
                ordered = TRUE

            )
        )


    # Adicionar fonte de texto necessária
    sysfonts::font_add_google("Crimson Text","Crimson")
    showtext::showtext.auto()


    # Definir configurações gerais para o gráfico
    config_plot <-
        c(
            "eucalipto" = "#35B779",
            "pinus" = "#ED7953",
            "outros" = "#31688E",
            "background" = "#F5F5F2",
            "text" = "#22211D",
            "text_fam" = "Crimson",
            "eixo_x_title" = "Ano-base",
            "eixo_y_title" = "Área (Milhões de hectares)",
            "title" = " - Histórico de área plantada por gênero, considerando informações do IBÁ",
            "caption" = "**@Dataviz:** *Pacote R - maykongpedro/plantiosflorestais*"
        )

    #Opção simples
    # subtitulo_iba  <- paste0("Série histórica de 2006 a 2008 baseada nos dados do IBÁ",
    #                          " disponilizados pelo SNIF.", " Para os anos de 2009 a 2019",
    #                          " os dados utilizados são referentes ao Relatório Anual 2020.",
    #                          " Fonte: 2006-2008: IBÁ |  2009-2017: Pöyry & IBÁ | 2018-2019: FGV & IBÁ"
    # )

    # OPção com quebra de linha na fonte
    # subtitulo_iba  <- paste0("Série histórica de 2006 a 2008 baseada nos dados do IBÁ",
    #                          " disponilizados pelo SNIF.", "<br>Para os anos de 2009 a 2019",
    #                          " os dados utilizados são referentes ao **Relatório Anual 2020**.",
    #                          "<br><br>**Fonte dos dados:** <br>2006-2008: IBÁ <br>2009-2017: Pöyry & IBÁ <br>2018-2019: FGV & IBÁ"
    # )

    # # opção sem quebra de linha na  nem itálico
    # subtitulo_iba  <- paste0("Série histórica de 2006 a 2008 baseada nos dados do IBÁ",
    #                          " disponilizados pelo SNIF.", "<br>Para os anos de 2009 a 2019",
    #                          " os dados utilizados são referentes ao **Relatório Anual 2020**.",
    #                          "<br><br>**Fonte dos dados:** 2006-2008: IBÁ | ",
    #                          "2009-2017: Pöyry & IBÁ | ",
    #                          "2018-2019: FGV & IBÁ"
    # )

    # Definir subtítulo
    # opção sem quebra de linha na fonte
    subtitulo_iba  <- paste0("Série histórica de 2006 a 2008 baseada nos dados do IBÁ",
                             " disponilizados pelo **SNIF**.", "<br>Para os anos de 2009 a 2019",
                             " os dados utilizados são referentes ao **Relatório Anual 2020**.",
                             "<br><br>**Fonte dos dados:** *2006-2008*: IBÁ | ",
                             "*2009-2017*: Pöyry & IBÁ | ",
                             "*2018-2019*: FGV & IBÁ"
    )



    iba %>%
        dplyr::mutate(area_ha = area_ha / 10 ^ 6) %>%
        ggplot2::ggplot() +
        ggplot2::geom_col(ggplot2::aes(x =  ano_base , y = area_ha, fill = genero)) +
        ggplot2::scale_y_continuous(
            expand = c(0, 0),
            limits = c(0, 10),
            labels = scales::number_format(accuracy = .2)
        ) +
        ggplot2::scale_fill_manual(
            values = c(config_plot[["outros"]],
                       config_plot[["pinus"]],
                       config_plot[["eucalipto"]]),
            guide = ggplot2::guide_legend(reverse = TRUE)
        ) +

        ggplot2::ggtitle(
            paste0(local,
                   config_plot[["title"]]),
            subtitle = glue::glue(subtitulo_iba)
            ) +

        ggplot2::labs(x = config_plot[["eixo_x_title"]],
                      y =  config_plot[["eixo_y_title"]],
                      fill = NULL,
                      caption = config_plot[["caption"]])+

        ggplot2::theme_minimal(base_family = config_plot[["text_fam"]],  base_size = 16) +
        ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = config_plot[["background"]], color = NA),
            panel.background = ggplot2::element_rect(fill = config_plot[["background"]], color = NA),
            plot.margin = ggplot2::unit(c(1,  1 , 1, 1), "cm"),
            legend.position = "top",
            axis.line = ggplot2::element_line(size = 1),
            axis.ticks = ggplot2::element_line(size = 1),
            text = ggplot2::element_text(color = config_plot[["text"]]),
            axis.title = ggplot2::element_text(face = "bold"),
            plot.title = ggplot2::element_text(face = "bold"),
            plot.subtitle = ggtext::element_markdown(),
            plot.caption = ggtext::element_markdown()
        )


}



