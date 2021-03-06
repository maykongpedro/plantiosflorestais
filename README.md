
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plantiosflorestais

<!-- badges: start -->
<!-- badges: end -->

Este pacote tem como objetivo disponibilizar dados de área de
mapeamentos públicos de plantios florestais no Brasil.

O pacote foi resultado do Trabalho de Conclusão de Curso apresentado ao
curso de Engenharia Florestal da Universidade Federal do Paraná (UFPR),
pelo graduando Maykon G. G. Pedro. O trabalho final pode ser acessado
pelo seguinte link:
[Ver trabalho](https://github.com/maykongpedro/plantiosflorestais/blob/master/inst/20210823-TCC-Corrigido-Aprovado-Maykon.pdf).

O repositório que contém todo o processo de extração e limpeza dos dados
pode ser consultado no seguinte endereço [Acessar
Repositório](https://github.com/maykongpedro/2021-07-04-extracao-mapeamentos-plantios-florestais)

Os mapeamentos disponíveis podem ser verificados no tópico “Fonte de
dados” desse `README`.

Todo o uso dos dados aqui contidos deve ser feito com a **citação da
fonte** do relatório envolvido.

## Instalação

Você pode instalar a versão de desenvolvimento desse pacote diretamente
do GitHub usando os seguintes comandos:

``` r
# install.packages("devtools")
devtools::install_github("maykongpedro/plantiosflorestais")
library(plantiosflorestais)
```

## Acesso às bases

Os dados presentes nesse pacote foram extraídos e organizados gerando
duas bases principais, uma para os mapeamentos que continham dados para
municípios, e outra para os mapeamentos que continham dados gerais, em
nível estadual ou regional.

As mesmas podem ser acessadas pelos seguintes comandos do pacote:

``` r
mapeamento_municipios <- plantiosflorestais::mapeamentos_municipios
mapeamento_estados <- plantiosflorestais::mapeamentos_estados
```

As colunas contidas nessas bases são as seguintes:

### Mapeamento em nível de municípios:

``` r
dplyr::glimpse(mapeamento_municipios)
#> Rows: 32,536
#> Columns: 10
#> $ mapeamento      <chr> "IBGE - Não identificado", "IBGE - Não identificado", ~
#> $ fonte           <chr> "IBGE - Dados disponibilizados pelo SNIF", "IBGE - Dad~
#> $ ano_base        <chr> "2014", "2015", "2016", "2014", "2015", "2016", "2014"~
#> $ uf              <chr> "RO", "RO", "RO", "RO", "RO", "RO", "RO", "RO", "RO", ~
#> $ estado          <chr> "Rondônia", "Rondônia", "Rondônia", "Rondônia", "Rondô~
#> $ municipio       <chr> "Alta Floresta D'oeste", "Alta Floresta D'oeste", "Alt~
#> $ code_muni       <dbl> 1100015, 1100015, 1100015, 1100015, 1100015, 1100015, ~
#> $ nucleo_regional <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
#> $ genero          <chr> "Eucalyptus", "Eucalyptus", "Eucalyptus", "Pinus", "Pi~
#> $ area_ha         <dbl> 0, 0, 0, 0, 0, 0, 250, 2425, 3316, 0, 0, 0, 0, 0, 0, 3~
```

### Mapeamento em nível de estados:

``` r
dplyr::glimpse(mapeamento_estados)
#> Rows: 989
#> Columns: 10
#> $ mapeamento      <chr> "IBÁ - Relatório Anual 2020", "IBÁ - Relatório Anual 2~
#> $ fonte           <chr> "Pöyry e IBÁ", "Pöyry e IBÁ", "Pöyry e IBÁ", "Pöyry e ~
#> $ ano_base        <chr> "2009", "2010", "2011", "2012", "2013", "2014", "2015"~
#> $ uf              <chr> "MG", "MG", "MG", "MG", "MG", "MG", "MG", "MG", "MG", ~
#> $ estado          <chr> "Minas Gerais", "Minas Gerais", "Minas Gerais", "Minas~
#> $ regiao          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
#> $ nucleo_regional <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
#> $ corede          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
#> $ genero          <chr> "Eucalyptus", "Eucalyptus", "Eucalyptus", "Eucalyptus"~
#> $ area_ha         <dbl> 1300000, 1400000, 1401787, 1438971, 1404429, 1400232, ~
```

A documentação das bases pode ser acessada pelo comando
`?plantiosflorestais::mapeamentos_municipios` e
`?plantiosflorestais::mapeamentos_estado` .

## Fonte de dados

Cada relatório ou mapeamento que foi usado nesse pacote está destacado
na tabela a seguir. A fonte dos dados foi mantida dentro das tibbles
existentes no pacote.

| Responsável | Fonte                                 | Relatório                                                                                                                                                                                            |
|-------------|---------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| IBGE        | IBGE                                  | [Histórico disponível no SNIF](https://dados.gov.br/dataset/sistema-nacional-de-informacoes-florestais-snif/resource/fdf7e4ce-8475-4205-8aad-3f97665b8a41)                                           |
| SFB         | IBGE/PEVS                             | [Florestas do Brasil em resumo 2019](http://www.acr.org.br/uploads/biblioteca/Florestas_Brasil_2019_Portugues.pdf)                                                                                   |
| IBÁ         | IBÁ                                   | [Histórico disponível no SNIF](https://dados.gov.br/dataset/sistema-nacional-de-informacoes-florestais-snif/resource/43251bd6-e2c9-4dc8-93c9-379bf15e29d9)                                           |
| IBÁ         | IBÁ/FGV/Pöyry                         | [IBÁ - Relatório Anual 2020](https://www.iba.org/datafiles/publicacoes/relatorios/relatorio-iba-2020.pdf)                                                                                            |
| APRE        | APRE/UFPR                             | [APRE - Estudo Setorial 2020](https://apreflorestas.com.br/publicacoes/estudo-setorial-apre-2020-2/)                                                                                                 |
| IFPR        | SFB/IFPR                              | [Mapeamentos dos plantios florestais do estado do Paraná](https://apreflorestas.com.br/publicacoes/ifpr-e-sfb-mapeamento-dos-plantios-florestais-do-estado-do-parana/)                               |
| ACR         | ACR/UDESC-CAV                         | [ACR - Anuário Estatístico de Base Florestal para o estado de Santa Catarina 2019](http://www.acr.org.br/uploads/biblioteca/Anuario_ACR_2019_atualizado.pdf)                                         |
| AGEFLOR     | AFUBRA/AGEFLOR, FEPAM, RDK Logs, SEMA | [AGEFLOR - A indústria de base florestal no Rio Grande do Sul 2017](http://www.ageflor.com.br/noticias/wp-content/uploads/2017/08/A-INDUSTRIA-DE-BASE-FLORESTAL-NO-RS-2017.pdf)                      |
| AGEFLOR     | AGEFLOR, FEPAM, Codex, RDK Logs       | [AGEFLOR - O setor de base florestal no Rio Grande do Sul 2020](http://www.ageflor.com.br/noticias/wp-content/uploads/2020/12/O-Setor-de-Base-Florestal-no-Rio-Grande-do-Sul-2020-ano-base-2019.pdf) |
| FAMATO      | IMEA                                  | [Diagnóstico de Florestas Plantadas do Estado do Mato Grosso](http://www.arefloresta.org.br/uploads/downloads/00072201414739.pdf)                                                                    |

O presente pacote não dispensa a leitura e consulta dos relatórios
oficiais, apenas consolida números de áreas. Toda explicação
metodológica sobre os mapeamentos e noção de contexto para cada valor
podem ser encontrados nos respectivos arquivos originais.

Toda informação utilizada advinda desse pacote deve ser devidamente
citada a fonte, usando o mapeamento de referência.

## Funções disponíveis

O pacote contém 7 funções simples, sendo:

-   Quatro delas para explorar os dados

-   Uma para plotar um dos mapeamentos

-   Duas para exportar os dados

As funções podem ser verificadas a seguir:

-   **`mapeamentos_disponiveis()`**

Função utilizada para verificar rapidamente quais os mapeamentos
disponíveis nas bases.

-   **`mapeamento_disponivel_uf()`**

Função utilizada para verificar a área dos mapeamentos existentes para
uma determinada unidade federativa, tendo como argumento uma
`unidade_federativa`. Se não definido nada para o argumento, o padrão é
‘PR’.

-   **`generos_plantios_disponiveis()`**

Exibe os gêneros existentes no pacote. Pode ou não exibir o nome do
mapeamento, com a definição do argumento `exibir_nome_mapeamento`. O
padrão da função é `FALSE`, para não exibir o nome dos mapeamentos.

-   **`series_historicas_disponiveis()`**

Obtém os mapeamentos com séries históricas dentro das bases e suas
respectivas áreas.

-   **`plotar_historico_iba()`**

Função para plotagem de gráficos. Realiza o plot por estado ou para o
país com os dados da Indústria Brasileira de Árvores. Possui dois
argumentos: `abrangecia_uf` e `exibir_rotulos`.

O primeiro argumento recebe uma unidade federativa como string, “PR”,
por exemplo. Caso não seja declarado nenhum estado, o gráfico será com
os dados nacionais. O segundo argumento define se os rótulos de totais
das barras devem ser ou não exibidos, com `TRUE` ou `FALSE`. O padrão é
`FALSE`.

-   **`exportar_xlsx()`**

Função que tem como objetivo exportar as duas bases do pacote em forma
de planilha excel. As duas bases são exportadas para um arquivo .XLSX,
com duas planilhas internas, cada uma representando uma base. O
argumento padrão a ser passado é o caminho do local que o usuário deseja
salvar a base. O endereço da pasta deve acabar com “/”. Caso o usuário
deseje salvar na pasta do projeto ativo, basta usar o argumento como
`./`. Um exemplo de uso para uma pasta do windows seria:
`exportar_xlsx(C:/Users/mayko/Downloads/)`.

-   **`exportar_csv()`**

A lógica é a mesma da função anterior, porém o objetivo é exportar para
um arquivo de texto separado por ‘;’ (ponto e vírgula) com *enconding*
UTF-8. Como são duas bases, a função gera dois arquivos .CSV.
