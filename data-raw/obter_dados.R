## code to prepare `DATASET` dataset goes here

# Usar pipe
usethis::use_pipe()
devtools::document()


# Fazer download dos mapeamentos faxinados --------------------------------

# obter links
mapeamentos_municipios_url <- "https://github.com/maykongpedro/2021-07-04-extracao-mapeamentos-plantios-florestais/raw/master/data/consolidado/mapeamentos_municipios.rds"
mapeamentos_gerais_url <- "https://github.com/maykongpedro/2021-07-04-extracao-mapeamentos-plantios-florestais/raw/master/data/consolidado/mapeamentos_gerais.rds"

# mapeamento com municípios
download.file(mapeamentos_municipios_url,
              destfile = "data-raw/mapeamentos_municipios.rds")


# mapeamento estadual e nacional
download.file(mapeamentos_gerais_url,
              destfile = "data-raw/mapeamentos_gerais.rds")


# Importar dados para o pacote --------------------------------------------
mapeamentos_municipios <- readr::read_rds("./data-raw/mapeamentos_municipios.rds")
mapeamentos_estaduais <- readr::read_rds("./data-raw/mapeamentos_gerais.rds")


usethis::use_data(mapeamentos_municipios)
usethis::use_data(mapeamentos_estaduais)


