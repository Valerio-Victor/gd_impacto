# PACOTES: ----------------------------------------------------------------
library(magrittr, include.only = '%>%')
library(ggplot2)
library(nasapower)


# IMPORTAÇÃO: -------------------------------------------------------------
url_aneel <- 'https://dadosabertos.aneel.gov.br/'

ckanr::ckanr_setup(url_aneel)

url_dados <- ckanr::resource_show(id = 'b1bd71e7-d0ad-4214-9053-cbd58e9564a7')

url_final <- (url_dados$url)

dados <- readr::read_delim(file = url_final,
                           delim = ';',
                           escape_double = FALSE,
                           col_types = readr::cols(
                             DatGeracaoConjuntoDados =
                               readr::col_date(format = '%Y-%m-%d'),
                             AnmPeriodoReferencia =
                               readr::col_date(format = '%m/%Y'),
                             DthAtualizaCadastralEmpreend =
                               readr::col_date(format = '%Y-%m-%d'),
                             NumCoordNEmpreendimento =
                               readr::col_character(),
                             NumCoordEEmpreendimento =
                               readr::col_character()),
                           locale = readr::locale(encoding = 'ISO-8859-1'),
                           trim_ws = TRUE)


# ARRUMAÇÃO: --------------------------------------------------------------
dados_brutos <- dados %>%
  janitor::clean_names()

colnames(dados_brutos)

verde <- '#1c5253'

# TRATAMENTO E VISUALIZAÇÃO -----------------------------------------------
inst_ano <- dados_brutos %>%
  dplyr::filter(sig_tipo_geracao == 'UFV') %>%
  dplyr::select(dth_atualiza_cadastral_empreend) %>%
  dplyr::rename('data' = dth_atualiza_cadastral_empreend) %>%
  dplyr::mutate(data = lubridate::year(data),
                inst = 1) %>%
  dplyr::filter(data >= 2012) %>%
  dplyr::group_by(data) %>%
  dplyr::summarise(anual = sum(inst)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(acum = cumsum(anual))

inst_ano_graf <- inst_ano %>%
  ggplot() +
  geom_line(aes(x = data, y = acum),
            color = verde,
            size = 1) +
  geom_point(aes(x = data, y = acum),
             color = verde,
             size = 3) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    accuracy = 1),
                     limits = c(0,3000000)) +
  scale_x_continuous(breaks = seq(2012, 2024, 2)) +
  labs(title = 'Evolução das Instalações de UFV no Brasil',
       subtitle = 'Fonte: ANEEL',
       x = 'Ano',
       y = 'Instalações') +
  theme_minimal()

inst_ano_graf


inst_estado <- dados_brutos %>%
  dplyr::filter(sig_tipo_geracao == 'UFV') %>%
  dplyr::filter(sig_uf == 'MG') %>%
  dplyr::select(dth_atualiza_cadastral_empreend) %>%
  dplyr::rename('data' = dth_atualiza_cadastral_empreend) %>%
  dplyr::mutate(data = lubridate::year(data),
                inst = 1) %>%
  dplyr::filter(data >= 2012) %>%
  dplyr::group_by(data) %>%
  dplyr::summarise(anual_mg = sum(inst)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(acum_mg = cumsum(anual_mg)) %>%
  dplyr::left_join(inst_ano, by = c('data' = 'data')) %>%
  dplyr::mutate(valor = acum_mg/acum)

inst_estado_graf <- inst_estado %>%
  ggplot() +
  geom_line(aes(x = data, y = valor),
            color = verde,
            size = 1) +
  geom_point(aes(x = data, y = valor),
             color = verde,
             size = 3) +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                    accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(2012, 2024, 2)) +
  labs(title = 'Evolução das Instalações de UFV em MG',
       subtitle = 'Fonte: ANEEL',
       x = 'Ano',
       y = 'Proporção do Total') +
  theme_minimal() +
  theme(legend.position = 'none')

inst_estado_graf


inst_pot <- dados_brutos %>%
  dplyr::filter(sig_tipo_geracao == 'UFV') %>%
  dplyr::select(dth_atualiza_cadastral_empreend, mda_potencia_instalada_kw) %>%
  dplyr::rename('data' = dth_atualiza_cadastral_empreend,
                'potencia' = mda_potencia_instalada_kw) %>%
  dplyr::mutate(data = lubridate::year(data)) %>%
  dplyr::filter(data >= 2012) %>%
  dplyr::mutate(potencia = as.numeric(stringr::str_replace(potencia,
                                                pattern = ',',
                                                replacement = '.')))


inst_pot_graf <- inst_pot %>%
  ggplot() +
  geom_histogram(aes(fill = data, y = potencia))

inst_pot_graf

ggsave('./docs/inst_ano_graf.png',
       inst_ano_graf,
       width = 5,
       height = 5,
       dpi = 300)

ggsave('./docs/inst_estado_graf.png',
       inst_estado_graf,
       width = 5,
       height = 5,
       dpi = 300)

