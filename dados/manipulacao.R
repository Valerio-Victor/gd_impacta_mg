# PACOTES -----------------------------------------------------------------
library(magrittr, include.only = "%>%")
library(ggplot2)

# IMPORTAÇÃO --------------------------------------------------------------
(
bd_macro_r <- readxl::read_xlsx(path = "real.xlsx",
                                sheet = 1) %>% 
  janitor::clean_names()
)

(
bd_macro_h <- readxl::read_xlsx(path = "hipotetico.xlsx",
                                sheet = 1) %>% 
    janitor::clean_names()
)

mapa <- geobr::read_state() %>% 
  dplyr::mutate(ref = ifelse(abbrev_state == "MG", 1,0))

# ORGANIZAÇÃO -------------------------------------------------------------
(
macro_r <- bd_macro_r %>% 
  dplyr::rename("variavel" = zone_macro_b,
                "mg" = minas_gerais_percent,
                "br" = resto_brasil_percent) %>% 
  dplyr::filter(
    variavel == "Consumo das Famílias"|
      variavel == "Investimento Real" |
      variavel == "Volume de Exportações" |
      variavel == "Volume de Importação" |
      variavel == "PIB Real" |
      variavel == "Emprego Agregado" |
      variavel == "Salário Real Médio" |
      variavel == "Índice de Preços") %>% 
  dplyr::mutate(variavel = dplyr::case_when(
    variavel == "Consumo das Famílias" ~ "Consumo",
    variavel == "Investimento Real" ~ "Investimento",
    variavel == "Volume de Exportações" ~ "Exportações",
    variavel == "Volume de Importação" ~ "Importações",
    variavel == "PIB Real" ~ "PIB Real",
    variavel == "Emprego Agregado" ~ "Emprego",
    variavel == "Salário Real Médio" ~ "Salário",
    variavel == "Índice de Preços" ~ "Inflação"
  )) %>% 
  dplyr::mutate(ref = dplyr::case_when(
    variavel == "Consumo" ~ 2,
    variavel == "Investimento" ~ 3,
    variavel == "Exportações" ~ 4,
    variavel == "Importações" ~ 5,
    variavel == "PIB Real" ~ 1,
    variavel == "Emprego" ~ 1,
    variavel == "Salário" ~ 2,
    variavel == "Inflação" ~ 3
  )) %>% 
  tidyr::pivot_longer(cols = -c(variavel, ref),
                      names_to = "local",
                      values_to = "valor") %>% 
  dplyr::mutate(cenario = "Cenário Real")
)

(
macro_h <- bd_macro_r %>% 
    dplyr::rename("variavel" = zone_macro_b,
                  "mg" = minas_gerais_percent,
                  "br" = resto_brasil_percent) %>% 
    dplyr::filter(
      variavel == "Consumo das Famílias"|
        variavel == "Investimento Real" |
        variavel == "Volume de Exportações" |
        variavel == "Volume de Importação" |
        variavel == "PIB Real" |
        variavel == "Emprego Agregado" |
        variavel == "Salário Real Médio" |
        variavel == "Índice de Preços") %>% 
    dplyr::mutate(variavel = dplyr::case_when(
      variavel == "Consumo das Famílias" ~ "Consumo",
      variavel == "Investimento Real" ~ "Investimento",
      variavel == "Volume de Exportações" ~ "Exportações",
      variavel == "Volume de Importação" ~ "Importações",
      variavel == "PIB Real" ~ "PIB Real",
      variavel == "Emprego Agregado" ~ "Emprego",
      variavel == "Salário Real Médio" ~ "Salário",
      variavel == "Índice de Preços" ~ "Inflação"
    )) %>% 
    dplyr::mutate(ref = dplyr::case_when(
      variavel == "Consumo" ~ 2,
      variavel == "Investimento" ~ 3,
      variavel == "Exportações" ~ 4,
      variavel == "Importações" ~ 5,
      variavel == "PIB Real" ~ 1,
      variavel == "Emprego" ~ 1,
      variavel == "Salário" ~ 2,
      variavel == "Inflação" ~ 3
    )) %>% 
    tidyr::pivot_longer(cols = -c(variavel, ref),
                        names_to = "local",
                        values_to = "valor") %>% 
    dplyr::mutate(cenario = "Cenário Hipotético")
)

(
macro_cenarios <- dplyr::bind_rows(macro_r,
                                   macro_h)
)

# VISUALIZAÇÃO ------------------------------------------------------------
# Macro em Minas Gerais:
(
graf_macro_mg_i_r <- plotly::ggplotly(
  macro_cenarios %>%
    dplyr::filter(local == "mg" & cenario == "Cenário Real") %>% 
    dplyr::filter(variavel != "Salário" &
                    variavel != "Inflação" &
                    variavel != "Emprego") %>% 
    dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>% 
    ggplot() + 
    geom_col(aes(y = variavel,
                 x = valor/100,
                 fill = valor >= 0, 
                 group = 1,
                 text = paste0("Variável: ", variavel,
                               "<br>Variação: ", scales::percent(valor/100,
                                                            accuracy = 0.01,
                                                            decimal.mark = ","))),
             width = 0.7) + 
    scale_fill_manual(values = c("TRUE"  = "#1F5A7A", "FALSE" = "#B44C43")) + 
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.1,
                                                       decimal.mark = ","),
                       breaks = seq(from = -0.125, to = 0.125, by = 0.025),
                       limits = c(0, 0.10)) + 
    labs(title = "Impacto Econômico (Minas Gerais)",
         x = "Variação Percentual",
         y = "") + 
    theme_light() + 
    theme(legend.position = "none"),
  tooltip = "text"
) 
)

(
graf_macro_mg_ii_r <- plotly::ggplotly(
  macro_cenarios %>%
    dplyr::filter(local == "mg" & cenario == "Cenário Real") %>% 
    dplyr::filter(variavel == "Salário" |
                    variavel == "Inflação" |
                    variavel == "Emprego") %>% 
    dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>% 
    ggplot() + 
    geom_col(aes(y = variavel,
                 x = valor/100,
                 fill = valor >= 0,
                 group = 1,
                 text = paste0("Variável: ", variavel,
                               "<br>Variação: ", scales::percent(valor/100,
                                                                 accuracy = 0.01,
                                                                 decimal.mark = ","))),
             width = 0.7 * 3/5) + 
    scale_fill_manual(values = c("TRUE"  = "#1F5A7A", "FALSE" = "#B44C43")) + 
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.1,
                                                       decimal.mark = ","),
                       breaks = seq(from = -0.075, to = 0.075, by = 0.025),
                       limits = c(-0.075, 0.075)) + 
    labs(title = "Impacto Econômico (Minas Gerais)",
         x = "Variação Percentual",
         y = "") + 
    theme_light() + 
    theme(legend.position = "none"),
  tooltip = "text"
  
)
)

# Macro no Resto do Brasil
(
  graf_macro_br_ii_r <- plotly::ggplotly(
    macro_cenarios %>%
      dplyr::filter(local == "br" & cenario == "Cenário Hipotético") %>% 
      dplyr::filter(variavel != "Salário" &
                      variavel != "Inflação" &
                      variavel != "Emprego") %>% 
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>% 
      ggplot() + 
      geom_col(aes(y = variavel,
                   x = valor/100,
                   fill = valor >= 0, 
                   group = 1,
                   text = paste0("Variável: ", variavel,
                                 "<br>Variação: ", scales::percent(valor/100,
                                                                   accuracy = 0.01,
                                                                   decimal.mark = ","))),
               width = 0.7) + 
      scale_fill_manual(values = c("TRUE"  = "#1F5A7A", "FALSE" = "#B44C43")) + 
      scale_x_continuous(labels = scales::percent_format(accuracy = 0.1,
                                                         decimal.mark = ","),
                         breaks = seq(from = -0.125, to = 0.125, by = 0.025),
                         limits = c(-0.10, 0.10)) + 
      labs(title = "Impacto Econômico (Demais Estados do Brasil)",
           x = "Variação Percentual",
           y = "") + 
      theme_light() + 
      theme(legend.position = "none"),
    tooltip = "text"
  ) 
)

(
  graf_macro_br_ii_r <- plotly::ggplotly(
    macro_cenarios %>%
      dplyr::filter(local == "br" & cenario == "Cenário Hipotético") %>% 
      dplyr::filter(variavel == "Salário" |
                      variavel == "Inflação" |
                      variavel == "Emprego") %>% 
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>% 
      ggplot() + 
      geom_col(aes(y = variavel,
                   x = valor/100,
                   fill = valor >= 0,
                   group = 1,
                   text = paste0("Variável: ", variavel,
                                 "<br>Variação: ", scales::percent(valor/100,
                                                                   accuracy = 0.01,
                                                                   decimal.mark = ","))),
               width = 0.7 * 3/5) + 
      scale_fill_manual(values = c("TRUE"  = "#1F5A7A", "FALSE" = "#B44C43")) + 
      scale_x_continuous(labels = scales::percent_format(accuracy = 0.1,
                                                         decimal.mark = ","),
                         breaks = seq(from = -0.075, to = 0.075, by = 0.025),
                         limits = c(-0.075, 0.075)) + 
      labs(title = "Impacto Econômico (Demais Estados do Brasil)",
           x = "Variação Percentual",
           y = "") + 
      theme_light() + 
      theme(legend.position = "none"),
    tooltip = "text"
    
  )
)

mapa %>% 
  dplyr::filter(abbrev_state == "MG") %>% 
  ggplot() + 
  geom_sf(aes(fill = as.factor(ref)), color = "#1F5A7A") + 
  scale_fill_manual(values = c('1' = "#1F5A7A", '0' = "#fff")) + 
  theme_void() +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

mapa %>% 
  ggplot() + 
  geom_sf(aes(fill = as.factor(ref)), color = "#1F5A7A") + 
  scale_fill_manual(values = c('0' = "#1F5A7A", '1' = "#fff")) + 
  theme_void() +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

