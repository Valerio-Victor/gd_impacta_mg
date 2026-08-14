# PACOTES: ----------------------------------------------------------------
library(magrittr, include.only = '%>%')
library(ggplot2)
library(shiny)
library(bslib)
library(plotly)
library(sf)

# IMPORTAÇÃO: -------------------------------------------------------------
agregados <- readxl::read_xlsx(path = 'macro_final.xlsx',
                               sheet = 'Agregados Macroeconômicos') %>% 
  tidyr::pivot_longer(cols = c(mg, resto),
                      names_to = 'local',
                      values_to = 'valor') %>% 
  dplyr::mutate(ref = dplyr::case_when(
    variavel == 'PIB Real' ~ 1,
    variavel == 'Consumo das Famílias' ~ 2,
    variavel == 'Investimento Real' ~ 3,
    variavel == 'Volume de Importações' ~ 4,
    variavel == 'Volume de Exportações' ~ 5,
    variavel == 'Emprego Agregado' ~ 6,
    variavel == 'Salário Real Médio' ~ 7,
    variavel == 'Índice de Preços' ~ 8)) %>% 
  dplyr::arrange(local, cenario) 

producao_setorial <- readxl::read_xlsx(path = 'macro_final.xlsx',
                                       sheet = 'Produção Setorial') %>% 
  dplyr::rename('valor' = mg) %>% 
  dplyr::mutate(ref = dplyr::case_when(
    variavel == 'Agropecuária' ~ 1,
    variavel == 'Indústria' ~ 2,
    variavel == 'Serviço' ~ 3,
    variavel == 'Extrativa' ~ 4,
    variavel == 'Comércio' ~ 5,
    variavel == 'Transporte' ~ 6)) %>% 
  dplyr::arrange(cenario)

icms_setorial <- readxl::read_xlsx(path = 'macro_final.xlsx',
                                   sheet = 'ICMS Setorial') %>% 
  dplyr::rename('valor' = mg) %>% 
  dplyr::mutate(ref = dplyr::case_when(
    variavel == 'Agropecuária' ~ 1,
    variavel == 'Indústria' ~ 2,
    variavel == 'Serviço' ~ 3,
    variavel == 'Extrativa' ~ 4,
    variavel == 'Comércio' ~ 5,
    variavel == 'Transporte' ~ 6)) %>% 
  dplyr::arrange(cenario)

tradutor <- readxl::read_xlsx(path = 'tradutor_gempack.xlsx',
                              sheet = 'final')

micro_mg <- readRDS(file = 'geobr.rds')

agregados_micro <- readxl::read_xlsx(path = 'microrregioes_final.xlsx',
                                     sheet = 'Agregados - Micro') %>% 
  tidyr::pivot_longer(cols = -c(cenario,variavel),
                      names_to = 'micro',
                      values_to = 'valor') %>% 
  dplyr::full_join(tradutor, by = c('micro' = 'resultado')) %>% 
  dplyr::mutate(micro = geobr) %>% 
  dplyr::select(-geobr) %>% 
  dplyr::mutate(ref = dplyr::case_when(
    variavel == 'PIB Real' ~ 1,
    variavel == 'Consumo das Famílias' ~ 2,
    variavel == 'Investimento Real' ~ 3,
    variavel == 'Volume de Importações' ~ 4,
    variavel == 'Volume de Exportações' ~ 5,
    variavel == 'Emprego Agregado' ~ 6,
    variavel == 'Salário Real Médio' ~ 7,
    variavel == 'Índice de Preços' ~ 8)) %>% 
  dplyr::arrange(cenario) 

agregados_micro <- micro_mg %>% 
    dplyr::left_join(agregados_micro, by = c('name_micro' = 'micro'))

producao_micro <- readxl::read_xlsx(path = 'microrregioes_final.xlsx',
                                    sheet = 'Produção - Micro') %>% 
  tidyr::pivot_longer(cols = -c(cenario,variavel),
                      names_to = 'micro',
                      values_to = 'valor') %>% 
  dplyr::full_join(tradutor, by = c('micro' = 'resultado')) %>% 
  dplyr::mutate(micro = geobr) %>% 
  dplyr::select(-geobr) %>% 
  dplyr::mutate(ref = dplyr::case_when(
    variavel == 'Agropecuária' ~ 1,
    variavel == 'Indústria' ~ 2,
    variavel == 'Serviço' ~ 3,
    variavel == 'Extrativa' ~ 4,
    variavel == 'Comércio' ~ 5,
    variavel == 'Transporte' ~ 6)) %>% 
  dplyr::arrange(cenario) 

producao_micro <- micro_mg %>% 
  dplyr::full_join(producao_micro, by = c('name_micro' = 'micro'))

investimento_micro <- readxl::read_xlsx(path = 'microrregioes_final.xlsx',
                                        sheet = 'Investimento - Micro') %>% 
  tidyr::pivot_longer(cols = -c(cenario,variavel),
                      names_to = 'micro',
                      values_to = 'valor') %>% 
  dplyr::full_join(tradutor, by = c('micro' = 'resultado')) %>% 
  dplyr::mutate(micro = geobr) %>% 
  dplyr::select(-geobr)  %>% 
  dplyr::mutate(ref = dplyr::case_when(
    variavel == 'Agropecuária' ~ 1,
    variavel == 'Indústria' ~ 2,
    variavel == 'Serviço' ~ 3,
    variavel == 'Extrativa' ~ 4,
    variavel == 'Comércio' ~ 5,
    variavel == 'Transporte' ~ 6)) %>% 
  dplyr::arrange(cenario) 

investimento_micro <- micro_mg %>% 
  dplyr::full_join(investimento_micro, by = c('name_micro' = 'micro'))

icms_micro <- readxl::read_xlsx(path = 'microrregioes_final.xlsx',
                                sheet = 'ICMS - Micro') %>% 
  tidyr::pivot_longer(cols = -c(cenario,variavel),
                      names_to = 'micro',
                      values_to = 'valor') %>% 
  dplyr::full_join(tradutor, by = c('micro' = 'resultado')) %>% 
  dplyr::mutate(micro = geobr) %>% 
  dplyr::select(-geobr)  %>% 
  dplyr::mutate(ref = dplyr::case_when(
    variavel == 'Agropecuária' ~ 1,
    variavel == 'Indústria' ~ 2,
    variavel == 'Serviço' ~ 3,
    variavel == 'Extrativa' ~ 4,
    variavel == 'Comércio' ~ 5,
    variavel == 'Transporte' ~ 6)) %>% 
  dplyr::arrange(cenario) 

icms_micro <- micro_mg %>% 
  dplyr::full_join(icms_micro, by = c('name_micro' = 'micro'))

azul <- '#1F5A7A'
verde <- '#00B5A1'

# INTERFACE DO USUÁRIO: ---------------------------------------------------
ui <- page_navbar(
title = "GD Impacta MG",
  
theme = bs_theme(
  version = 5,
  bootswatch = "flatly",
  
  primary = "#1F5A7A",
  secondary = "#5C6B73",
  success = "#00B5A1",
  info = "#3F88C5",
  warning = "#C7922C",
  danger = "#8c3949",
  
  bg = "#F7F9FB",
  fg = "#263238",
  
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono"),
  
  border_radius = "0.3rem"
),

# PROJETO ---------------------------------------------------------------
nav_menu(
  title = "Projeto",

nav_panel(
title = 'Sobre o APP',
card(
  fill = FALSE,
  card_header('PROJETO DE PESQUISA',
              style = "background-color: #1F5A7A; color: white;"),
  card_body(
    fillable = FALSE,
    shiny::includeHTML("guia_info_projeto.html")
  )
)
),

nav_panel(
title = 'Pesquisadores',
card(
  fill = FALSE,
  card_header('EQUIPE DO PROJETO',
              style = "background-color: #1F5A7A; color: white;"),
  card_body(
    fillable = FALSE,
    shiny::includeHTML("guia_info_equipe.html")
  )
)
),

nav_panel(
title = 'Licença de Uso',
card(
  fill = FALSE,
  card_header('LICENÇA DE USO',
              style = "background-color: #1F5A7A; color: white;"),
  card_body(
    fillable = FALSE,
    shiny::includeHTML("guia_info_licenca.html")
  )
)
)

),

# ANÁLISE FINANCEIRA ----------------------------------------------------
# nav_menu(
#   title = "Análise financeira",
#   
#   nav_panel(
#     title = "Mapas financeiros",
#     
#     layout_sidebar(
#       sidebar = sidebar(
#         title = "Filtros",
#         
#         selectInput(
#           inputId = "cenario_financeiro",
#           label = "Cenário",
#           choices = c(
#             "Cenário real",
#             "Cenário hipotético",
#             "Comparação"
#           )
#         ),
#         
#         selectInput(
#           inputId = "indicador_financeiro",
#           label = "Indicador financeiro",
#           choices = c(
#             "Valor presente líquido",
#             "Taxa interna de retorno",
#             "Payback",
#             "Custo nivelado de energia"
#           )
#         )
#       ),
#       
#       card(
#         full_screen = TRUE,
#         
#         card_header("Distribuição espacial dos resultados financeiros"),
#         
#         card_body(
#           plotOutput(
#             outputId = "mapa_financeiro",
#             height = "650px"
#           )
#         )
#       )
#     )
#   ),
#   
#   nav_panel(
#     title = "Calculadora financeira",
#     
#     layout_sidebar(
#       sidebar = sidebar(
#         title = "Parâmetros",
#         
#         numericInput(
#           inputId = "investimento_inicial",
#           label = "Investimento inicial",
#           value = 100000,
#           min = 0
#         ),
#         
#         numericInput(
#           inputId = "economia_anual",
#           label = "Economia anual estimada",
#           value = 15000,
#           min = 0
#         ),
#         
#         numericInput(
#           inputId = "taxa_desconto",
#           label = "Taxa de desconto (%)",
#           value = 10,
#           min = 0
#         ),
#         
#         numericInput(
#           inputId = "vida_util",
#           label = "Vida útil do projeto",
#           value = 25,
#           min = 1
#         ),
#         
#         actionButton(
#           inputId = "calcular",
#           label = "Calcular",
#           class = "btn-primary"
#         )
#       ),
#       
#       layout_column_wrap(
#         width = 1 / 3,
#         
#         value_box(
#           title = "Valor presente líquido",
#           value = textOutput("valor_vpl")
#         ),
#         
#         value_box(
#           title = "Taxa interna de retorno",
#           value = textOutput("valor_tir")
#         ),
#         
#         value_box(
#           title = "Payback",
#           value = textOutput("valor_payback")
#         )
#       ),
#       
#       card(
#         full_screen = TRUE,
#         
#         card_header("Fluxo de caixa do projeto"),
#         
#         card_body(
#           plotOutput(
#             outputId = "grafico_fluxo_caixa",
#             height = "500px"
#           )
#         )
#       )
#     )
#   )
# ),

# RESULTADOS ECONÔMICOS -------------------------------------------------
nav_menu(
title = 'Impacto Econômico',

nav_panel(
title = 'Resultado Agregado',
card(
  fill = FALSE,
  card_header('IMPACTO ECONÔMICO ACUMULADO EM MINAS GERAIS (2015-2025)',
              style = "background-color: #1F5A7A; color: white;"),
  card_body(
    fillable = FALSE,
    layout_columns(
      col_widths = c(6,6),
      gap = '1rem',
      plotlyOutput("graf31_1"),
      plotlyOutput("graf31_2"),
      plotlyOutput("graf31_3"),
      plotlyOutput("graf31_4")
    )
  )
),
card(
  fill = FALSE,
  card_header('IMPACTO ECONÔMICO ACUMULADO NOS DEMAIS ESTADOS (2015-2025)',
              style = "background-color: #1F5A7A; color: white;"),
  card_body(
    fillable = FALSE,
    layout_columns(
      col_widths = c(6,6),
      gap = '1rem',
      plotlyOutput("graf31_5"),
      plotlyOutput("graf31_6")
    )
  )
)
),

nav_panel(
  title = 'Distribuição Espacial',
  
layout_sidebar(
sidebar = sidebar(
  title = 'FILTROS',
  
  h6(
    'Distribuição Espacial',
    class = 'mt-2 mb-3'
  ),
  
  selectInput(
    inputId = 'cenario_microrregiao',
    label = 'Cenário',
    choices = c(
      'Real',
      'Hipotético'
    )
  ),
  
  selectInput(
    inputId = 'indicador_microrregiao',
    label = 'Indicador Agregado',
    choices = c(
      'Consumo das Famílias',
      'Investimento Real',
      'Volume de Exportações',
      'Volume de Importações',
      'PIB Real',
      'Emprego Agregado',
      'Salário Real Médio',
      'Índice de Preços'
    )
  ),
  
  selectInput(
    inputId = 'setor_microrregiao',
    label = 'Setor Econômico',
    choices = c(
      'Agropecuária',
      'Extrativa',
      'Indústria',
      'Serviço',
      'Comércio',
      'Transporte'
    )
  ),
  
  tags$hr(),
  
  h6(
    'Resultado da Microrregião',
    class = 'mt-2 mb-3'
  ),
  
  selectInput(
    inputId = 'selecao_microrregiao',
    label = 'Microrregião',
    choices = c(
      'Unaí',
      'Paracatu',
      'Januária',
      'Janaúba',
      'Salinas',
      'Pirapora',
      'Montes Claros',
      'Grão Mogol',
      'Bocaiúva',
      'Diamantina',
      'Capelinha',
      'Araçuaí',
      'Pedra Azul',
      'Almenara',
      'Teófilo Otoni',
      'Nanuque',
      'Ituiutaba',
      'Uberlândia',
      'Patrocínio',
      'Patos de Minas',
      'Frutal',
      'Uberaba',
      'Araxá',
      'Três Marias',
      'Curvelo',
      'Bom Despacho',
      'Sete Lagoas',
      'Conceição do Mato Dentro',
      'Pará de Minas',
      'Belo Horizonte',
      'Itabira',
      'Itaguara',
      'Ouro Preto',
      'Conselheiro Lafaiete',
      'Guanhães',
      'Peçanha',
      'Governador Valadares',
      'Mantena',
      'Ipatinga',
      'Caratinga',
      'Aimorés',
      'Piuí',
      'Divinópolis',
      'Formiga',
      'Campo Belo',
      'Oliveira',
      'Passos',
      'São Sebastião do Paraíso',
      'Alfenas',
      'Varginha',
      'Poços de Caldas',
      'Pouso Alegre',
      'Santa Rita do Sapucaí',
      'São Lourenço',
      'Andrelândia',
      'Itajubá',
      'Lavras',
      'São João del Rei',
      'Barbacena',
      'Ponte Nova',
      'Manhuaçu',
      'Viçosa',
      'Muriaé',
      'Ubá',
      'Juiz de Fora',
      'Cataguases'
    )
  )
),
card(
  fill = FALSE,
  card_header('DISTRIBUIÇÃO ESPACIAL DO IMPACTO ACUMULADO (2015-2025)',
              style = "background-color: #1F5A7A; color: white;"),
  card_body(
    fillable = FALSE,
    layout_columns(
      col_widths = c(6,6),
      gap = '1rem',
      plotlyOutput("graf32_1"),
      plotlyOutput("graf32_2"),
      plotlyOutput("graf32_3"),
      plotlyOutput("graf32_4")
    )
  )
),
card(
  fill = FALSE,
  card_header('IMPACTO ACUMULADO NA MICRORREGIÃO SELECIONADA (2015-2025)',
              style = "background-color: #1F5A7A; color: white;"),
  card_body(
    fillable = FALSE,
    layout_columns(
      col_widths = c(6,6),
      gap = '1rem',
      plotlyOutput("graf32_5"),
      plotlyOutput("graf32_6"),
      plotlyOutput("graf32_7"),
      plotlyOutput("graf32_8")
    )
  )
)







)
),
)
)


# SERVIDOR ----------------------------------------------------------------
server <- function(input, output, session) {


# NAVEGAÇÃO 3.1 (IMPACTO ECONÔMICO - RESULTADOS AGREGADOS) ----------------
output$graf31_1 <- renderPlotly({
  plotly::ggplotly(
    agregados %>%
      dplyr::filter(local == 'mg' & ref <= 5) %>%
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>%
      ggplot() +
      geom_col(aes(x = valor/100,
                   y = variavel,
                   fill = cenario,
                   text = paste0('Variável: ', variavel,
                                 '<br>Valor: ', scales::percent(
                                   valor/100,
                                   decimal.mark = ',',
                                   accuracy = 0.01),
                                 '<br>Cenário: ', cenario)),
               position = 'dodge',
               width = 0.7) +
      scale_x_continuous(labels = scales::percent_format(decimal.mark = ',')) +
      scale_fill_manual(values = c('Hipotético' = verde, 'Real' = azul)) +
      labs(title = 'Atividade Econômica e Demanda Agregada',
           x = '',
           y = '',
           fill = '') +
      theme_light() +
      theme(legend.position = 'bottom'),
    tooltip = 'text') %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )
})

output$graf31_2 <- renderPlotly({
  plotly::ggplotly(
    agregados %>%
      dplyr::filter(local == 'mg' & ref >= 6) %>%
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>%
      ggplot() +
      geom_col(aes(x = valor/100,
                   y = variavel,
                   fill = cenario,
                   text = paste0('Variável: ', variavel,
                                 '<br>Valor: ', scales::percent(
                                   valor/100,
                                   decimal.mark = ',',
                                   accuracy = 0.01),
                                 '<br>Cenário: ', cenario)),
               position = 'dodge',
               width = 0.7*3/5) +
      scale_x_continuous(labels = scales::percent_format(decimal.mark = ',')) +
      scale_fill_manual(values = c('Hipotético' = verde, 'Real' = azul)) +
      labs(title = 'Trabalho e Preços',
           x = '',
           y = '',
           fill = '') +
      theme_light() +
      theme(legend.position = 'bottom'),
    tooltip = 'text') %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )
})

output$graf31_3 <- renderPlotly({
  plotly::ggplotly(
    producao_setorial %>%
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>%
      ggplot() +
      geom_col(aes(x = valor,
                   y = variavel,
                   fill = cenario,
                   text = paste0('Variável: ', variavel,
                                 '<br>Valor: ', scales::percent(
                                   valor,
                                   decimal.mark = ',',
                                   accuracy = 0.01),
                                 '<br>Cenário: ', cenario)),
               position = 'dodge',
               width = 0.7) +
      scale_x_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                         accuracy = 0.1)) +
      scale_fill_manual(values = c('Hipotético' = verde, 'Real' = azul)) +
      labs(title = 'Produção Setorial',
           x = '',
           y = '',
           fill = '') +
      theme_light() +
      theme(legend.position = 'bottom'),
    tooltip = 'text') %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )
})

output$graf31_4 <- renderPlotly({
  plotly::ggplotly(
    icms_setorial %>%
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>%
      ggplot() +
      geom_col(aes(x = valor,
                   y = variavel,
                   fill = cenario,
                   text = paste0('Variável: ', variavel,
                                 '<br>Valor: ', scales::dollar(
                                   valor,
                                   big.mark = '.',
                                   decimal.mark = ',',
                                   prefix = 'R$ ',
                                   accuracy = 1), ' Milhões',
                                 '<br>Cenário: ', cenario)),
               position = 'dodge',
               width = 0.7) +
      scale_x_continuous(labels = scales::dollar_format(big.mark = '.',
                                                        decimal.mark = ',',
                                                        prefix = 'R$ ')) +
      scale_fill_manual(values = c('Hipotético' = verde, 'Real' = azul)) +
      labs(title = 'Arrecadação de ICMS Setorial (Em Milhões)',
           x = '',
           y = '',
           fill = '') +
      theme_light() +
      theme(legend.position = 'bottom'),
    tooltip = 'text') %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )

})

output$graf31_5 <- renderPlotly({
  plotly::ggplotly(
    agregados %>%
      dplyr::filter(local == 'resto' & ref <= 5) %>%
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>%
      ggplot() +
      geom_col(aes(x = valor/100,
                   y = variavel,
                   fill = cenario,
                   text = paste0('Variável: ', variavel,
                                 '<br>Valor: ', scales::percent(
                                   valor/100,
                                   decimal.mark = ',',
                                   accuracy = 0.01),
                                 '<br>Cenário: ', cenario)),
               position = 'dodge',
               width = 0.7) +
      scale_x_continuous(labels = scales::percent_format(decimal.mark = ',')) +
      scale_fill_manual(values = c('Hipotético' = verde, 'Real' = azul)) +
      labs(title = 'Atividade Econômica e Demanda Agregada',
           x = '',
           y = '',
           fill = '') +
      theme_light() +
      theme(legend.position = 'bottom'),
    tooltip = 'text') %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )
})

output$graf31_6 <- renderPlotly({
  plotly::ggplotly(
    agregados %>%
      dplyr::filter(local == 'mg' & ref >= 6) %>%
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>%
      ggplot() +
      geom_col(aes(x = valor/100,
                   y = variavel,
                   fill = cenario,
                   text = paste0('Variável: ', variavel,
                                 '<br>Valor: ', scales::percent(
                                   valor/100,
                                   decimal.mark = ',',
                                   accuracy = 0.01),
                                 '<br>Cenário: ', cenario)),
               position = 'dodge',
               width = 0.7*3/5) +
      scale_x_continuous(labels = scales::percent_format(decimal.mark = ',')) +
      scale_fill_manual(values = c('Hipotético' = verde, 'Real' = azul)) +
      labs(title = 'Trabalho e Preços',
           x = '',
           y = '',
           fill = '') +
      theme_light() +
      theme(legend.position = 'bottom'),
    tooltip = 'text') %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )
})


# NAVEGAÇÃO 3.2 (IMPACTO ECONÔMICO - RESULTADOS MICRORREGIÕES) ------------
output$graf32_1 <- renderPlotly({
  plotly::ggplotly(
    agregados_micro %>%
      dplyr::filter(cenario == input$cenario_microrregiao & 
                      variavel == input$indicador_microrregiao) %>% 
      ggplot() +
      geom_sf(aes(fill = valor, 
                  text = paste0(
                    '<br>Microrregião: ', name_micro,
                    '<br>Variação: ', scales::percent(valor/100,
                                                      decimal.mark = ',',
                                                      accuracy = 0.01))),
              color = azul) + 
      scale_fill_gradient(low = "#E8F0F4", 
                          high = ifelse(input$cenario_microrregiao == 'Real',
                                        azul,
                                        verde),
                          labels = scales::label_number(accuracy = 1,
                                                        decimal.mark = ',')) +
      labs(title = input$indicador_microrregiao,
           fill = 'Δ%') + 
      theme_minimal() +
      theme(legend.position = 'bottom',
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            panel.grid = element_blank()),
    tooltip = 'text') %>%
    plotly::style(hoveron = 'fills') 
})

output$graf32_2 <- renderPlotly({
  plotly::ggplotly(
    producao_micro %>%
      dplyr::filter(cenario == input$cenario_microrregiao & 
                      variavel == input$setor_microrregiao) %>% 
      ggplot() +
      geom_sf(aes(fill = valor, 
                  text = paste0(
                    '<br>Microrregião: ', name_micro,
                    '<br>Variação: ', scales::percent(valor/100,
                                                      decimal.mark = ',',
                                                      accuracy = 0.01))),
              color = azul) + 
      scale_fill_gradient(low = "#E8F0F4", 
                          high = ifelse(input$cenario_microrregiao == 'Real',
                                        azul,
                                        verde),
                          labels = scales::label_number(accuracy = 1,
                                                        decimal.mark = ',')) +
      labs(title = 'Produção Setorial',
           fill = 'Δ%') + 
      theme_minimal() +
      theme(legend.position = 'bottom',
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            panel.grid = element_blank()),
    tooltip = 'text') %>%
    plotly::style(hoveron = 'fills')
})

output$graf32_3 <- renderPlotly({
  plotly::ggplotly(
    investimento_micro %>%
      dplyr::filter(cenario == input$cenario_microrregiao & 
                      variavel == input$setor_microrregiao) %>% 
      ggplot() +
      geom_sf(aes(fill = valor, 
                  text = paste0(
                    '<br>Microrregião: ', name_micro,
                    '<br>Variação: ', scales::percent(valor/100,
                                                      decimal.mark = ',',
                                                      accuracy = 0.01))),
              color = azul) + 
      scale_fill_gradient(low = "#E8F0F4", 
                          high = ifelse(input$cenario_microrregiao == 'Real',
                                        azul,
                                        verde),
                          labels = scales::label_number(accuracy = 1,
                                                        decimal.mark = ',')) +
      labs(title = 'Investimento Setorial',
           fill = 'Δ%') + 
      theme_minimal() +
      theme(legend.position = 'bottom',
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            panel.grid = element_blank()),
    tooltip = 'text') %>%
    plotly::style(hoveron = 'fills')
})

output$graf32_4 <- renderPlotly({
  plotly::ggplotly(
    icms_micro %>%
      dplyr::filter(cenario == input$cenario_microrregiao & 
                      variavel == input$setor_microrregiao) %>% 
      ggplot() +
      geom_sf(aes(fill = valor/1000000, 
                  text = paste0(
                    '<br>Microrregião: ', name_micro,
                    '<br>Variação: ', scales::dollar(valor/1000000,
                                                     decimal.mark = ',',
                                                     big.mark = '.',
                                                     accuracy = 0.01,
                                                     prefix = 'R$ '), 
                    ' Milhões')),
              color = azul) + 
      scale_fill_gradient(low = "#E8F0F4", 
                          high = ifelse(input$cenario_microrregiao == 'Real',
                                        azul,
                                        verde),
                          labels = scales::label_number(accuracy = 1,
                                                        decimal.mark = ',',
                                                        big.mark = '.')) +
      labs(title = 'Arrecadação Setorial (Em Milhões de R$)',
           fill = 'R$') + 
      theme_minimal() +
      theme(legend.position = 'bottom',
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            panel.grid = element_blank()),
    tooltip = 'text') %>%
    plotly::style(hoveron = 'fills')
})

output$graf32_5 <- renderPlotly({
  plotly::ggplotly(
    agregados_micro %>%
      dplyr::filter(name_micro == input$selecao_microrregiao & ref <= 6) %>%
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>%
      ggplot() +
      geom_col(aes(x = valor/100,
                   y = variavel,
                   fill = cenario,
                   text = paste0('Variável: ', variavel,
                                 '<br>Valor: ', scales::percent(
                                   valor/100,
                                   decimal.mark = ',',
                                   accuracy = 0.01),
                                 '<br>Cenário: ', cenario,
                                 '<br>Microrregião: ', input$selecao_microrregiao)),
               position = 'dodge',
               width = 0.7) +
      scale_x_continuous(labels = scales::percent_format(decimal.mark = ',')) +
      scale_fill_manual(values = c('Hipotético' = verde, 'Real' = azul)) +
      labs(title = 'Atividade Econômica e Demanda Agregada',
           x = '',
           y = '',
           fill = '') +
      theme_light() +
      theme(legend.position = 'bottom'),
    tooltip = 'text') %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )
})

output$graf32_6 <- renderPlotly({
  plotly::ggplotly(
    agregados_micro %>%
      dplyr::filter(name_micro == input$selecao_microrregiao & ref >= 6) %>%
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>%
      ggplot() +
      geom_col(aes(x = valor/100,
                   y = variavel,
                   fill = cenario,
                   text = paste0('Variável: ', variavel,
                                 '<br>Valor: ', scales::percent(
                                   valor/100,
                                   decimal.mark = ',',
                                   accuracy = 0.01),
                                 '<br>Cenário: ', cenario)),
               position = 'dodge',
               width = 0.7*3/5) +
      scale_x_continuous(labels = scales::percent_format(decimal.mark = ',')) +
      scale_fill_manual(values = c('Hipotético' = verde, 'Real' = azul)) +
      labs(title = 'Trabalho e Preços',
           x = '',
           y = '',
           fill = '') +
      theme_light() +
      theme(legend.position = 'bottom'),
    tooltip = 'text') %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )
})

output$graf32_7 <- renderPlotly({
  plotly::ggplotly(
    producao_micro %>%
      dplyr::filter(name_micro == input$selecao_microrregiao) %>%
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>%
      ggplot() +
      geom_col(aes(x = valor/100,
                   y = variavel,
                   fill = cenario,
                   text = paste0('Variável: ', variavel,
                                 '<br>Valor: ', scales::percent(
                                   valor/100,
                                   decimal.mark = ',',
                                   accuracy = 0.01),
                                 '<br>Cenário: ', cenario)),
               position = 'dodge',
               width = 0.7) +
      scale_x_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                         accuracy = 0.1)) +
      scale_fill_manual(values = c('Hipotético' = verde, 'Real' = azul)) +
      labs(title = 'Produção Setorial',
           x = '',
           y = '',
           fill = '') +
      theme_light() +
      theme(legend.position = 'bottom'),
    tooltip = 'text') %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )
})

output$graf32_8 <- renderPlotly({
  plotly::ggplotly(
    investimento_micro %>%
      dplyr::filter(name_micro == input$selecao_microrregiao) %>%
      dplyr::mutate(variavel = forcats::fct_reorder(variavel, dplyr::desc(ref))) %>%
      ggplot() +
      geom_col(aes(x = valor/100,
                   y = variavel,
                   fill = cenario,
                   text = paste0('Variável: ', variavel,
                                 '<br>Valor: ', scales::percent(
                                   valor/100,
                                   decimal.mark = ',',
                                   accuracy = 0.01),
                                 '<br>Cenário: ', cenario)),
               position = 'dodge',
               width = 0.7) +
      scale_x_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                         accuracy = 0.1)) +
      scale_fill_manual(values = c('Hipotético' = verde, 'Real' = azul)) +
      labs(title = 'Investimento Setorial',
           x = '',
           y = '',
           fill = '') +
      theme_light() +
      theme(legend.position = 'bottom'),
    tooltip = 'text') %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )
})


}


# EXECUÇÃO ----------------------------------------------------------------
shinyApp(
  ui = ui,
  server = server
)





