library(shiny)
library(bslib)
library(plotly)

# INTERFACE ---------------------------------------------------------------
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
      title = "Sobre o projeto",
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Projeto",
          
          p(
            "Informações gerais sobre o projeto, objetivos, metodologia e",
            "contextualização da geração distribuída em Minas Gerais."
          )
        ),
        
        card(
          full_screen = TRUE,
          
          card_header(
            "Impactos econômicos da geração distribuída em Minas Gerais"
          ),
          
          card_body(
            h4("Sobre a aplicação"),
            
            p(
              "Esta aplicação apresenta os impactos econômicos acumulados",
              "da geração distribuída em Minas Gerais entre 2015 e 2025."
            ),
            
            p(
              "Os resultados são apresentados para dois cenários:"
            ),
            
            tags$ul(
              tags$li(
                strong("Cenário real: "),
                "considera as alterações regulatórias ocorridas no período."
              ),
              
              tags$li(
                strong("Cenário hipotético: "),
                "considera a ausência das alterações introduzidas pela",
                "Lei nº 14.300."
              )
            )
          )
        )
      )
    ),
    
    nav_panel(
      title = "Pesquisadores",
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Equipe",
          
          p(
            "Conheça os pesquisadores e as instituições envolvidas",
            "no desenvolvimento do projeto."
          )
        ),
        
        layout_column_wrap(
          width = 1 / 3,
          
          card(
            card_header("Pesquisador 1"),
            card_body(
              h5("Nome do pesquisador"),
              p("Instituição"),
              p("Área de atuação")
            )
          ),
          
          card(
            card_header("Pesquisador 2"),
            card_body(
              h5("Nome do pesquisador"),
              p("Instituição"),
              p("Área de atuação")
            )
          ),
          
          card(
            card_header("Pesquisador 3"),
            card_body(
              h5("Nome do pesquisador"),
              p("Instituição"),
              p("Área de atuação")
            )
          )
        )
      )
    ),
    
    nav_panel(
      title = "Licença de uso",
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Licença",
          
          p(
            "Informações sobre a utilização, reprodução e citação",
            "dos resultados apresentados."
          )
        ),
        
        card(
          card_header("Condições de utilização"),
          
          card_body(
            p(
              "Insira nesta seção a licença adotada, a forma correta",
              "de citação da pesquisa e as condições de uso dos dados."
            )
          )
        )
      )
    )
  ),
  
  # ANÁLISE FINANCEIRA ----------------------------------------------------
  
  nav_menu(
    title = "Análise financeira",
    
    nav_panel(
      title = "Mapas financeiros",
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Filtros",
          
          selectInput(
            inputId = "cenario_financeiro",
            label = "Cenário",
            choices = c(
              "Cenário real",
              "Cenário hipotético",
              "Comparação"
            )
          ),
          
          selectInput(
            inputId = "indicador_financeiro",
            label = "Indicador financeiro",
            choices = c(
              "Valor presente líquido",
              "Taxa interna de retorno",
              "Payback",
              "Custo nivelado de energia"
            )
          )
        ),
        
        card(
          full_screen = TRUE,
          
          card_header("Distribuição espacial dos resultados financeiros"),
          
          card_body(
            plotOutput(
              outputId = "mapa_financeiro",
              height = "650px"
            )
          )
        )
      )
    ),
    
    nav_panel(
      title = "Calculadora financeira",
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Parâmetros",
          
          numericInput(
            inputId = "investimento_inicial",
            label = "Investimento inicial",
            value = 100000,
            min = 0
          ),
          
          numericInput(
            inputId = "economia_anual",
            label = "Economia anual estimada",
            value = 15000,
            min = 0
          ),
          
          numericInput(
            inputId = "taxa_desconto",
            label = "Taxa de desconto (%)",
            value = 10,
            min = 0
          ),
          
          numericInput(
            inputId = "vida_util",
            label = "Vida útil do projeto",
            value = 25,
            min = 1
          ),
          
          actionButton(
            inputId = "calcular",
            label = "Calcular",
            class = "btn-primary"
          )
        ),
        
        layout_column_wrap(
          width = 1 / 3,
          
          value_box(
            title = "Valor presente líquido",
            value = textOutput("valor_vpl")
          ),
          
          value_box(
            title = "Taxa interna de retorno",
            value = textOutput("valor_tir")
          ),
          
          value_box(
            title = "Payback",
            value = textOutput("valor_payback")
          )
        ),
        
        card(
          full_screen = TRUE,
          
          card_header("Fluxo de caixa do projeto"),
          
          card_body(
            plotOutput(
              outputId = "grafico_fluxo_caixa",
              height = "500px"
            )
          )
        )
      )
    )
  ),
  
  # RESULTADOS ECONÔMICOS -------------------------------------------------
  
  nav_menu(
    title = "Resultados econômicos",

#####################################################
nav_panel(
  title = "Minas Gerais",
  

    
    card(
      
      card_header("Impacto Real"),
      
      card_body(
        
        h5("Atividade Econômica"),
        
        plotlyOutput("graf1", height = 280),
        
        tags$hr(),
        
        h5("Mercado de Trabalho"),
        
        plotlyOutput("graf2", height = 220)
        
      )
      
    )
),


######################################################
    
    nav_panel(
      title = "Microrregiões",
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Filtros",
          
          selectInput(
            inputId = "cenario_microrregiao",
            label = "Cenário",
            choices = c(
              "Cenário real",
              "Cenário hipotético",
              "Diferença entre cenários"
            )
          ),
          
          selectInput(
            inputId = "indicador_microrregiao",
            label = "Indicador",
            choices = c(
              "Consumo das Famílias",
              "Investimento Real",
              "Consumo do Governo",
              "Volume de Exportações",
              "Volume de Importação",
              "PIB Real",
              "Emprego Agregado",
              "Salário Real Médio",
              "Estoque de Capital Médio",
              "Deflator do PIB",
              "Índice de Preços"
            )
          ),
          
          selectInput(
            inputId = "microrregiao_selecionada",
            label = "Microrregião",
            choices = NULL
          )
        ),
        
        layout_columns(
          col_widths = c(8, 4),
          
          card(
            full_screen = TRUE,
            
            card_header("Distribuição espacial dos impactos"),
            
            card_body(
              plotOutput(
                outputId = "mapa_microrregioes",
                height = "600px"
              )
            )
          ),
          
          card(
            card_header("Microrregião selecionada"),
            
            card_body(
              uiOutput("resumo_microrregiao")
            )
          )
        ),
        
        card(
          full_screen = TRUE,
          
          card_header("Impactos por microrregião"),
          
          card_body(
            plotOutput(
              outputId = "grafico_microrregioes",
              height = "650px"
            )
          )
        )
      )
    ),
    
    nav_panel(
      title = "Rankings regionais",
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Filtros",
          
          selectInput(
            inputId = "variavel_ranking",
            label = "Indicador",
            choices = c(
              "PIB Real",
              "Consumo das Famílias"
            )
          ),
          
          selectInput(
            inputId = "cenario_ranking",
            label = "Cenário",
            choices = c(
              "Cenário real",
              "Cenário hipotético",
              "Comparação entre cenários"
            )
          ),
          
          sliderInput(
            inputId = "quantidade_microrregioes",
            label = "Quantidade de microrregiões",
            min = 5,
            max = 66,
            value = 15,
            step = 1
          ),
          
          radioButtons(
            inputId = "ordenacao_ranking",
            label = "Ordenação",
            choices = c(
              "Maiores impactos",
              "Menores impactos"
            )
          )
        ),
        
        layout_column_wrap(
          width = 1 / 3,
          
          value_box(
            title = "Maior impacto",
            value = textOutput("maior_impacto_ranking")
          ),
          
          value_box(
            title = "Impacto mediano",
            value = textOutput("mediana_impacto_ranking")
          ),
          
          value_box(
            title = "Menor impacto",
            value = textOutput("menor_impacto_ranking")
          )
        ),
        
        layout_columns(
          col_widths = c(7, 5),
          
          card(
            full_screen = TRUE,
            
            card_header("Ranking das microrregiões"),
            
            card_body(
              plotOutput(
                outputId = "grafico_ranking",
                height = "650px"
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            
            card_header("PIB real e consumo das famílias"),
            
            card_body(
              plotOutput(
                outputId = "grafico_pib_consumo",
                height = "650px"
              )
            )
          )
        )
      )
    ),
    
    nav_panel(
      title = "Setores econômicos",
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Filtros",
          
          radioButtons(
            inputId = "tipo_impacto_setorial",
            label = "Tipo de impacto",
            choices = c(
              "Produção industrial",
              "Investimento industrial"
            )
          ),
          
          selectInput(
            inputId = "cenario_setorial",
            label = "Cenário",
            choices = c(
              "Cenário real",
              "Cenário hipotético",
              "Diferença entre cenários"
            )
          ),
          
          selectInput(
            inputId = "setor_economico",
            label = "Setor econômico",
            choices = NULL
          ),
          
          selectInput(
            inputId = "microrregiao_setorial",
            label = "Microrregião",
            choices = NULL
          )
        ),
        
        layout_columns(
          col_widths = c(7, 5),
          
          card(
            full_screen = TRUE,
            
            card_header("Distribuição espacial do impacto setorial"),
            
            card_body(
              plotOutput(
                outputId = "mapa_setorial",
                height = "600px"
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            
            card_header("Resultados por setor econômico"),
            
            card_body(
              plotOutput(
                outputId = "grafico_setorial",
                height = "600px"
              )
            )
          )
        ),
        
        card(
          full_screen = TRUE,
          
          card_header("Setores econômicos e microrregiões"),
          
          card_body(
            plotOutput(
              outputId = "mapa_calor_setorial",
              height = "700px"
            )
          )
        )
      )
    ),
    
    nav_panel(
      title = "Valores monetários",
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Filtros",
          
          selectInput(
            inputId = "cenario_monetario",
            label = "Cenário",
            choices = c(
              "Cenário real",
              "Cenário hipotético",
              "Diferença entre cenários"
            )
          ),
          
          selectInput(
            inputId = "componente_monetario",
            label = "Componente",
            choices = c(
              "Impacto total",
              "Consumo das famílias",
              "Investimento",
              "Consumo do governo",
              "Estoques",
              "Exportações"
            )
          ),
          
          selectInput(
            inputId = "microrregiao_monetaria",
            label = "Microrregião",
            choices = NULL
          )
        ),
        
        layout_column_wrap(
          width = 1 / 3,
          
          value_box(
            title = "Impacto total",
            value = textOutput("impacto_total_monetario"),
            showcase = tags$span("R$ milhões")
          ),
          
          value_box(
            title = "Maior impacto regional",
            value = textOutput("maior_impacto_monetario")
          ),
          
          value_box(
            title = "Participação regional",
            value = textOutput("participacao_regional_monetaria")
          )
        ),
        
        layout_columns(
          col_widths = c(7, 5),
          
          card(
            full_screen = TRUE,
            
            card_header("Impacto monetário por microrregião"),
            
            card_body(
              plotOutput(
                outputId = "mapa_monetario",
                height = "600px"
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            
            card_header("Composição do impacto monetário"),
            
            card_body(
              plotOutput(
                outputId = "grafico_composicao_monetaria",
                height = "600px"
              )
            )
          )
        ),
        
        card(
          full_screen = TRUE,
          
          card_header(
            "Impactos acumulados entre 2015 e 2025 — milhões de reais"
          ),
          
          card_body(
            plotOutput(
              outputId = "grafico_monetario_microrregioes",
              height = "650px"
            )
          )
        )
      )
    )
  )
)


# SERVIDOR ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Os objetos output serão construídos posteriormente,
  # após a importação e o tratamento das planilhas.
  
}


# EXECUÇÃO ----------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)