library(shiny)
library(readr)
library(DT)
library(quarto)
library(shinyjs)

# Definir as colunas e carregar os dados
colunas <- c('Processo', 'Classe', 'Recorrente', 'Recorrido', 'Autuação', 'Relator')
dados <- read_csv('partes.csv', col_names = colunas, col_types = 'c')

# Interface do usuário
ui <- fluidPage(
          useShinyjs(),  # Adicionar shinyjs
          titlePanel('Geração de minutas'),
          
          uiOutput("dynamicUI"),
          
          # Modal de carregamento
          tags$div(id = "loadingModal", class = "modal", tabindex = "-1", role = "dialog",
                   tags$div(class = "modal-dialog", role = "document",
                            tags$div(class = "modal-content",
                                     tags$div(class = "modal-header",
                                              tags$h5(class = "modal-title", "Gerando Minuta"),
                                              tags$button(type = "button", class = "close", `data-dismiss` = "modal", `aria-label` = "Close",
                                                          tags$span(`aria-hidden` = "true", "&times;")
                                              )
                                     ),
                                     tags$div(class = "modal-body",
                                              tags$p("Por favor, aguarde enquanto a minuta está sendo gerada...")
                                     )
                            )
                   )
          )
)

# Lógica do servidor
server <- function(input, output, session) {
          # Variável reativa para armazenar se a minuta foi gerada
          minutaGerada <- reactiveVal(FALSE)
          processoSelecionado <- reactiveVal(FALSE)
          
          # Renderizar a tabela de processos
          output$tabelaProcessos <- DT::renderDataTable({
                    datatable(dados, selection = 'single')
          })
          
          # Observar a seleção de uma linha na tabela e mudar para a aba de detalhes
          observeEvent(input$tabelaProcessos_rows_selected, {
                    processoSelecionado(TRUE)
          })
          
          # Renderizar o texto de instrução ou os detalhes do processo selecionado
          output$instrucaoSelecao <- renderText({
                    if (is.null(input$tabelaProcessos_rows_selected)) {
                              "Por favor, selecione um processo na primeira aba."
                    } else {
                              ""
                    }
          })
          
          output$detalhesProcesso <- renderTable({
                    req(input$tabelaProcessos_rows_selected)
                    dados[input$tabelaProcessos_rows_selected, ]
          })
          
          # Renderizar os botões de ação
          output$botoesAcoes <- renderUI({
                    if (!is.null(input$tabelaProcessos_rows_selected)) {
                              tagList(
                                        actionButton("gerarMinuta", "Gerar Minuta de Despacho"),
                                        actionButton("selecionarOutroProcesso", "Selecionar Outro Processo")
                              )
                    }
          })
          
          # Lógica para gerar a minuta de despacho
          observeEvent(input$gerarMinuta, {
                    # Mostrar o modal de carregamento
                    showModal(modalDialog(
                              title = "Gerando Minuta",
                              "Por favor, aguarde enquanto a minuta está sendo gerada...",
                              easyClose = FALSE,
                              footer = NULL
                    ))
                    
                    # Executar o comando quarto::quarto_render
                    parametros <- list(numero = dados$Processo[input$tabelaProcessos_rows_selected])
                    quarto::quarto_render(input = 'despacho.qmd', output_format = 'html', execute_params = parametros)
                    quarto::quarto_render(input = 'despacho.qmd', output_format = 'docx', execute_params = parametros)
                    
                    
                    # Fechar o modal de carregamento
                    removeModal()
                    
                    # Atualizar a variável reativa para indicar que a minuta foi gerada
                    minutaGerada(TRUE)
          })
          
          # Renderizar o conteúdo da minuta
          output$conteudoMinuta <- renderUI({
                    if (minutaGerada()) {
                              # Ler o conteúdo do arquivo HTML
                              conteudo <- readLines("despacho.html")
                              
                              # Exibir o conteúdo HTML
                              HTML(paste(conteudo, collapse = "\n"))
                    } else {
                              HTML("<p>Por favor, gere uma minuta na aba 'Detalhes do Processo'.</p>")
                    }
          })
          
          # Renderizar o botão para baixar a minuta de despacho na aba "Minuta de Despacho"
          output$botaoBaixarMinuta <- renderUI({
                    if (minutaGerada()) {
                              downloadButton("baixarMinuta", "Baixar Minuta")
                    }
          })
          
          # Renderizar o botão para selecionar outro processo na aba "Minuta de Despacho"
          output$botaoSelecionarOutroProcesso <- renderUI({
                    actionButton("selecionarOutroProcesso", "Selecionar Outro Processo")
          })
          
          # Lógica para baixar a minuta de despacho
          output$baixarMinuta <- downloadHandler(
                    filename = function() {
                              "despacho.docx"
                    },
                    content = function(file) {
                              file.copy("despacho.docx", file)
                    }
          )
          
          # Lógica para selecionar outro processo
          observeEvent(input$selecionarOutroProcesso, {
                    session$reload()
          })
          
          # Renderizar a interface dinâmica
          output$dynamicUI <- renderUI({
                    if (!processoSelecionado()) {
                              fluidPage(
                                        DT::dataTableOutput('tabelaProcessos')
                              )
                    } else if (!minutaGerada()) {
                              fluidPage(
                                        textOutput('instrucaoSelecao'),
                                        tableOutput('detalhesProcesso'),
                                        uiOutput('botoesAcoes')
                              )
                    } else {
                              fluidPage(
                                        uiOutput('conteudoMinuta'),
                                        uiOutput('botaoBaixarMinuta'),
                                        uiOutput('botaoSelecionarOutroProcesso')
                              )
                    }
          })
}

# Executar o aplicativo
shinyApp(ui = ui, server = server)