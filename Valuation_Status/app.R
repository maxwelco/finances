library(shiny)
library(tidyverse)

status <- read_csv2("../Valuation_Status/statusinvest-busca-avancada.csv")
setores <- read_csv("../Valuation_Status/b3_setores.csv")

risco <- 16
EPR <- 9.5 # % premio de risco de investir na bolsa
EPR_IPCA <- 15

status1 <- status %>%
  mutate(DPA = (DY/100) * PRECO) %>%
  mutate(valuation_bazin_RS = DPA / 0.06) %>%
  mutate(`desconto_bazin_%` = ((PRECO - valuation_bazin_RS) / valuation_bazin_RS)*100) %>%
  mutate(valuation_grahan_RS = (22.5 * LPA * VPA)^0.5) %>%
  mutate(`desconto_grahan_%` = ((PRECO - valuation_grahan_RS) /
                                  valuation_grahan_RS)*100) %>%
  mutate(valuation_gordon_RS = (DPA * (1 + (`CAGR LUCROS 5 ANOS`/100))) / risco) %>%
  mutate(`desconto_gordon_%` = ((PRECO - valuation_gordon_RS) /
                                  valuation_gordon_RS)*100) %>%
  mutate(PAYOUT = DPA / LPA) %>%
  mutate(crescimento_esperado = (1 - PAYOUT) * ROE) %>%
  mutate(media_crescimento = (`CAGR LUCROS 5 ANOS` + crescimento_esperado)/2) %>%
  mutate_if(is.double, ~round(., 2))

status2 <- status1 %>%
  left_join(setores, by = "TICKER") %>%
  dplyr::select(TICKER, EMPRESA, PRECO, `SETOR ECONÔMICO`, SUBSETOR, SEGMENTO,
                PRECO, valuation_bazin_RS, `desconto_bazin_%`, valuation_grahan_RS,
                `desconto_grahan_%`, valuation_gordon_RS, `desconto_gordon_%`,
                crescimento_esperado, media_crescimento) %>%
  arrange(SEGMENTO)






ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  titlePanel(
    "Valuation Me Poupe"
  ),
  tabsetPanel(
    tabPanel("Descontado",
  fluidRow(
    column(3,
           selectInput("setor", "Setor", choices = status2$`SETOR ECONÔMICO`)),
    column(3,
           selectInput("code", "Ativo", choices = status2$TICKER)),
    column(3,
           selectInput("empresa", "Empresa", choices = status2$EMPRESA)
    )),
  fluidRow(
    column(2, tableOutput("desconto_bazin")),
    column(2, tableOutput("desconto_grahan")),
    column(2, tableOutput("desconto_gordon")),
    column(2, tableOutput("crescimento_esperado")),
    column(2, tableOutput("media_crescimento"))
))))
server <- function(input, output, session) {
  status3 <- reactive(status2 %>%
                        filter(TICKER == input$code))

  output$desconto_bazin <- renderTable(
    status3() %>% dplyr::select(TICKER, `desconto_bazin_%`)
  )
  output$desconto_grahan <- renderTable(
    status3() %>% dplyr::select(TICKER, `desconto_grahan_%`)
  )
  output$desconto_gordon <- renderTable(
    status3() %>% dplyr::select(TICKER, `desconto_gordon_%`)
  )
  output$crescimento_esperado <- renderTable(
    status3() %>% dplyr::select(TICKER, crescimento_esperado)
  )
  output$media_crescimento <- renderTable(
    status3() %>% dplyr::select(TICKER, media_crescimento)
  )
}
shinyApp(ui, server)
