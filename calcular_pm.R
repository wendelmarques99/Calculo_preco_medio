# bibliotecas
library(readxl)
library(magrittr)

# base de dados
operacoes <- readxl::read_xlsx("dados_brutos.xlsx")
# Funcao que calcula o Preco medio das operacoes
calculo_de_pm <- function(x){
  x %>% 
    dplyr::mutate(custo_total = corretagem + custos_extras) %>% 
    dplyr::group_by(data, conta_bolsa) %>% 
    dplyr::mutate(rateio = custo_total/sum(valor_operado)*valor_operado) %>% 
    dplyr::mutate(pm = ifelse(c_v == "c", preco + rateio/quantidade, preco - rateio/quantidade)) %>% 
    dplyr::group_by(ativos, data, conta_bolsa) %>%
    dplyr::summarise(
      pm = round(weighted.mean(pm, quantidade), 4),
      valor_operado = sum(valor_operado),
      quantidade = sum(quantidade), c_v = c_v, corretagem, custos_extras) %>% 
    dplyr::arrange(data) %>% 
    unique()
}

# df com preco_medio
operacoes %>% 
calculo_de_pm() 
