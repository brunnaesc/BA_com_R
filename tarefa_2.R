library(tidyverse)

# Crie uma função para computar o VPL. Note que ele é dependente do vetor de fluxos de caixa e da taxa de desconto. 
# Não esqueça que saídas de capital são fluxos com sinal negativo.

get_VPL <- function(fluxo_caixa,
                    tx_desconto){
  
  t <- length(fluxo_caixa) - 1
  
  fc_desc <- rep(0, length(fluxo_caixa))
  
  
  for (i in seq_along(fluxo_caixa)) {
    
    fc_desc[i] <- fluxo_caixa[i] / (1 + tx_desconto) ^ i - 1
    
  }

  sum(fc_desc)
  
}

# Aplique sua funçaão para uma proposta com custo inicial de R$200, 
# e os seguintes fluxos dos períodos 1 ao 4: R$50, R$60, R$70 e R$200.
# Considere taxas de desconto de 1% até 50%.

fc <- c(-200, 50, 60, 70, 200)

txs <- seq(from = 0.01,
           to = 0.5,
           by = 0.01)

ex <- map(txs, ~get_VPL(fluxo_caixa = fc, tx_desconto = .x)) %>% 
  unlist()

vpl_ex <- tibble(txs, ex) %>% 
  rename(taxa = txs,
         vpl = ex)


# Por fim, faça um gráfico relacionando a taxa de desconto (eixo x) 
# com o VPL resultante (eixo y).

vpl_ex %>% 
  ggplot() +
  geom_line(aes(x = taxa, y = vpl, group = 1), color = "#5F9EA0") + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$")) +
  labs(title = "Relação entre taxa de desconto e VPL",
       caption = "Fluxo de caixa com investimento incial de R$200 e fluxos de R$50, R$60, R$70 e R$200.") +
  ggthemes::theme_fivethirtyeight()

  
