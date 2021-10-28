library(tidyverse)


# crie um código que seja capaz de:
# elaborar um banco de dados com nomes, idades, profissão, estado civil e renda;
# seja capaz de calcular os valores médios de idade e renda;
# seja capaz de filtrar dentre os dados, apenas as informações de indivíduos com determinado estado civil ou renda a partir de certo valor


# banco de dados --------

nome <- c("Maria",
          "José",
          "Antônio",
          "João",
          "Francisco",
          "Ana",
          "Luiz",
          "Paulo",
          "Carlos",
          "Manoel",
          "Pedro",
          "Francisca",
          "Marcos",
          "Raimundo",
          "Sebastião",
          "Antônia",
          "Marcelo",
          "Jorge",
          "Márcia",
          "Geraldo",
          "Adriana",
          "Sandra",
          "Luis",
          "Fernando",
          "Fabio",
          "Roberto",
          "Márcio",
          "Edson",
          "André",
          "Sérgio",
          "Josefa",
          "Patrícia",
          "Daniel",
          "Rodrigo",
          "Rafael",
          "Joaquim",
          "Vera",
          "Ricardo",
          "Eduardo",
          "Terezinha",
          "Sônia",
          "Alexandre",
          "Rita",
          "Luciana",
          "Cláudio",
          "Rosa",
          "Benedito",
          "Leandro",
          "Raimunda",
          "Mário")


profissao <- c("Ajudante de pintor",
               "Administrador no comércio de mercadorias",
               "Auxiliar de lavanderia",
               "Construtor civil",
               "Ajudante",
               "Vendedor em comércio atacadista",
               "Cozinheiro de restaurante",
               "Administrador",
               "Farmacêutico",
               "Assistente de vendas",
               "Caixa de banco",
               "Operador de negócios",
               "Supervisor de segurança do trabalho",
               "Encarregado de padaria",
               "Instalador de máquinas",
               "Montador de acessórios",
               "Auxiliar de marceneiro",
               "Eletricista instalador de alta e baixa tensão",
               "Gerente administrativo",
               "Almoxarife",
               "Carpinteiro (obras)",
               "Arrumadeira de hotel",
               "Técnico de laboratório de análises clínicas",
               "Assistente de serviço de contabilidade",
               "Condutor de escavadeira",
               "Ajudante de churrasqueiro",
               "Atendente de posto de gasolina",
               "Encarregado de garagem",
               "Ajudante de serralheiro",
               "Auxiliar de manutenção elétrica e hidráulica",
               "Fisioterapeuta",
               "Professor das séries iniciais",
               "Colhedor de uva",
               "Borracheiro",
               "Auxiliar de limpeza",
               "Atendente central telemarketing",
               "Gesseiro",
               "Encarregado de supermercado",
               "Promotor de vendas",
               "Atendente de bar",
               "Condutor de veículo de carga",
               "Auxiliar de técnico de controle de qualidade",
               "Assistente de engenharia (construção civil)",
               "Técnico de enfermagem",
               "Analista de controle de qualidade",
               "Consultor de vendas",
               "Montador de andaimes (edificações)",
               "Educador infantil de nível médio",
               "Motorista de ônibus rodoviário",
               "Atendente de creche")


estado_civil <- rep(c("Solteiro", "Casado", "Divorciado", "Viúvo", "Separado"), 10)

# cria 50 idades entre 18 e 100 anos
idade <- runif(50, min = 18, max = 100) %>% 
  floor()

# cria 50 rendas entre 500 e 50 mil reais
renda <- runif(50, min = 500, max = 50000) %>% 
  round(2)


bd <- tibble(nome, idade, profissao, renda, estado_civil)



# valores médios de idade e renda -----------------------------------------


bd %>% 
  summarise(media_idade = mean(idade),
            media_renda = mean(renda))



# filtrar estado civil e renda --------------------------------------------


filtrar_renda <- function(renda_minima = 500,
                          renda_maxima = 50000) {
  
  res <- bd %>% 
    filter(renda > renda_minima & renda < renda_maxima)
  
  res
  
}

# exemplos
filtrar_renda(renda_minima = 40000)
filtrar_renda(renda_minima = 10000,
              renda_maxima = 30000)


filtrar_estado_civil <- function(sit_civil = NULL) {
  
  sit_civil <- str_to_title(sit_civil)
  
  res <- bd %>% 
    filter(estado_civil == sit_civil)
  
  res
  
}

# exemplos
filtrar_estado_civil(sit_civil = "solteiro")
filtrar_estado_civil(sit_civil = "viúvo")
