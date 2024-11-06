## Scripts utilizado para responder a Lista 5 - Lucas Ast

library(dplyr)
library(data.table)
library(magrittr)

# Lendo o banco de dados

dados <- fread("Dados/chocolate.csv")
dados

# para vermos o resumo dos dados, podemos usar a função "glimpse()", rows = quantidade de linhas;
# columns = quantidade de colunas; $ = variáveis do banco de dados.

glimpse(dados)

# a.  Quantos países produzem chocolate?

dados %>% 
  summarise(origem_cacau = n_distinct(origem_cacau)) # Summarise = Serve para calcular estatísticas como soma, média, contagem ou outros cálculos em colunas de um data frame.

dados %>% 
  summarise(local_compania = n_distinct(local_compania)) #  n_distinct(): Conta o número de valores únicos em uma coluna ou vetor. É útil para saber quantas entradas distintas existem.

tabela_paises <- dados %>%
  count(local_compania, origem_cacau) %>%                # count(): Conta a frequência de elementos em uma ou mais colunas de um data frame. Retorna um data frame com uma contagem de quantas vezes cada combinação de valores aparece.
  arrange(desc(n))

# b. Quantos chocolates existem com pelo menos 3 ingredientes?

total_chocolates_3 <- dados %>% 
  filter(ingredientes >= 3) %>% 
  summarise(total_chocolates = n())
total_chocolates_3

# c. Quantos chocolates existem com 5 ingredientes?

total_chocolates_5 <- dados %>% 
  filter(ingredientes >= 5) %>% 
  summarise(total_chocolates = n())
total_chocolates_5

# d. Quantos chocolates existem com pelo menos 4 características memoráveis?
library(stringr)
                                                                          # A função mutate() é usada para criar novas colunas ou modificar colunas existentes em um data frame.
chocolate_caract_4 <- dados %>%
  mutate(num_caracteristicas = str_count(caracteristicas, ",") + 1) %>%  # Conta o número de características
  filter(num_caracteristicas >= 4) %>%  # Filtra os chocolates com 4 ou mais características
  summarise(total_chocolates = n())  # Conta o total de chocolates que atendem à condição

chocolate_caract_4

# e.  Quantos chocolates existem com Sal em sua composição?

chocolate_com_sal <- dados %>%
  filter(str_detect(ingredientes, "S")) %>%  # Verifica se "S" está presente na string da coluna ingredientes
  summarise(total_chocolates_sal = n())  # Conta o total de chocolates que atendem à condição

chocolate_com_sal 
  
# f.  Quantos chocolates existem com Baunilha em sua composição?

chocolate_com_baunilha <- dados %>% 
  filter(str_detect(ingredientes, "V")) %>% 
  summarise(total_chocolates_baunilha = n())

chocolate_com_baunilha

# g.  Quantos chocolates existem com Lecitina e Baunilha em sua composição?

chocolate_com_BL <- dados %>% 
  filter(str_detect(ingredientes, "V") & str_detect(ingredientes, "L")) %>%
  summarise(total_chocolate_BL = n())

chocolate_com_BL

