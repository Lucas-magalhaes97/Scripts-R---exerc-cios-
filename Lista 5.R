## Scripts utilizado para responder a Lista 5 - Lucas Ast

library(tidyr)
library(dplyr)
library(data.table)
library(magrittr)

######### Exercício 1 ###########

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

######### Exercício 2 ###########

library(dplyr)
library(data.table)
library(magrittr)
library(stringr)

# lendo o banco de dados

art <- fread("Dados/Art.csv")
art

art_moma <- fread("Dados/Art_Moma.csv")
art_moma

glimpse(art)
glimpse(art_moma)

# a.  Qual a média de exposições realizadas pelo MoMA e pelo Whitney por ano?

media_moma <- art_moma %>% 
  summarise(media_exposicoes_moma = mean(moma_count_to_year, na.rm = TRUE))  # Calcula a média e ignora valores NA

media_moma

media_whitney <- art_moma %>% 
  summarise(media_exposicoes_whitney = mean(whitney_count_to_year, na.rm = TRUE))

media_whitney


# b.  Qual a média de exposições realizadas pelo MoMA e pelo Whitney por ano para artistas de raça não branca?

colnames(art)
colnames(art_moma)

# Com base nas colunas listadas, a coluna de junção comum é artist_unique_id.
art_completo <- art %>%
  inner_join(art_moma, by = "artist_unique_id")

# Exibindo o novo data frame combinado
art_completo

# Filtrando apenas os artistas de raça não branca e calculando a média das exposições
media_exposicoes_nao_brancos <- art_completo %>%
  filter(artist_race_nwi == "Non-White") %>%
  summarise(
    media_exposicoes_moma = mean(moma_count_to_year, na.rm = TRUE),
    media_exposicoes_whitney = mean(whitney_count_to_year, na.rm = TRUE)
  )

media_exposicoes_nao_brancos

# c.  Quais os quatro artistas com mais exposições realizadas pelo MoMA?

# Agrupando por artista e somando o número total de exposições realizadas pelo MoMA
top4_artistas_moma <- art_completo %>%
  group_by(artist_name) %>%
  summarise(total_exposicoes_moma = sum(moma_count_to_year, na.rm = TRUE)) %>%
  arrange(desc(total_exposicoes_moma)) %>%
  head(4)

top4_artistas_moma


# d.  Do total de artistas, quantos são homens e quantos são mulheres?

# Contando o número de homens e mulheres no total de artistas
contagem_genero <- art_completo %>%
  group_by(artist_gender) %>%
  summarise(total_artistas = n()) %>%
  arrange(desc(total_artistas))

contagem_genero

# e.  Do total de artistas, qual as cinco nacionalidades predominante?

# Contando as cinco nacionalidades mais predominantes
top5_nacionalidades <- art_completo %>%
  group_by(artist_nationality) %>%
  summarise(total_artistas = n()) %>%
  arrange(desc(total_artistas)) %>%
  head(5)

top5_nacionalidades

# f.  Dos artistas que expuseram no MoMA, quantos aparecem em cada livro? E dos que expuseram no Whitney?

# Contando o número de artistas que expuseram em cada livro no MoMA
artistas_moma_por_livro <- art_completo %>%
  filter(!is.na(moma_count_to_year) & moma_count_to_year > 0) %>%  # Filtrando artistas que expuseram no MoMA
  group_by(book) %>%
  summarise(artistas_por_livro_moma = n_distinct(artist_name)) %>%
  arrange(desc(artistas_por_livro_moma))

artistas_moma_por_livro

# Contando o número de artistas que expuseram em cada livro no Whitney
artistas_whitney_por_livro <- art_completo %>%
  filter(!is.na(whitney_count_to_year) & whitney_count_to_year > 0) %>%  # Filtrando artistas que expuseram no Whitney
  group_by(book) %>%
  summarise(artistas_por_livro_whitney = n_distinct(artist_name)) %>%
  arrange(desc(artistas_por_livro_whitney))

# Exibindo os resultados
artistas_whitney_por_livro

# g.  Qual a média de espaço ocupado por página de cada artista?

# Calculando a média de espaço ocupado por página para cada artista
media_espaco_por_artista <- art_completo %>%
  group_by(artist_name) %>%
  summarise(media_espaco_por_pagina = mean(space_ratio_per_page_total, na.rm = TRUE))

media_espaco_por_artista

# 3.  Para esse exercício você deverá utilizar os banco de dados `refugiados_pais.csv.gz` e `refugiados.csv.gz`. Considere apenas observações completas.

# lendo os bancos de dados

refugiados_pais <- fread("Dados/refugiados_pais.csv")
refugiados_pais

refugiados <- fread("Dados/refugiados.csv")
refugiados

colnames(refugiados)
colnames(refugiados_pais)

glimpse(refugiados)
glimpse(refugiados_pais)

# a.  Qual a média de refugiados por país?

# Agrupar por id_destino e somar o número de refugiados
refugiados_por_pais <- refugiados %>%
  group_by(id_destino) %>%
  summarise(total_refugiados = sum(refugiados, na.rm = TRUE))

# Unir com o banco de dados de países para obter os nomes dos países
refugiados_com_nome <- refugiados_por_pais %>%
  left_join(refugiados_pais, by = c("id_destino" = "id"))

# Calcular a média de refugiados por país
media_refugiados_por_pais <- refugiados_com_nome %>%
  summarise(media_refugiados = mean(total_refugiados))

# Exibir o resultado
media_refugiados_por_pais

# b.  Quantos refugiados houveram saíndo do Afeganistão em 1990? E a partir de 2000?

# Filtrando os dados para o Afeganistão em 1990
refugiados_afeganistao_1990 <- refugiados %>%
  filter(id_origem == "AFG" & ano == 1990) %>%
  summarise(total_refugiados_1990 = sum(refugiados, na.rm = TRUE))

# Filtrando os dados para o Afeganistão a partir de 2000
refugiados_afeganistao_2000 <- refugiados %>%
  filter(id_origem == "AFG" & ano >= 2000) %>%
  summarise(total_refugiados_2000 = sum(refugiados, na.rm = TRUE))

# Exibindo os resultados
refugiados_afeganistao_1990
refugiados_afeganistao_2000

# c.  Crie a matriz de migração intercontinental (de -\> para) de refugiados do ano 2005.

# Passo 1: Filtrar os dados de refugiados para o ano de 2005
refugiados_2005 <- refugiados %>%
  filter(ano == 2005)

# Passo 2: Obter as regiões/continentes dos países de origem e destino
refugiados_com_regiao <- refugiados_2005 %>%
  left_join(refugiados_pais, by = c("id_origem" = "id")) %>%
  left_join(refugiados_pais, by = c("id_destino" = "id"), suffix = c("_origem", "_destino"))

# Passo 3: Construir a matriz de migração intercontinental
matriz_migracao <- refugiados_com_regiao %>%
  group_by(regiao_origem, regiao_destino) %>%
  summarise(total_refugiados = sum(refugiados, na.rm = TRUE)) %>%
  pivot_wider(names_from = regiao_destino, values_from = total_refugiados, values_fill = list(total_refugiados = 0))

# Exibindo a matriz de migração
matriz_migracao

# d.  Qual o país que mais recebeu refugiados em 2005? E em 2010?

# Filtrar dados para os anos de 2005 e 2010
refugiados_2005_2010 <- refugiados %>%
  filter(ano %in% c(2005, 2010))

# Agrupar por id_destino e ano, e somar o total de refugiados
refugiados_agrupados <- refugiados_2005_2010 %>%
  group_by(ano, id_destino) %>%
  summarise(total_refugiados = sum(refugiados, na.rm = TRUE)) %>%
  arrange(desc(total_refugiados))

# Unir com o data frame de países para obter os nomes
refugiados_com_nome <- refugiados_agrupados %>%
  left_join(refugiados_pais, by = c("id_destino" = "id"))

# Encontrar o país que mais recebeu refugiados em 2005 e em 2010
mais_refugiados_2005 <- refugiados_com_nome %>%
  filter(ano == 2005) %>%
  slice_max(total_refugiados, n = 1)

mais_refugiados_2010 <- refugiados_com_nome %>%
  filter(ano == 2010) %>%
  slice_max(total_refugiados, n = 1)

# Exibir os resultados
mais_refugiados_2005
mais_refugiados_2010

# e.  Quantos refugiados os 3 países que mais receberam refugiados em 2010 receberam em 2005?

# Passo 1: Encontrar os 3 países que mais receberam refugiados em 2010
top3_paises_2010 <- refugiados_com_nome %>%
  filter(ano == 2010) %>%
  arrange(desc(total_refugiados)) %>%
  slice_max(total_refugiados, n = 3)

# Exibir os 3 países de 2010
top3_paises_2010

# Passo 2: Filtrar dados de 2005 para os países identificados
refugiados_2005_top3 <- refugiados_com_nome %>%
  filter(ano == 2005, id_destino %in% top3_paises_2010$id_destino) %>%
  select(nome, total_refugiados)

# Exibir os resultados para 2005
refugiados_2005_top3






