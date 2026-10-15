# ==============================================================================
# PROJETO: O Funil da Educação Básica no Paraná (AV1)
# OBJETIVO: Análise visual de matrículas aplicando princípios de Tufte
# ==============================================================================

# 1. CARREGAMENTO DOS PACOTES --------------------------------------------------
# Se você ainda não tiver algum desses pacotes, retire o '#' da linha abaixo e rode uma vez:
# install.packages(c("readxl", "dplyr", "ggplot2", "ggthemes", "patchwork"))

library(readxl)    # Para ler o arquivo Excel
library(dplyr)     # Para manipulação e limpeza de dados
library(ggplot2)   # Para a criação dos gráficos
library(ggthemes)  # Para aplicar o theme_tufte()
library(patchwork) # Para organizar os gráficos na mesma folha no final
library(tidyr)

# 2. IMPORTAÇÃO DOS DADOS ------------------------------------------------------
# A base analítica principal com as ~16 mil observações está na aba "Plan1"
caminho_arquivo <- "Matriculas_faixa_etaria_rede_municipio_consultas.xlsx"

# Lendo os dados (isso pode levar alguns segundos dependendo do computador)
df_bruto <- read_excel(caminho_arquivo, sheet = "Plan1")

# 3. VERIFICAÇÃO INICIAL -------------------------------------------------------
# Vamos dar uma olhada na estrutura dos dados que acabamos de importar
glimpse(df_bruto)

# 4. PREPARAÇÃO DOS DADOS: GRÁFICO 1 -------------------------------------------
# Agrupar os dados por Ano e Rede para ter o total do Estado
df_temporal <- df_bruto %>%
  # A rede Federal é muito pequena no contexto geral (geralmente institutos federais), 
  # então vamos focar nas três principais para limpar o gráfico.
  filter(Rede %in% c("Estadual", "Municipal", "Privada")) %>%
  group_by(Ano, Rede) %>%
  summarise(Total_Matriculas = sum(`Total Educação Básica`, na.rm = TRUE), .groups = 'drop')

# Criar um dataframe auxiliar apenas com o último ano (2025) para os rótulos diretos
df_rotulos <- df_temporal %>%
  filter(Ano == 2025)

# 5. CONSTRUÇÃO DO GRÁFICO 1 (Atualizado com eixo Y ancorado e quebras definidas)
grafico_1 <- ggplot(df_temporal, aes(x = Ano, y = Total_Matriculas, color = Rede)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = df_rotulos, size = 2) + 
  # Adiciona os rótulos no final da linha (Direct Labeling)
  geom_text(data = df_rotulos, aes(label = Rede), 
            hjust = -0.1, size = 4, fontface = "bold") +
  # Formatação do eixo X
  scale_x_continuous(breaks = seq(2016, 2025, by = 1), 
                     limits = c(2016, 2026.5)) + 
  # Forçamos o limite de 0 a 1.2M com quebras fixas de 300 mil
  scale_y_continuous(limits = c(0, 1200000),
                     breaks = seq(0, 1200000, by = 300000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_color_manual(values = c("Estadual" = "#4361EE", "Municipal" = "#F72585", "Privada" = "#4CC9F0")) +
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "Evolução das Matrículas na Educação Básica (PR)",
       subtitle = "Série Histórica 2016 - 2025",
       x = "Ano",
       y = "Total de Matrículas") +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 14),
        axis.title.y = element_text(margin = margin(r = 10)),
        # Adiciona linhas de grade horizontais muito finas e claras (estilo Tufte)
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3))

print(grafico_1)

# 6. PREPARAÇÃO DOS DADOS: GRÁFICO 2 (Slopegraph) ------------------------------
df_etapas <- df_bruto %>%
  filter(Ano == 2025, Rede %in% c("Estadual", "Municipal", "Privada")) %>%
  group_by(Rede) %>%
  summarise(
    `Anos Iniciais` = sum(`Matrículas Anos Iniciais`, na.rm = TRUE),
    `Anos Finais` = sum(`Matrículas Anos Finais`, na.rm = TRUE),
    `Ensino Médio` = sum(`Matrículas Ensino Médio`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Transforma as colunas de etapas em linhas para o ggplot conseguir desenhar
  pivot_longer(cols = c(`Anos Iniciais`, `Anos Finais`, `Ensino Médio`), 
               names_to = "Etapa", values_to = "Matriculas") %>%
  # Força a ordem cronológica no eixo X (se não o R coloca em ordem alfabética)
  mutate(Etapa = factor(Etapa, levels = c("Anos Iniciais", "Anos Finais", "Ensino Médio")))

# Filtra apenas o início e o fim para posicionar os rótulos diretos (estilo Tufte)
df_etapas_rotulos_inicio <- df_etapas %>% filter(Etapa == "Anos Iniciais")
df_etapas_rotulos_fim <- df_etapas %>% filter(Etapa == "Ensino Médio")

# 7. CONSTRUÇÃO DO GRÁFICO 2 ---------------------------------------------------
grafico_2 <- ggplot(df_etapas, aes(x = Etapa, y = Matriculas, group = Rede, color = Rede)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  # Nome da rede no início da linha (esquerda)
  geom_text(data = df_etapas_rotulos_inicio, aes(label = Rede), 
            hjust = 1.2, size = 4, fontface = "bold") +
  # Valor numérico no final da linha (direita)
  geom_text(data = df_etapas_rotulos_fim, aes(label = format(Matriculas, big.mark = ".")), 
            hjust = -0.2, size = 4) +
  scale_color_manual(values = c("Estadual" = "#4361EE", "Municipal" = "#F72585", "Privada" = "#4CC9F0")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "A Troca de Bastão Institucional (2025)",
       subtitle = "Transição de alunos entre as etapas da Educação Básica no PR",
       x = "",
       y = "Total de Matrículas") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        # Tufte remove o eixo Y quando os rótulos diretos já dizem o suficiente, 
        # mas aqui manteremos para facilitar a leitura da escala central
        axis.line = element_line(color = "black", linewidth = 0.3))

print(grafico_2)