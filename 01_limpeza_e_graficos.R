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

# 5. CONSTRUÇÃO DO GRÁFICO 1 ---------------------------------------------------
grafico_1 <- ggplot(df_temporal, aes(x = Ano, y = Total_Matriculas, color = Rede)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = df_rotulos, size = 2) + # Ponto no final da linha
  # Adiciona os rótulos no final da linha (Direct Labeling), empurrando um pouco para a direita
  geom_text(data = df_rotulos, aes(label = Rede), 
            hjust = -0.1, size = 4, fontface = "bold") +
  # Formatação dos eixos
  scale_x_continuous(breaks = seq(2016, 2025, by = 1), 
                     limits = c(2016, 2026.5)) + # Espaço extra para caber o texto
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_color_manual(values = c("Estadual" = "#4361EE", "Municipal" = "#F72585", "Privada" = "#4CC9F0")) +
  # Aplicação dos princípios de Tufte
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "Evolução das Matrículas na Educação Básica (PR)",
       subtitle = "Série Histórica 2016 - 2025",
       x = "Ano",
       y = "Total de Matrículas") +
  theme(legend.position = "none", # Removemos a legenda tradicional
        plot.title = element_text(face = "bold", size = 14),
        axis.title.y = element_text(margin = margin(r = 10)))

# Exibir o gráfico no RStudio
print(grafico_1)
