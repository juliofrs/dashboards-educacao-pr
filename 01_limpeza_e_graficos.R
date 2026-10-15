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
  geom_text(data = df_rotulos, aes(label = Rede), hjust = -0.1, size = 4, fontface = "bold") +
  scale_x_continuous(breaks = seq(2016, 2025, by = 1), limits = c(2016, 2026.5)) + 
  scale_y_continuous(limits = c(0, 1200000), breaks = seq(0, 1200000, by = 300000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_color_manual(values = c("Estadual" = "#4361EE", "Municipal" = "#F72585", "Privada" = "#4CC9F0")) +
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "Evolução Histórica Geral", subtitle = NULL, x = "", y = "") + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3))

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
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  geom_text(data = df_etapas_rotulos_inicio, aes(label = Rede), hjust = 1.2, size = 4, fontface = "bold") +
  geom_text(data = df_etapas_rotulos_fim, aes(label = format(Matriculas, big.mark = ".")), hjust = -0.2, size = 4) +
  scale_color_manual(values = c("Estadual" = "#4361EE", "Municipal" = "#F72585", "Privada" = "#4CC9F0")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "A Transição de Etapas", subtitle = NULL, x = "", y = "") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.line = element_line(color = "black", linewidth = 0.3))

# 8. PREPARAÇÃO DOS DADOS: GRÁFICO 3 (A Curva da Vida Escolar) -----------------
# (A preparação dos dados continua a mesma do passo anterior)
df_idade <- df_bruto %>%
  filter(Ano == 2025, Rede %in% c("Estadual", "Municipal", "Privada")) %>%
  group_by(Rede) %>%
  summarise(
    `0 a 3 anos` = sum(`Matrículas 0 a 3 anos`, na.rm = TRUE),
    `4 a 5 anos` = sum(`Matrículas 4 a 5 anos`, na.rm = TRUE),
    `6 a 10 anos` = sum(`Matrículas 6 a 10 anos`, na.rm = TRUE),
    `11 a 14 anos` = sum(`Matrículas 11 a 14 anos`, na.rm = TRUE),
    `15 a 17 anos` = sum(`Matrículas 15 a 17 anos`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = -Rede, names_to = "Faixa_Etaria", values_to = "Matriculas") %>%
  mutate(Faixa_Etaria = factor(Faixa_Etaria, 
                               levels = c("0 a 3 anos", "4 a 5 anos", "6 a 10 anos", 
                                          "11 a 14 anos", "15 a 17 anos")))

# Criamos um dataframe apenas com o último ponto ("15 a 17 anos") para ancorar o texto
df_idade_rotulos <- df_idade %>% filter(Faixa_Etaria == "15 a 17 anos")

# 9. CONSTRUÇÃO DO NOVO GRÁFICO 3 (Minimalista) --------------------------------

grafico_3 <- ggplot(df_idade, aes(x = Faixa_Etaria, y = Matriculas, group = Rede, color = Rede)) +
  geom_line(linewidth = 1.5) + geom_point(size = 3) +
  geom_text(data = df_idade_rotulos, aes(label = Rede), hjust = -0.1, size = 4.5, fontface = "bold") +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.15))) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), limits = c(0, NA)) +
  scale_color_manual(values = c("Estadual" = "#4361EE", "Municipal" = "#F72585", "Privada" = "#4CC9F0")) +
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "O Ciclo por Idade", subtitle = NULL, x = "", y = "") +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 11, face = "bold"),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3))

# 10. PREPARAÇÃO DOS DADOS: GRÁFICO 4 (Small Multiples) ------------------------
df_vocacao <- df_bruto %>%
  filter(Ano == 2025, Rede %in% c("Estadual", "Municipal", "Privada")) %>%
  group_by(Rede) %>%
  summarise(
    Creche = sum(`Matrículas Creche`, na.rm = TRUE),
    `Pré-escola` = sum(`Matrículas Pré-escola`, na.rm = TRUE),
    `Anos Iniciais` = sum(`Matrículas Anos Iniciais`, na.rm = TRUE),
    `Anos Finais` = sum(`Matrículas Anos Finais`, na.rm = TRUE),
    `Ensino Médio` = sum(`Matrículas Ensino Médio`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = -Rede, names_to = "Etapa", values_to = "Matriculas") %>%
  # Limpa os nomes das etapas para não poluir o eixo X
  mutate(Etapa = factor(Etapa, 
                        levels = c("Creche", "Pré-escola", "Anos Iniciais", 
                                   "Anos Finais", "Ensino Médio"),
                        labels = c("Creche", "Pré", "Fund. I", "Fund. II", "Médio")))

# 11. CONSTRUÇÃO DO GRÁFICO 4 --------------------------------------------------

grafico_4 <- ggplot(df_vocacao, aes(x = Etapa, y = Matriculas, fill = Rede)) +
  geom_col(width = 0.7) + facet_wrap(~ Rede, ncol = 3) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), limits = c(0, NA)) +
  scale_fill_manual(values = c("Estadual" = "#4361EE", "Municipal" = "#F72585", "Privada" = "#4CC9F0")) +
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "Vocação das Redes", subtitle = NULL, x = "", y = "") + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
        axis.line.x = element_line(color = "black", linewidth = 0.3),
        strip.text = element_text(face = "bold", size = 12, margin = margin(b = 10)))

# 12. MONTAGEM FINAL DA FOLHA A4 (Patchwork - Layout Horizontal) ---------------
# Layout 2x2: Gráficos 1 e 3 em cima, Gráficos 2 e 4 em baixo
painel_final_horizontal <- (grafico_1 | grafico_3) / 
  (grafico_2 | grafico_4) +
  plot_annotation(
    title = "O Funil da Educação Básica no Paraná (2016-2025)",
    subtitle = "Uma análise visual das matrículas e da transição entre redes de ensino",
    caption = "Fonte dos dados: Bases do Governo (Adaptado) | Criação: Julio Freitas",
    theme = theme(plot.title = element_text(size = 18, face = "bold", family = "serif", hjust = 0.5),
                  plot.subtitle = element_text(size = 14, family = "serif", hjust = 0.5),
                  plot.caption = element_text(size = 10, family = "serif", color = "gray50"))
  )

# 13. EXPORTAÇÃO PARA IMPRESSÃO (Tamanho A4 Horizontal/Paisagem) ---------------
# O formato A4 horizontal tem 29.7 cm de largura por 21 cm de altura
ggsave("Painel_Final_A4_Horizontal.pdf", plot = painel_final_horizontal, 
       width = 29.7, height = 21, units = "cm", 
       device = cairo_pdf) # Garante fontes perfeitamente nítidas no PDF

ggsave("Painel_Final_A4_Horizontal.png", plot = painel_final_horizontal, 
       width = 29.7, height = 21, units = "cm", dpi = 300, bg = "white")

# Exibe na aba Plots
print(painel_final_horizontal)