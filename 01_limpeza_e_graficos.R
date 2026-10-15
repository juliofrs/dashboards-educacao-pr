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

# DEFINIÇÃO DA PALETA DE CORES (Alto Impacto Visual) ---------------------------
cores_impacto <- c("Estadual" = "#7209B7",  # Roxo Vibrante
                   "Municipal" = "#FF006E", # Magenta Intenso
                   "Privada" = "#06D6A0")   # Verde Esmeralda Brilhante

# 2. IMPORTAÇÃO DOS DADOS ------------------------------------------------------
caminho_arquivo <- "Matriculas_faixa_etaria_rede_municipio_consultas.xlsx"
df_bruto <- read_excel(caminho_arquivo, sheet = "Plan1")

# 3. VERIFICAÇÃO INICIAL -------------------------------------------------------
glimpse(df_bruto)

# 4. PREPARAÇÃO DOS DADOS: GRÁFICO 1 -------------------------------------------
df_temporal <- df_bruto %>%
  filter(Rede %in% c("Estadual", "Municipal", "Privada")) %>%
  group_by(Ano, Rede) %>%
  summarise(Total_Matriculas = sum(`Total Educação Básica`, na.rm = TRUE), .groups = 'drop')

df_rotulos <- df_temporal %>% filter(Ano == 2025)

# 5. CONSTRUÇÃO DO GRÁFICO 1 ---------------------------------------------------
grafico_1 <- ggplot(df_temporal, aes(x = Ano, y = Total_Matriculas, color = Rede)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = df_rotulos, size = 2) + 
  geom_text(data = df_rotulos, aes(label = Rede), hjust = -0.1, size = 4, fontface = "bold") +
  scale_x_continuous(breaks = seq(2016, 2025, by = 1), limits = c(2016, 2026.5)) + 
  scale_y_continuous(limits = c(0, 1200000), breaks = seq(0, 1200000, by = 300000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_color_manual(values = cores_impacto) +
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
  pivot_longer(cols = c(`Anos Iniciais`, `Anos Finais`, `Ensino Médio`), 
               names_to = "Etapa", values_to = "Matriculas") %>%
  mutate(Etapa = factor(Etapa, levels = c("Anos Iniciais", "Anos Finais", "Ensino Médio")))

df_etapas_rotulos_inicio <- df_etapas %>% filter(Etapa == "Anos Iniciais")
df_etapas_rotulos_fim <- df_etapas %>% filter(Etapa == "Ensino Médio")

# 7. CONSTRUÇÃO DO GRÁFICO 2 ---------------------------------------------------
grafico_2 <- ggplot(df_etapas, aes(x = Etapa, y = Matriculas, group = Rede, color = Rede)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  geom_text(data = df_etapas_rotulos_inicio, aes(label = Rede), hjust = 1.2, size = 4, fontface = "bold") +
  geom_text(data = df_etapas_rotulos_fim, aes(label = format(Matriculas, big.mark = ".")), hjust = -0.2, size = 4) +
  scale_color_manual(values = cores_impacto) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "A Transição de Etapas", subtitle = NULL, x = "", y = "") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.line = element_line(color = "black", linewidth = 0.3))

# 8. PREPARAÇÃO DOS DADOS: GRÁFICO 3 (A Curva da Vida Escolar) -----------------
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

df_idade_rotulos <- df_idade %>% filter(Faixa_Etaria == "15 a 17 anos")

# 9. CONSTRUÇÃO DO NOVO GRÁFICO 3 ----------------------------------------------
grafico_3 <- ggplot(df_idade, aes(x = Faixa_Etaria, y = Matriculas, group = Rede, color = Rede)) +
  geom_line(linewidth = 1.5) + geom_point(size = 3) +
  geom_text(data = df_idade_rotulos, aes(label = Rede), hjust = -0.1, size = 4.5, fontface = "bold") +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.15))) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), limits = c(0, NA)) +
  scale_color_manual(values = cores_impacto) +
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
  mutate(Etapa = factor(Etapa, 
                        levels = c("Creche", "Pré-escola", "Anos Iniciais", 
                                   "Anos Finais", "Ensino Médio"),
                        labels = c("Creche", "Pré", "Fund. I", "Fund. II", "Médio")))

# 11. CONSTRUÇÃO DO GRÁFICO 4 --------------------------------------------------
grafico_4 <- ggplot(df_vocacao, aes(x = Etapa, y = Matriculas, fill = Rede)) +
  geom_col(width = 0.7) + facet_wrap(~ Rede, ncol = 3) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), limits = c(0, NA)) +
  scale_fill_manual(values = cores_impacto) +
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "Vocação das Redes", subtitle = NULL, x = "", y = "") + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
        axis.line.x = element_line(color = "black", linewidth = 0.3),
        strip.text = element_text(face = "bold", size = 12, margin = margin(b = 10)))

# 12. MONTAGEM FINAL DA FOLHA A4 (Patchwork - Layout Horizontal) ---------------
painel_final_horizontal <- (grafico_1 | grafico_3) / 
  (grafico_2 | grafico_4) +
  plot_annotation(
    title = "Panorama das Matrículas na Educação Básica no Paraná (2016-2025)",
    caption = "Fonte: Bases do Governo (Adaptado) | Criação: Julio Freitas",
    theme = theme(plot.title = element_text(size = 20, face = "bold", family = "serif", hjust = 0.5, margin = margin(b = 15)),
                  plot.caption = element_text(size = 10, family = "serif", color = "gray50"))
  )

# 13. EXPORTAÇÃO PARA IMPRESSÃO (Tamanho A4 Horizontal/Paisagem) ---------------
ggsave("Painel_Final_A4_Horizontal.pdf", plot = painel_final_horizontal, 
       width = 29.7, height = 21, units = "cm", 
       device = cairo_pdf) 

ggsave("Painel_Final_A4_Horizontal.png", plot = painel_final_horizontal, 
       width = 29.7, height = 21, units = "cm", dpi = 300, bg = "white")

# Exibe na aba Plots
print(painel_final_horizontal)