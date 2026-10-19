# ==============================================================================
# PROJETO: O Funil da Educação Básica no Paraná (AV1)
# OBJETIVO: Análise visual de matrículas aplicando princípios de Tufte
# ==============================================================================

# 1. CARREGAMENTO DOS PACOTES --------------------------------------------------
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

# 4. PREPARAÇÃO DOS DADOS: GRÁFICO 1 (Evolução Histórica Geral - Gráfico de Linhas)
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
  labs(title = "Evolução Histórica - Matrículas por rede", subtitle = NULL, x = "", y = "") + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3))

# 6. PREPARAÇÃO DOS DADOS: GRÁFICO 2 (O Paradoxo da Rede Privada - Barras) -----
df_privada_pct <- df_bruto %>%
  filter(Ano == 2025) %>%
  group_by(Município) %>%
  summarise(
    Total_Alunos = sum(`Total Educação Básica`, na.rm = TRUE),
    Total_Privada = sum(`Total Educação Básica`[Rede == "Privada"], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(Total_Alunos >= 1000) %>%
  mutate(Pct_Privada = (Total_Privada / Total_Alunos) * 100) %>%
  slice_max(Pct_Privada, n = 5) %>%
  mutate(Município = reorder(Município, Pct_Privada))

# 7. CONSTRUÇÃO DO GRÁFICO 2 ---------------------------------------------------
grafico_2 <- ggplot(df_privada_pct, aes(x = Pct_Privada, y = Município)) +
  geom_col(fill = cores_impacto["Privada"], width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", Pct_Privada)), 
            hjust = -0.2, size = 5, fontface = "bold", color = cores_impacto["Privada"]) +
  scale_x_continuous(limits = c(0, 70)) + 
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "O Paradoxo da Rede Privada", 
       subtitle = "Top 5 cidades com maior % de alunos no ensino particular (2025)",
       x = "", y = "") +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray30"),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold"))

# 8. PREPARAÇÃO DOS DADOS: GRÁFICO 3 (O Ciclo por Idade) -----------------------
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

# 9. CONSTRUÇÃO DO GRÁFICO 3 ---------------------------------------------------
grafico_3 <- ggplot(df_idade, aes(x = Faixa_Etaria, y = Matriculas, group = Rede, color = Rede)) +
  geom_line(linewidth = 1.5) + geom_point(size = 3) +
  geom_text(data = df_idade_rotulos, aes(label = Rede), hjust = -0.1, size = 4.5, fontface = "bold") +
  # 1. Aumentamos o multiplicador da direita (de 0.15 para 0.30) para afastar a borda
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.30))) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), limits = c(0, NA)) +
  scale_color_manual(values = cores_impacto) +
  # 2. Desativamos o corte (clipping) para que o texto possa invadir a margem
  coord_cartesian(clip = "off") +
  theme_tufte(base_size = 12, base_family = "serif") +
  labs(title = "O Ciclo por Idade", subtitle = NULL, x = "", y = "") +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 11, face = "bold"),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        # 3. Adicionamos margem física extra na direita (Top, Right, Bottom, Left)
        plot.margin = margin(t = 10, r = 40, b = 10, l = 10))

# 10. PREPARAÇÃO DOS DADOS: GRÁFICO 4 (Vocação das Redes) ----------------------
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
       width = 29.7, height = 21, units = "cm", device = cairo_pdf) 

print(painel_final_horizontal)


