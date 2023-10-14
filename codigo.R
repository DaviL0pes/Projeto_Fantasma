# Importando pacotes

library(tidyverse)

# Definindo o tema para gráficos

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}

# Definindo o dataframe para os dados das vendas

vendas <- read.csv('vendas.csv')

# Separando as categorias de produtos

feminina <- vendas[vendas$Category == "Women's Fashion", "Price"]
masculina <- vendas[vendas$Category == "Men's Fashion", "Price"]
infantil <- vendas[vendas$Category == "Kids' Fashion", "Price"]

# Calculando a renda anual de cada categoria

rfem = sum(feminina, na.rm = T)
rmas = sum(masculina, na.rm = T)
rinf = sum(infantil, na.rm = T)

# Repetindo o processo para os produtos que não foram devolvidos

feminina2 <- vendas[vendas$Category == "Women's Fashion" & is.na(vendas$Motivo.devolução), "Price"]
masculina2 <- vendas[vendas$Category == "Men's Fashion" & is.na(vendas$Motivo.devolução), "Price"]
infantil2 <- vendas[vendas$Category == "Kids' Fashion" & is.na(vendas$Motivo.devolução), "Price"]

rfem2 = sum(feminina2, na.rm = T)
rmas2 = sum(masculina2, na.rm = T)
rinf2 = sum(infantil2, na.rm = T)

# Criando um novo dataframe para elaboração de gráficos

Categorias <- c("Moda Feminina", "Moda Masculina", "Moda Infantil")
Renda.Anual <- c(rfem, rmas, rinf)
Freq.Rel <- c("33,24%", "31,49%", "35,27%")
Renda.Anual2 <- c(rfem2, rmas2, rinf2)
Freq.Rel2 <- c("32,97%", "30,91%", "36,12%")

rendas_anuais <- data.frame(Categorias, Renda.Anual, Freq.Rel, Renda.Anual2, Freq.Rel2)

# Primeiro gráfico de frequências

label = str_c(Renda.Anual, " (", Freq.Rel, ")")
RA1 <- ggplot(rendas_anuais) +
  aes(x = Categorias, y = Renda.Anual, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Categoria", y = "Renda Anual") +
  theme_estat()
RA1

# Segundo gráfico de frequências

label2 = str_c(Renda.Anual2, " (", Freq.Rel2, ")")
RA2 <- ggplot(rendas_anuais) +
  aes(x = Categorias, y = Renda.Anual2, label = label2) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Categoria", y = "Renda Anual") +
  theme_estat()
RA2
