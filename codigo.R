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

# Criando um novo dataframe para elaboração de gráficos

Categorias <- c("Moda Feminina", "Moda Masculina", "Moda Infantil")
Renda.Anual <- c(rfem, rmas, rinf)
Freq.Rel <- c("33,24%", "31,49%", "35,27%")

rendas_anuais <- data.frame(Categorias, Renda.Anual, Freq.Rel)

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

# Correção da Primeira Entrega Parcial

# Separando as vendas por mês

jan = vendas[substr(vendas$Data.Venda,1,2) == "1/", ]
fev = vendas[substr(vendas$Data.Venda,1,2) == "2/", ]
mar = vendas[substr(vendas$Data.Venda,1,2) == "3/", ]
abr = vendas[substr(vendas$Data.Venda,1,2) == "4/", ]
mai = vendas[substr(vendas$Data.Venda,1,2) == "5/", ]
jun = vendas[substr(vendas$Data.Venda,1,2) == "6/", ]
jul = vendas[substr(vendas$Data.Venda,1,2) == "7/", ]
ago = vendas[substr(vendas$Data.Venda,1,2) == "8/", ]
set = vendas[substr(vendas$Data.Venda,1,2) == "9/", ]
out = vendas[substr(vendas$Data.Venda,1,2) == "10", ]
nov = vendas[substr(vendas$Data.Venda,1,2) == "11", ]
dez = vendas[substr(vendas$Data.Venda,1,2) == "12", ]

# Separando o faturamento dos meses por categoria

fjan <- jan[jan$Category == "Women's Fashion", "Price"]
ffev <- fev[fev$Category == "Women's Fashion", "Price"]
fmar <- mar[mar$Category == "Women's Fashion", "Price"]
fabr <- abr[abr$Category == "Women's Fashion", "Price"]
fmai <- mai[mai$Category == "Women's Fashion", "Price"]
fjun <- jun[jun$Category == "Women's Fashion", "Price"]
fjul <- jul[jul$Category == "Women's Fashion", "Price"]
fago <- ago[ago$Category == "Women's Fashion", "Price"]
fset <- set[set$Category == "Women's Fashion", "Price"]
fout <- out[out$Category == "Women's Fashion", "Price"]
fnov <- nov[nov$Category == "Women's Fashion", "Price"]
fdez <- dez[dez$Category == "Women's Fashion", "Price"]

mjan <- jan[jan$Category == "Men's Fashion", "Price"]
mfev <- fev[fev$Category == "Men's Fashion", "Price"]
mmar <- mar[mar$Category == "Men's Fashion", "Price"]
mabr <- abr[abr$Category == "Men's Fashion", "Price"]
mmai <- mai[mai$Category == "Men's Fashion", "Price"]
mjun <- jun[jun$Category == "Men's Fashion", "Price"]
mjul <- jul[jul$Category == "Men's Fashion", "Price"]
mago <- ago[ago$Category == "Men's Fashion", "Price"]
mset <- set[set$Category == "Men's Fashion", "Price"]
mout <- out[out$Category == "Men's Fashion", "Price"]
mnov <- nov[nov$Category == "Men's Fashion", "Price"]
mdez <- dez[dez$Category == "Men's Fashion", "Price"]

ijan <- jan[jan$Category == "Kids' Fashion", "Price"]
ifev <- fev[fev$Category == "Kids' Fashion", "Price"]
imar <- mar[mar$Category == "Kids' Fashion", "Price"]
iabr <- abr[abr$Category == "Kids' Fashion", "Price"]
imai <- mai[mai$Category == "Kids' Fashion", "Price"]
ijun <- jun[jun$Category == "Kids' Fashion", "Price"]
ijul <- jul[jul$Category == "Kids' Fashion", "Price"]
iago <- ago[ago$Category == "Kids' Fashion", "Price"]
iset <- set[set$Category == "Kids' Fashion", "Price"]
iout <- out[out$Category == "Kids' Fashion", "Price"]
inov <- nov[nov$Category == "Kids' Fashion", "Price"]
idez <- dez[dez$Category == "Kids' Fashion", "Price"]

# Análise destrinchada do faturamento anual por categoria

# Criando tabelas para cada categoria

Meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
RendasF <- c((sum(fjan, na.rm = T)),(sum(ffev, na.rm = T)),(sum(fmar, na.rm = T)),(sum(fabr, na.rm = T)),(sum(fmai, na.rm = T)),(sum(fjun, na.rm = T)),(sum(fjul, na.rm = T)),(sum(fago, na.rm = T)),(sum(fset, na.rm = T)),(sum(fout, na.rm = T)),(sum(fnov, na.rm = T)),(sum(fdez, na.rm = T)))
RendasM <- c((sum(mjan, na.rm = T)),(sum(mfev, na.rm = T)),(sum(mmar, na.rm = T)),(sum(mabr, na.rm = T)),(sum(mmai, na.rm = T)),(sum(mjun, na.rm = T)),(sum(mjul, na.rm = T)),(sum(mago, na.rm = T)),(sum(mset, na.rm = T)),(sum(mout, na.rm = T)),(sum(mnov, na.rm = T)),(sum(mdez, na.rm = T)))
RendasI <- c((sum(ijan, na.rm = T)),(sum(ifev, na.rm = T)),(sum(imar, na.rm = T)),(sum(iabr, na.rm = T)),(sum(imai, na.rm = T)),(sum(ijun, na.rm = T)),(sum(ijul, na.rm = T)),(sum(iago, na.rm = T)),(sum(iset, na.rm = T)),(sum(iout, na.rm = T)),(sum(inov, na.rm = T)),(sum(idez, na.rm = T)))

FatF <- data.frame(Meses, RendasF)
FatF$Meses <- factor(FatF$Meses, levels = unique(FatF$Meses), ordered = T)
FatM <- data.frame(Meses, RendasM)
FatM$Meses <- factor(FatM$Meses, levels = unique(FatM$Meses), ordered = T)
FatI <- data.frame(Meses, RendasI)
FatI$Meses <- factor(FatI$Meses, levels = unique(FatI$Meses), ordered = T)

# Elaboração dos Gráficos

RF1 <- ggplot(FatF) +
  aes(x = Meses, y = RendasF, label = RendasF) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Mês", y = "Faturamento") + scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme_estat()
RF1

RM1 <- ggplot(FatM) +
  aes(x = Meses, y = RendasM, label = RendasM) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Mês", y = "Faturamento") + scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme_estat()
RM1

RI1 <- ggplot(FatI) +
  aes(x = Meses, y = RendasI, label = RendasI) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Mês", y = "Faturamento") + scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme_estat()
RI1


# Gráficos trimestrais e semestrais

FattF <- data.frame(Trimestre = c("Primeiro", "Segundo", "Terceiro", "Quarto"), Faturamento = c(5292, 1678, 5186, 5865))
FattF$Trimestre <- factor(FattF$Trimestre, levels = unique(FattF$Trimestre), ordered = T)
FattM <- data.frame(Trimestre = c("Primeiro", "Segundo", "Terceiro", "Quarto"), Faturamento = c(5088, 1784, 4224, 6028))
FattM$Trimestre <- factor(FattM$Trimestre, levels = unique(FattM$Trimestre), ordered = T)
FattI <- data.frame(Trimestre = c("Primeiro", "Segundo", "Terceiro", "Quarto"), Faturamento = c(3972, 1858, 6018, 7407))
FattI$Trimestre <- factor(FattI$Trimestre, levels = unique(FattI$Trimestre), ordered = T)

ggplot(FattF) +
  aes(x = Trimestre, y = Faturamento, label = Faturamento) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Trimestre", y = "Faturamento") +
  theme_estat()

ggplot(FattM) +
  aes(x = Trimestre, y = Faturamento, label = Faturamento) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Trimestre", y = "Faturamento") +
  theme_estat()

ggplot(FattI) +
  aes(x = Trimestre, y = Faturamento, label = Faturamento) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Trimestre", y = "Faturamento") +
  theme_estat()

################################################################################

# Segunda Entrega: Variação do preço por marca 

# Registrando as diferentes marcas

unique(vendas$Brand)

# Separando vetores de preço por marca

adidas <- vendas[vendas$Brand == "Adidas", "Price"]
hem <- vendas[vendas$Brand == "H&M", "Price"]
zara <- vendas[vendas$Brand == "Zara", "Price"]
gucci <- vendas[vendas$Brand == "Gucci", "Price"]
nike <- vendas[vendas$Brand == "Nike", "Price"]

# Calculando medidas de dispersão  para cada uma das marcas