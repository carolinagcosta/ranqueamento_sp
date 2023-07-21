####################################################################
#### Análise de Correspondência Múltipla + Análise Fatorial PCA ####
####################################################################

#### 1. Instalação e carregamento dos pacotes utilizados ####

pacotes <- c("plotly",
             "tidyverse",
             "ggrepel",
             "knitr", 
             "kableExtra",
             "reshape2",
             "PerformanceAnalytics", 
             "psych",
             "ltm", 
             "Hmisc",
             "readxl",
             "sjPlot",
             "ade4")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#### 2. Carregamento da base de dados ####

sp <- read_xlsx('sp.xlsx') #dados da planilha para teste. Importante que haja uma variável quantitativa para a análise

view(sp)

# Separação das variáveis em qualitativas e quantitativas
var_quali <- sp[,c(3:6)]
var_quanti <- sp[,2]

## O objetivo é criar um ranking que capture os valores quantitativos

# A função para a criação da ACM pede que sejam utilizados "fatores"
var_quali <- as.data.frame(unclass(var_quali), stringsAsFactors=TRUE)

# Estatísticas descritivas
summary(var_quali)
summary(var_quanti)

str(var_quali)

# Iniciando a Análise de Correspondência Múltipla nas variáveis qualitativas
# É importante que as variáveis qualitativas apresentem alguma correlação
sjt.xtab(var.row = var_quali$amostra_disponível,
         var.col = var_quali$amostra_obtencao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")#correlação significativa

sjt.xtab(var.row = var_quali$amostra_disponível,
         var.col = var_quali$iucn,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8") #correlação significativa

sjt.xtab(var.row = var_quali$decisoes,
         var.col = var_quali$amostra_disponível,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8") #correlação significativa

#todas as categorias estavam signiticamente associadas a pelo menos uma outra categoria

# Análise de Correspondência Múltipla

ACM <- dudi.acm(var_quali, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
perc_variancia

# Quantidade de categorias por variável
quant_categorias <- apply(var_quali,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# Obtendo as coordenadas das observações
coord_obs <- ACM$li

# Adicionando as coordenadas ao banco de dados de variáveis quantitativas
var_quanti <- bind_cols(var_quanti, coord_obs)

# Coeficientes de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(var_quanti), type="pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes

### Elaboração da Análise Fatorial Por Componentes Principais ###

# Teste de esfericidade de Bartlett - testa a hipótese de que as variáveis não são correlacionadas na população.
cortest.bartlett(var_quanti)

# Elaboração da análise fatorial por componentes principais
fatorial <- principal(var_quanti,
                      nfactors = length(var_quanti),
                      rotate = "none",
                      scores = TRUE)
fatorial

# Eigenvalues (autovalores)
eigenvalues <- round(fatorial$values, 5)
eigenvalues
round(sum(eigenvalues), 2) # soma dos autovalores

# Identificação da variância compartilhada em cada fator
variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")

# Variância compartilhada pelas variáveis originais para a formação de cada fator
round(variancia_compartilhada, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

### Elaboração da Análise Fatorial por Componentes Principais ###
### Fatores extraídos a partir de autovalores maiores que 1 ###

# Definição da quantidade de fatores com eigenvalues maiores ou iguais a 1
k <- sum(eigenvalues >= 1)
print(k)

# Elaboração da análise fatorial por componentes principais
fatorial_final <- principal(var_quanti,
                            nfactors = k,
                            rotate = "none",
                            scores = TRUE)

# Cálculo dos scores fatoriais
scores_fatoriais <- as.data.frame(fatorial_final$weights)

# Visualização dos scores fatoriais
round(scores_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos fatores propriamente ditos
fatores <- as.data.frame(fatorial_final$scores)
View(fatores)

# Cálculo das cargas fatoriais
cargas_fatoriais <- as.data.frame(unclass(fatorial_final$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo das comunalidades
comunalidades <- as.data.frame(unclass(fatorial_final$communality)) %>%
  rename(comunalidades = 1)

# Visualização das comunalidades para os 2 fatores extraídos
round(comunalidades, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Loading plot com as cargas dos 2 primeiros fatores
cargas_fatoriais[, 1:2] %>%
  data.frame() %>%
  rownames_to_column("variáveis") %>%
  ggplot(aes(x = PC1, y = PC2, label = variáveis)) +
  geom_point(color = "darkblue",
             size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  theme_bw()

# Criação de um ranking Critério da soma ponderada e ordenamento)
sp$ranking <- fatores$PC1 * variancia_compartilhada$PC1[2] +
                 fatores$PC2 * variancia_compartilhada$PC2[2]

# Ranking e valor
corr_valor <- rcorr(as.matrix(sp[,c(2,7)]))

valor_corr_coef <- corr_valor$r # Matriz de correlações
valor_corr_sig <- round(corr_valor$P, 5) # Matriz com p-valor dos coeficientes

# Fim!