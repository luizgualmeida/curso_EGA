# =============================================================================
#  PSICOMETRIA DE REDES — SCRIPT COMPLETO DO CURSO
# =============================================================================
#
#  ESTRUTURA DO SCRIPT
#  ─────────────────────────────────────────────────────────────────────────
#  DIA 1 – HORA 1:  Teoria (slides – sem código)
#  DIA 1 – HORA 2:  Prática 1 — Preparação, exploração e UVA
#  DIA 2 – HORA 1:  Prática 2 — EGA e visualização
#  DIA 2 – HORA 2:  Prática 3 — Estabilidade, invariância e interpretação
#  ─────────────────────────────────────────────────────────────────────────
#
#  DICA: execute o código BLOCO POR BLOCO (Ctrl+Enter no RStudio).
#  Não tente rodar tudo de uma vez logo no início.
# =============================================================================


# =============================================================================
#  BLOCO 0 – INSTALAÇÃO E CARREGAMENTO DOS PACOTES
# =============================================================================

# Instale apenas se ainda não tiver instalado.
# Remova o "#" na frente de install.packages() da linha correspondente
# e rode UMA VEZ. Depois coloque o "#" de volta.
# 
# install.packages("EGAnet")      # pacote principal – criado por Golino
# install.packages("tidyverse")   # manipulação de dados
# install.packages("psych")       # estatísticas descritivas e correlações
# install.packages("qgraph")      # visualização de redes
# install.packages("ggplot2")     # gráficos elegantes
# install.packages("dplyr")       # manipulação de dados
# install.packages("haven")       # leitura de arquivos .sav (SPSS)

# Carregando os pacotes -------------------------------------------------------
library(EGAnet)     # Exploratory Graph Analysis (Golino & Epskamp, 2017)
library(tidyverse)  # conjunto de pacotes para ciência de dados
library(psych)      # estatísticas psicométricas clássicas
library(qgraph)     # visualização de redes psicométricas
library(ggplot2)    # gráficos
library(dplyr)      # manipulação de data frames

# Verificando as versões (importante para reprodutibilidade)
packageVersion("EGAnet")   # idealmente >= 2.0
packageVersion("psych")    # idealmente >= 2.0


# =============================================================================
#  BLOCO 1 – CARREGANDO E PREPARANDO OS DADOS
# =============================================================================

# Os dados vêm do estudo: Trust in scientists and their role in society across 
# 68 countries. Cologna et.al 2025. Nature Human Behaviour"
#
# O banco "ds_main.rds" contém respostas de vários países.
# Vamos focar no Brasil, mas o código para todos os países
# está comentado logo abaixo.

# --- 1.1 Carregando o banco de dados -----------------------------------------

dados_completos <- readRDS("ds_main.rds")

# Sempre inspecione o banco antes de qualquer análise!
dim(dados_completos)       # quantas linhas (participantes) e colunas (variáveis)?
names(dados_completos)     # quais são os nomes das variáveis?
head(dados_completos, 3)   # primeiras 3 linhas


# Verificando os países disponíveis no banco
table(dados_completos$COUNTRY_CODE)   


# --- 1.2 Filtrando apenas o Brasil -------------------------------------------

# FOCO DO CURSO: dados do Brasil
dados_br <- dados_completos %>%   #|> 
  filter(COUNTRY_CODE == "BRA")    # ajuste o nome da categoria se necessário

# Verificando quantos participantes temos do Brasil
nrow(dados_br)


# --- 1.3 [OPCIONAL] Rodando com todos os países ------------------------------
# Para usar todos os países, basta comentar o filtro acima e usar:
#
# dados_analise <- dados_completos   # sem filtro de país
#
# Para analisar países específicos, pode-se usar:
# dados_BRA_USA_DEU <- dados_completos |>
#   filter(COUNTRY_CODE %in% c("BRA", "USA", "DEU"))


# --- 1.4 Selecionando apenas os itens de confiança ---------------------------
# A escala de confiança nos cientistas tem múltiplos itens.
# Veja o codebook ou o artigo para identificar quais colunas são os itens.
# Abaixo, ajuste os nomes das colunas conforme o seu banco real.

# EXEMPLO — substitua pelos nomes reais das colunas da sua escala:
# Os itens provavelmente seguem um padrão 

# Identificando colunas de itens (procure por padrão de nome)
nomes_colunas <- names(dados_br)
print(nomes_colunas)  # examine a lista e identifique os itens

# Selecione os itens manualmente — ajuste conforme o banco real:
# (Este é um exemplo genérico; substitua pelos nomes reais)
itens_trust <- dados_br |>
  select(starts_with("TRUST_SCI_"))   # ajuste o prefixo conforme necessário

# Se preferir selecionar por posição de coluna:
# itens_trust <- dados_br %>% 
#   dplyr::select(59:70)


# Verificando a seleção
dim(itens_trust)
head(itens_trust)

# Salvando o objeto final que usaremos nas análises
# saveRDS(itens_trust, file = "itens_trust")
# write.csv(itens_trust, file = "itens_trust-comma")

# =============================================================================
#  BLOCO 2 – ESTATÍSTICAS DESCRITIVAS E EXPLORAÇÃO INICIAL
# =============================================================================

# Antes de qualquer análise de rede, é essencial conhecer os seus dados.
# Pule esta etapa e você pode chegar a conclusões equivocadas.

# --- 2.1 Descritivas básicas -------------------------------------------------

# Usando o pacote psych para um resumo completo
describe(itens_trust)
# Preste atenção em:
# - n: tamanho amostral por item (dados ausentes?)
# - mean: média de cada item
# - sd: desvio padrão
# - skew: assimetria (valores > |2| são problemáticos)
# - kurtosis: curtose (valores > |7| são problemáticos)
# - range: mín e máx (verifique se fazem sentido para a escala)


# --- 2.2 Dados ausentes (missing data) ----------------------------------------

# Contar quantos NAs existem por coluna
missing_por_item <- colSums(is.na(itens_trust))
print(missing_por_item)

# Percentual de dados ausentes por item
missing_pct <- round(missing_por_item / nrow(itens_trust) * 100, 2)
print(missing_pct)

# Regra geral: se > 10% de missing, investigue antes de prosseguir.


# --- 2.3 Distribuições por item -----------------------------------------------

# Frequências de resposta para cada item (útil para escalas Likert)
for (item in names(itens_trust)) {
  cat("\n--- Item:", item, "---\n")
  print(table(itens_trust[[item]], useNA = "always"))
}


# --- 2.4 Matriz de correlação visual ------------------------------------------

# A rede é construída a partir das correlações entre itens.
# Vale a pena visualizar a matriz de correlação antes.

cor_matrix <- cor(itens_trust, use = "pairwise.complete.obs")
round(cor_matrix, 2)   # arredondando para facilitar leitura

# Visualizando a matriz de correlação como mapa de calor
heatmap(cor_matrix)

# =============================================================================
#  BLOCO 3 – PASSO 1: ANÁLISE DE VARIÁVEIS ÚNICAS (UVA)
#  Identificar e tratar redundâncias entre itens
# =============================================================================

# O que é UVA?
# ─────────────────────────────────────────────────────────────────────────
# Antes de rodar a EGA, precisamos verificar se há itens REDUNDANTES.
# Dois itens são redundantes quando medem essencialmente a mesma coisa —
# ou seja, quando a correlação entre eles é tão alta que um não acrescenta
# informação além do outro.
#
# UVA usa a medida "weighted Topological Overlap" (wTO):
# - wTO > 0.25 = redundância moderada a grande (deve ser tratada)
# - wTO > 0.30 = redundância grande a muito grande
#
# Por que isso importa?
# Itens redundantes inflam artificialmente dimensões e distorcem a rede.
# Referência: Christensen, Garrido & Golino (2023)
# ─────────────────────────────────────────────────────────────────────────

# --- 3.1 Rodando a UVA -------------------------------------------------------

# ATENÇÃO: retire os NAs antes da UVA
itens_trust_uva <- itens_trust[complete.cases(itens_trust), ]

cat("N após remoção de NAs:", nrow(itens_trust_uva), "\n")

# Rodando a UVA
# O argumento "key" permite passar os textos reais dos itens (opcional, mas útil)
uva_resultado <- UVA(
  data = itens_trust_uva
  # key = c("Item 1 - texto aqui", "Item 2 - texto aqui", ...)
  # Descomente a linha acima e substitua pelos textos reais dos seus itens
)

# Exibindo os resultados
print(uva_resultado)

# O que procurar na saída:
# - Pares de variáveis com wTO > 0.25 (redundância)


# --- 3.2 Inspecionando o resultado da UVA ------------------------------------

# Quais variáveis foram mantidas e quais foram removidas?
uva_resultado$keep_remove

# O banco de dados já reduzido (sem itens redundantes):
dados_reduzidos <- uva_resultado$reduced_data

cat("\nDimensões dos dados originais:", dim(itens_trust_uva), "\n")
cat("Dimensões dos dados reduzidos:", dim(dados_reduzidos), "\n")
cat("Itens removidos por redundância:",
    ncol(itens_trust_uva) - ncol(dados_reduzidos), "\n")


# --- 3.3 [OPCIONAL] Tratamento manual de redundâncias ------------------------
# Se quiser tratar manualmente (removendo item específico):
#
# dados_reduzidos <- dados_completos_uva |>
#   dplyr::select(-nome_do_item_redundante)



# =============================================================================
#  BLOCO 4 – PASSO 2: EGA — EXPLORATORY GRAPH ANALYSIS
#  O coração do curso
# =============================================================================

# O que é a EGA?
# ─────────────────────────────────────────────────────────────────────────
# Proposta por Golino & Epskamp (2017), a EGA combina dois passos:
#
# 1. GLASSO (Graphical LASSO): estima a matriz de correlações PARCIAIS
#    entre os itens, penalizando conexões fracas (empurrando-as para zero).
#    Isso reduz o ruído e mantém apenas conexões "verdadeiras".
#    O grau de penalização (lambda) é selecionado automaticamente pelo
#    EBIC entre 100 valores testados (gamma = 0.5 por padrão).
#
# 2. Algoritmo de comunidade: detecta grupos (clusters) de itens
#    densamente conectados entre si. Cada comunidade = dimensão latente.
#
# A lógica central: CLUSTERS NA REDE = VARIÁVEIS LATENTES
# ─────────────────────────────────────────────────────────────────────────



# --- 4.2 Rodando a EGA -------------------------------------------------------

set.seed(42)   # fixando a semente para reprodutibilidade

ega_trust <- EGA(
  data = itens_trust, 
  model = "glasso",
  algorithm = "walktrap",
  plot.EGA = TRUE
)



# --- 4.1 O argumento uni.method — avaliação da unidimensionalidade ---------
#
# Antes de estimar a estrutura dimensional, a EGA precisa decidir se os dados
# apresentam mais de uma dimensão. Essa etapa é importante porque algoritmos
# de detecção de comunidades podem, ocasionalmente, retornar apenas uma única
# comunidade mesmo quando existem subestruturas relevantes nos dados.
#
# O argumento `uni.method` define qual procedimento será utilizado para
# verificar se uma solução unidimensional é plausível antes de aceitar a
# estrutura final estimada pela EGA.
#
# IMPORTANTE:
# O objetivo desses métodos NÃO é determinar o número exato de dimensões,
# mas avaliar se a hipótese de unidimensionalidade deve ser mantida ou
# rejeitada. Caso a unidimensionalidade seja rejeitada, a EGA prossegue com
# a estimação normal da rede e identificação das comunidades.
#
# ── "expand" ──────────────────────────────────────────────────────────────
#
# Método recomendado pelos autores da EGA (Golino et al., 2020).
#
# A matriz de correlação é expandida com quatro variáveis artificiais
# correlacionadas entre si (r = .50). Essa expansão cria uma estrutura
# conhecida que auxilia na identificação de situações em que uma dimensão
# dominante pode estar mascarando dimensões secundárias.
#
# Interpretação:
#   • Se a EGA da matriz expandida identificar até duas dimensões,
#     a estrutura é considerada compatível com unidimensionalidade.
#
#   • Se identificar três ou mais dimensões,
#     a hipótese de unidimensionalidade é rejeitada e a EGA é então aplicada
#     à matriz original para estimar a solução final.
#
# Vantagens:
#   • Maior sensibilidade para detectar multidimensionalidade fraca.
#   • Melhor desempenho em simulações quando fatores são moderadamente
#     correlacionados.
#   • Atualmente é o procedimento mais recomendado para aplicações
#     psicológicas e educacionais.
#
# Referência:
# Golino et al. (2020). Psychological Methods.
#
#
# ── "LE" (Leading Eigenvector) ────────────────────────────────────────────
#
# Utiliza o algoritmo de detecção de comunidades Leading Eigenvector
# diretamente sobre a matriz de correlação empírica.
#
# A decisão é simples:
#   • 1 comunidade  → estrutura considerada unidimensional
#   • >1 comunidade → hipótese de unidimensionalidade rejeitada
#
# Como o método é baseado na decomposição espectral da matriz,
# ele tende a ser mais conservador que o "expand".
#
# Vantagens:
#   • Mais rápido computacionalmente.
#   • Baseado na estrutura espectral dos dados.
#
# Limitações:
#   • Menor sensibilidade para detectar dimensões fracas ou muito
#     correlacionadas.
#
# Referência:
# Christensen et al. (2023). Behavior Research Methods.
#
#
# ── "louvain" (padrão histórico) ──────────────────────────────────────────
#
# Aplica o algoritmo Louvain diretamente à matriz de correlação observada.
#
# Decisão:
#   • 1 comunidade  → estrutura considerada unidimensional
#   • >1 comunidade → hipótese de unidimensionalidade rejeitada
#
# Embora tenha sido durante muito tempo o procedimento padrão,
# estudos recentes mostraram que ele pode apresentar menor capacidade de
# detectar multidimensionalidade quando as dimensões são fortemente
# correlacionadas.
#
# Por esse motivo, os autores do pacote atualmente recomendam
# preferencialmente o método "expand".
#
# Referência:
# Christensen & Golino (2021); Golino et al. (2020)
#
#
# RECOMENDAÇÃO PRÁTICA
#
# Para aplicações empíricas em psicologia, educação e ciências sociais:
#
#   uni.method = "expand"
#
# é geralmente a opção preferida por apresentar maior acurácia na distinção
# entre estruturas unidimensionais e multidimensionais.
#
# Caso a teoria sugira múltiplas dimensões, mas a EGA retorne apenas uma,
# vale verificar se o resultado permanece estável utilizando "expand" ou
# "LE" antes de concluir que o construto é verdadeiramente unidimensional.
# ─────────────────────────────────────────────────────────────────────────


# --- 4.2 Rodando a EGA -------------------------------------------------------

set.seed(42)   # fixando a semente para reprodutibilidade

ega_resultado <- EGA(
  data = itens_trust,
  
  # ESTIMAÇÃO DA REDE
  model = "glasso",
  # "glasso"  → GLASSO com seleção EBIC (padrão e mais usado na literatura)
  # "TMFG"    → Triangulated Maximally Filtered Graph (alternativa, mais esparsa)
  # "BGGM"    → Bayesian GGM (mais lento, adequado para N pequeno)
  
  # ALGORITMO DE DETECÇÃO DE COMUNIDADES
  algorithm = "walktrap",
  # "louvain"  → Louvain com consensus clustering (bom para dados
  #              psicologia; padrão atual do EGAnet v2+)
  # "walktrap" → Walktrap (usado no artigo original de 2017; mais conservador)
  # "leiden"   → Leiden (variante melhorada do Louvain; menos usado na prática)
  # NOTA: o algoritmo pode influenciar o número de comunidades encontradas.
  #       Cheque sempre a consistência do resultado com a TEORIA.
  
  # VERIFICAÇÃO DE UNIDIMENSIONALIDADE
  uni.method = "expand",
  # Veja a explicação detalhada no bloco 4.1 acima.
  # "expand"  → RECOMENDADO quando o resultado padrão retorna 1 dimensão
  #             inesperadamente. Usa expansão artificial da matriz para
  #             testar se unidimensionalidade é genuína ou artefato do
  #             algoritmo. (Golino et al., 2020, Psychological Methods)
  # "LE"      → Leading Eigenvector; teste mais conservador.
  #             (Christensen et al., 2023, Behavior Research Methods)
  # "louvain" → Padrão do pacote; pode colapsar dimensões reais em dados
  #             com fatores moderadamente correlacionados.
  
  plot.EGA = TRUE   # gerar o gráfico automaticamente
)

# Exibindo o resumo dos resultados
summary(ega_resultado)


# --- 4.3 Interpretando o output ----------------------------------------------

# O summary mostra:
# ─────────────────────────────────────────────────────────────────────────
# Model: GLASSO (EBIC com gamma = 0.5)
#   → gamma controla a esparsidade: maior gamma = rede mais esparsa
#   → EBIC seleciona o lambda ótimo entre 100 valores testados
#
# Number of nodes: N de itens na rede
# Number of edges: N de conexões não-nulas
# Edge density: proporção de conexões presentes vs. possíveis
#
# Non-zero edge weights:
#   M: média dos pesos das arestas
#   SD: desvio padrão dos pesos
#   Min/Max: peso mínimo e máximo
#
# Algorithm: Louvain (ou o que foi especificado)
# Number of communities: número de DIMENSÕES identificadas ← resultado central
#
# Membership: qual item pertence a qual dimensão
#
# Unidimensional: No → confirma estrutura multidimensional
#                 Yes → dados parecem unidimensionais (cheque com uni.method!)
#
# TEFI: Total Entropy Fit Index — quanto menor (mais negativo), melhor o ajuste
#       Útil para comparar duas soluções alternativas com o mesmo dataset
# ─────────────────────────────────────────────────────────────────────────

cat("\nNúmero de dimensões encontradas:", ega_resultado$n.dim, "\n")
cat("\nAlocação dos itens por dimensão:\n")
print(ega_resultado$wc)   # wc = walktrap/louvain communities


# --- 4.4 Visualização da rede ------------------------------------------------

# O gráfico já é gerado automaticamente pelo plot.EGA = TRUE.
# Para customizar (cores, tamanho dos nós, rótulos), use plot():
plot(ega_resultado)



# --- 4.5 Comparando algoritmos de comunidade ---------------------------------
#
# É boa prática verificar se o resultado é robusto ao algoritmo escolhido.
# Rode a EGA com pelo menos dois algoritmos e compare o n.dim:

set.seed(42)
ega_walktrap <- EGA(
  data    = itens_trust,
  model   = "glasso",
  algorithm  = "walktrap",
  uni.method = "expand",
  plot.EGA   = FALSE       # sem gráfico para esta comparação
)

set.seed(42)
ega_leiden <- EGA(
  data    = itens_trust,
  model   = "glasso",
  algorithm  = "leiden",
  uni.method = "expand",
  plot.EGA   = FALSE
)

# Tabela comparativa
cat("\n--- Comparação de algoritmos ---\n")
cat("Louvain  :", ega_resultado$n.dim, "dimensões\n")
cat("Walktrap :", ega_walktrap$n.dim,  "dimensões\n")
cat("Leiden   :", ega_leiden$n.dim,    "dimensões\n")
#
# Se os três concordarem → resultado robusto ao algoritmo, maior confiança.
# Se divergirem → investigue com o bootEGA qual solução é mais estável.


# --- 4.6 [OPCIONAL] EGA com dados de todos os países -------------------------
# Para replicar com todos os países (sem filtro):
#
# dados_todos <- dados_completos |>
#   select(starts_with("ts_")) |>  # ajuste o prefixo
#   na.omit()
#
# uva_todos   <- UVA(data = dados_todos)
# ega_todos   <- EGA(
#   data       = uva_todos$reduced_data,
#   model      = "glasso",
#   algorithm  = "louvain",
#   uni.method = "expand"   # importante manter aqui também
# )
# summary(ega_todos)




# =============================================================================
#  BLOCO 5 – PASSO 3: bootEGA — ESTABILIDADE DA SOLUÇÃO
#  Quão confiável é a estrutura que encontramos?
# =============================================================================

# Por que verificar a estabilidade?
# ─────────────────────────────────────────────────────────────────────────
# A EGA nos dá UMA solução com os dados que temos.
# Mas e se a amostra fosse um pouco diferente?
# A estrutura mudaria muito?
#
# O bootEGA responde isso via bootstrap paramétrico:
# 1. Usa a estrutura da EGA para gerar 500 amostras sintéticas
# 2. Roda a EGA em cada amostra
# 3. Verifica com que frequência a mesma estrutura é recuperada
#
# Referência: Christensen & Golino (2021)
# ─────────────────────────────────────────────────────────────────────────

# ATENÇÃO: este passo é computacionalmente mais pesado.
# 500 iterações podem levar alguns minutos dependendo do computador.

set.seed(42)

boot_resultado <- bootEGA(
  data = itens_trust_completo,
  iter = 500,              # número de amostras bootstrap (padrão: 500)
  seed = 42,              # reprodutibilidade
  type = "parametric",    # "parametric" (padrão) ou "resampling"
  plot.typicalStructure = TRUE  # gráfico da estrutura mediana
)

# Resumo do bootEGA
summary(boot_resultado)


# --- 5.1 Interpretando o bootEGA ---------------------------------------------

# O summary mostra:
# ─────────────────────────────────────────────────────────────────────────
# Bootstrap Samples: 500 (Parametric)
#
# Frequency: com que frequência cada Nº de dimensões foi encontrado
# → Ex: "5: 0.94" significa que 94% das amostras bootstrap
#   retornaram 5 dimensões
#
# Median dimensions: X [IC 95%]
# → O número mediano de dimensões e seu intervalo de confiança
# ─────────────────────────────────────────────────────────────────────────


# --- 5.2 Estabilidade dimensional --------------------------------------------

# Verifica a estabilidade de cada DIMENSÃO e de cada ITEM

estab_dimensional <- dimensionStability(boot_resultado)
print(estab_dimensional)

# O que observar:
# ─────────────────────────────────────────────────────────────────────────
# Proportion Replicated in Dimensions:
#   Para cada ITEM — com que frequência ele apareceu na mesma dimensão
#   que na solução empírica original.
#   → Critério: valores >= 0.70 são considerados estáveis
#               valores >= 0.75 são suficientemente estáveis
#
# Structural Consistency:
#   Para cada DIMENSÃO — com que frequência ela foi recuperada
#   exatamente (mesma composição de itens) nas amostras bootstrap.
#   → Mesmo critério: >= 0.70 a 0.75
# ─────────────────────────────────────────────────────────────────────────


# --- 5.3 Comparando estrutura empírica vs. estrutura mediana bootstrap --------

comparacao <- compare.EGA.plots(
  ega_resultado,
  boot_resultado,
  labels = c("EGA Empírica", "Estrutura Bootstrap Mediana")
)
# Idealmente os dois gráficos devem ser muito similares.


# =============================================================================
#  BLOCO 6 – MÉTRICAS DE CENTRALIDADE DA REDE
#  Quais itens são mais "importantes" na rede?
# =============================================================================

# Centralidade: medidas que quantificam a importância de cada nó na rede.
# Na psicometria de redes, nós são os itens (variáveis).

# --- 6.1 Calculando centralidade ---------------------------------------------

centralidade <- centrality_auto(ega_resultado$network)

# Tipos de centralidade mais relevantes:
# ─────────────────────────────────────────────────────────────────────────
# Strength (Força): soma dos pesos absolutos das arestas conectadas ao nó
#   → Itens com alta força têm conexões fortes com muitos outros itens
#
# Betweenness (Intermediação): frequência com que o nó fica no
#   caminho mais curto entre outros pares de nós
#   → Itens "ponte" entre dimensões terão alta intermediação
#
# Closeness (Proximidade): inverso da soma das distâncias do nó
#   a todos os outros nós da rede
# ─────────────────────────────────────────────────────────────────────────

# Visualizando as centralidades
centralityPlot(ega_resultado$network,
               include = c("Strength", "Betweenness", "Closeness"),
               orderBy = "Strength")


# --- 6.2 Extraindo os valores numéricos de centralidade ----------------------

# Força (strength) de cada item — o mais usado em psicometria de redes
forca <- centralidade$node.centrality$Strength
names(forca) <- names(dados_reduzidos)

# Ordenando do mais ao menos central
sort(forca, decreasing = TRUE)

cat("\nItem mais central (maior Strength):",
    names(which.max(forca)), "\n")
cat("Item menos central (menor Strength):",
    names(which.min(forca)), "\n")


# =============================================================================
#  BLOCO 7 – ANÁLISE POR PAÍS (Comparação Internacional)
#  [BLOCO OPCIONAL — para exploração adicional]
# =============================================================================

# Se quiser comparar a estrutura de rede entre países, é possível
# rodar a EGA separadamente para cada país e depois comparar.

# --- 7.1 Lista de países disponíveis -----------------------------------------

paises <- unique(dados_completos$country)
print(paises)

# --- 7.2 Função para rodar EGA por país (com tratamento de erros) ------------

rodar_ega_por_pais <- function(pais, dados_raw) {

  cat("\n========== País:", pais, "==========\n")

  # Filtrando
  d <- dados_raw |>
    filter(country == pais) |>
    select(starts_with("ts_")) |>    # ajuste o prefixo
    na.omit()

  cat("N =", nrow(d), "\n")

  # Verifica tamanho mínimo de amostra
  if (nrow(d) < 100) {
    cat("AVISO: amostra pequena (N < 100). Resultados podem ser instáveis.\n")
  }

  # UVA
  uva <- tryCatch(UVA(data = d), error = function(e) NULL)
  if (is.null(uva)) { cat("Erro na UVA.\n"); return(NULL) }

  # EGA
  ega <- tryCatch(
    EGA(data = uva$reduced_data, plot.EGA = FALSE),
    error = function(e) NULL
  )

  if (is.null(ega)) { cat("Erro na EGA.\n"); return(NULL) }

  cat("Dimensões encontradas:", ega$n.dim, "\n")
  return(ega)
}

# Exemplo: rodando para um segundo país (ex: USA)
# ega_usa <- rodar_ega_por_pais("USA", dados_completos)

# Para rodar para TODOS os países (pode demorar):
# resultados_paises <- lapply(paises, rodar_ega_por_pais, dados_raw = dados_completos)
# names(resultados_paises) <- paises


# =============================================================================
#  BLOCO 8 – REPORTANDO OS RESULTADOS
#  Como escrever sobre análise de redes em artigos científicos
# =============================================================================

# --- 8.1 Informações para o método -------------------------------------------

cat("\n\n===== INFORMAÇÕES PARA A SEÇÃO DE MÉTODO =====\n\n")

cat("Software e versão:\n")
cat("R versão:", as.character(getRversion()), "\n")
cat("EGAnet versão:", as.character(packageVersion("EGAnet")), "\n\n")

cat("Parâmetros da UVA:\n")
cat("  - Medida: weighted Topological Overlap (wTO)\n")
cat("  - Limiar de redundância: wTO > 0.25\n\n")

cat("Parâmetros da EGA:\n")
cat("  - Estimação da rede: GLASSO (Graphical LASSO)\n")
cat("  - Critério de seleção: EBIC (gamma = 0.5)\n")
cat("  - Algoritmo de comunidade: Walktrap\n\n")

cat("Parâmetros do bootEGA:\n")
cat("  - Tipo: Bootstrap Paramétrico\n")
cat("  - Iterações: 500\n")
cat("  - Critério de estabilidade: >= 0.70\n\n")


# --- 8.2 Tabela de resultados -------------------------------------------------

# Criando uma tabela resumindo dimensões e itens
tabela_dimensoes <- data.frame(
  Item     = names(ega_resultado$wc),
  Dimensao = as.integer(ega_resultado$wc)
)

tabela_dimensoes <- tabela_dimensoes |>
  arrange(Dimensao, Item)

print(tabela_dimensoes)

# Exportando a tabela para CSV
write.csv(tabela_dimensoes, "resultado_dimensoes_brasil.csv", row.names = FALSE)
cat("Tabela exportada para 'resultado_dimensoes_brasil.csv'\n")


# --- 8.3 Salvando os objetos R -----------------------------------------------

# Salva tudo em um único arquivo para não perder os resultados
save(uva_resultado, ega_resultado, boot_resultado,
     file = "resultados_ega_brasil.RData")
cat("Objetos salvos em 'resultados_ega_brasil.RData'\n")

# Para carregar futuramente:
# load("resultados_ega_brasil.RData")


# =============================================================================
#  BLOCO 9 – EXERCÍCIOS PROPOSTOS
# =============================================================================

# EXERCÍCIO 1 – Explorando os dados
# ─────────────────────────────────
# a) Quantos participantes brasileiros responderam à pesquisa?
# b) Quantos itens tem a escala de confiança nos cientistas?
# c) Existe algum item com mais de 10% de dados ausentes?
# d) Qual item tem a média mais alta? E o mais baixo?


# EXERCÍCIO 2 – UVA
# ─────────────────
# a) Houve algum par de itens redundante?
# b) Se sim, qual foi removido e por quê?
# c) O que aconteceria se você NÃO fizesse a UVA antes da EGA?


# EXERCÍCIO 3 – EGA
# ─────────────────
# a) Quantas dimensões a EGA identificou no Brasil?
# b) Elas fazem sentido teórico com o construto de confiança?
# c) Tente rodar a EGA com o algoritmo "louvain" em vez de "walktrap".
#    O número de dimensões mudou?


# EXERCÍCIO 4 – Estabilidade
# ──────────────────────────
# a) Com que frequência (%) o número de dimensões empírico foi recuperado
#    nas 500 amostras bootstrap?
# b) Existe algum item com estabilidade abaixo de 0.70?
#    Se sim, o que isso significa?
# c) A consistência estrutural de todas as dimensões ficou acima de 0.75?


# EXERCÍCIO 5 – Centralidade (desafio)
# ─────────────────────────────────────
# a) Qual item tem maior Strength na rede?
# b) O item mais central faz sentido para a teoria de confiança nos
#    cientistas? Por quê?
# c) Existe algum item com Betweenness muito maior que os outros?
#    O que isso pode indicar?


# EXERCÍCIO 6 – Comparação entre países (desafio avançado)
# ─────────────────────────────────────────────────────────
# a) Rode a EGA para um segundo país de sua escolha.
# b) O número de dimensões foi igual ao do Brasil?
# c) Os mesmos itens formam as mesmas dimensões nos dois países?
# d) O que diferenças na estrutura podem indicar sobre a validade
#    transcultural do instrumento?


# =============================================================================
#  FIM DO SCRIPT
# =============================================================================
#
#  Referências principais:
#
#  Golino, H. F., & Epskamp, S. (2017). Exploratory graph analysis:
#    A new approach for estimating the number of dimensions in
#    psychological research. PLoS ONE, 12(6), e0174035.
#
#  Christensen, A. P., & Golino, H. (2021). Estimating the stability
#    of psychological dimensions via bootstrap exploratory graph
#    analysis: A Monte Carlo simulation and tutorial.
#    Psych, 3(3), 479–500.
#
#  Christensen, A. P., Garrido, L. E., & Golino, H. (2023).
#    Unique variable analysis: A network psychometrics method to
#    detect local dependence. Multivariate Behavioral Research.
#
#  Almeida, I. & Pilati, R. Rethinking Trust in Scientists as a
#    Network Model: A Global Analysis and Implications for Science
#    Communication.
#
#  Documentação do EGAnet: https://r-ega.net
# =============================================================================
