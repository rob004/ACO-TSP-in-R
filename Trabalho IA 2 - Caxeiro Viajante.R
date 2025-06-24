# Configurações do algoritmo
num_formigas <- 10    # Número de formigas
num_iteracoes <- 50  # Número de iterações
taxa_evaporacao <- 0.5  # Taxa de evaporação da trilha de feromônio
alfa <- 1              # Peso para o feromônio
beta <- 2              # Peso para a heurística (1/custo)

# Inicializar variáveis para rastrear a melhor solução
menor_custo <- Inf  # Inicializa como infinito
melhor_caminho <- NULL  # Melhor caminho ainda não definido


# Função para inicializar trilhas de feromônio
inicializar_feromonio <- function(grafo) {
  n <- nrow(grafo)
  feromonio <- matrix(1, n, n)  # Inicia com 1 em todas as arestas
  return(feromonio)
}

# Função para calcular a heurística (1/custo)
calcular_heuristica <- function(grafo) {
  heuristica <- ifelse(grafo > 0, 1 / grafo, 0)
  return(heuristica)
}

# Função para a construção de soluções (corrigida)
construir_solucao <- function(formiga, feromonio, heuristica, grafo) {
  n <- nrow(grafo)
  caminho <- numeric()
  cidade_atual <- sample(1:n, 1)  # Começa em uma cidade aleatória
  caminho <- c(caminho, cidade_atual)
  
  while (length(caminho) < n) {
    cidades_restantes <- setdiff(1:n, caminho)
    
    # Caso especial: apenas uma cidade restante
    if (length(cidades_restantes) == 1) {
      caminho <- c(caminho, cidades_restantes)
      break
    }
    
    probabilidade <- numeric(length(cidades_restantes))
    
    # Calcular as probabilidades para as cidades restantes
    for (i in seq_along(cidades_restantes)) {
      cidade <- cidades_restantes[i]
      probabilidade[i] <- (feromonio[cidade_atual, cidade]^alfa) *
        (heuristica[cidade_atual, cidade]^beta)
    }
    
    # Tratar casos em que as probabilidades somam zero
    if (sum(probabilidade) == 0) {
      probabilidade <- rep(1 / length(cidades_restantes), length(cidades_restantes))
    } else {
      # Normalizar para que a soma seja 1
      probabilidade <- probabilidade / sum(probabilidade)
    }
    
    # Garantir que não existem valores inválidos
    probabilidade[is.na(probabilidade)] <- 0
    probabilidade[is.infinite(probabilidade)] <- 0
    
    # Selecionar a próxima cidade com base nas probabilidades
    proxima_cidade <- sample(cidades_restantes, 1, prob = probabilidade)
    caminho <- c(caminho, proxima_cidade)
    cidade_atual <- proxima_cidade
  }
  
  return(caminho)
}



# Função para calcular o custo de uma solução
calcular_custo <- function(caminho, grafo) {
  custo <- 0
  for (i in 1:(length(caminho) - 1)) {
    custo <- custo + grafo[caminho[i], caminho[i + 1]]
  }
  custo <- custo + grafo[caminho[length(caminho)], caminho[1]]  # Volta à inicial
  return(custo)
}

# Função para atualizar feromônio
atualizar_feromonio <- function(feromonio, caminhos, custos, taxa_evaporacao) {
  n <- nrow(feromonio)
  nova_trilha <- matrix(0, n, n)
  
  for (i in 1:length(caminhos)) {
    caminho <- caminhos[[i]]
    custo <- custos[i]
    for (j in 1:(length(caminho) - 1)) {
      nova_trilha[caminho[j], caminho[j + 1]] <- nova_trilha[caminho[j], caminho[j + 1]] + (1 / custo)
    }
    nova_trilha[caminho[length(caminho)], caminho[1]] <- nova_trilha[caminho[length(caminho)], caminho[1]] + (1 / custo)
  }
  
  feromonio <- (1 - taxa_evaporacao) * feromonio + nova_trilha
  return(feromonio)
}

# Grafo 1: matriz de adjacência
grafo1 <- matrix(0, nrow = 12, ncol = 12)
arestas1 <- list(
  c(5, 1, 8.21), c(2, 5, 9.97), c(10, 8, 5.78), c(3, 1, 7.13), 
  c(9, 11, 4.34), c(5, 12, 2.5), c(3, 6, 3.53), c(5, 7, 3.76),
  c(9, 3, 7.42), c(9, 12, 1.34), c(9, 8, 3.04), c(6, 7, 9.16),
  c(10, 3, 3.04), c(7, 8, 1.3), c(11, 8, 3.29), c(10, 12, 10),
  c(12, 8, 5.15), c(1, 11, 9.48), c(2, 3, 8.21), c(8, 3, 7.03),
  c(6, 4, 6.44), c(11, 12, 2.29), c(4, 9, 4.19), c(5, 8, 7.08),
  c(1, 12, 8.66)
)
for (aresta in arestas1) {
  grafo1[aresta[1], aresta[2]] <- aresta[3]
  grafo1[aresta[2], aresta[1]] <- aresta[3]  # Não direcionado
}

# Configurações do algoritmo
num_formigas <- 20
num_iteracoes <- 100
taxa_evaporacao <- 0.5
alfa <- 1
beta <- 2

# Arestas do grafo 2
arestas2 <- list(
  c(16, 6, 9), c(14, 17, 4), c(12, 11, 1), c(10, 17, 5), 
  c(7, 12, 5), c(19, 4, 3), c(1, 17, 10), c(18, 19, 5),
  c(16, 12, 7), c(2, 5, 9), c(11, 5, 7), c(18, 16, 8),
  c(7, 6, 4), c(19, 14, 5), c(10, 18, 6), c(17, 2, 9),
  c(13, 7, 10), c(20, 9, 8), c(18, 9, 1), c(20, 4, 6),
  c(18, 20, 4), c(4, 10, 7), c(3, 2, 4), c(2, 6, 4),
  c(9, 14, 5), c(7, 10, 1), c(10, 13, 5), c(9, 3, 6),
  c(7, 5, 10), c(20, 19, 5), c(19, 13, 1), c(17, 13, 2),
  c(15, 14, 8), c(13, 10, 8), c(20, 14, 2), c(2, 17, 1),
  c(17, 18, 6), c(1, 12, 4), c(8, 12, 10), c(4, 15, 5),
  c(19, 10, 2), c(5, 14, 1), c(19, 16, 8), c(10, 14, 1),
  c(8, 18, 3), c(6, 13, 7), c(11, 15, 5), c(9, 19, 7),
  c(14, 8, 7), c(12, 8, 5), c(19, 8, 4), c(17, 8, 6),
  c(16, 8, 6), c(20, 3, 7), c(3, 11, 1), c(2, 1, 8),
  c(1, 15, 1), c(2, 12, 8), c(7, 11, 2), c(5, 1, 5),
  c(7, 2, 3), c(12, 2, 1), c(19, 18, 3), c(7, 15, 2),
  c(14, 5, 7), c(18, 5, 10), c(17, 11, 7), c(13, 16, 4),
  c(1, 5, 1), c(8, 6, 1), c(9, 7, 6), c(15, 12, 8),
  c(5, 4, 6), c(11, 4, 9), c(9, 20, 9), c(14, 19, 5),
  c(5, 17, 9), c(12, 7, 5), c(11, 9, 2), c(17, 1, 10),
  c(7, 18, 4), c(18, 6, 3), c(15, 10, 7), c(13, 19, 5),
  c(20, 5, 4), c(3, 5, 5), c(2, 7, 5), c(9, 13, 10),
  c(8, 3, 6), c(5, 10, 1), c(11, 3, 8), c(16, 1, 2),
  c(6, 20, 6), c(1, 19, 10), c(10, 20, 1), c(15, 9, 5),
  c(14, 3, 4), c(13, 9, 8), c(20, 16, 9), c(18, 11, 8),
  c(3, 15, 6), c(9, 8, 10), c(10, 15, 4), c(6, 2, 4),
  c(11, 14, 2), c(6, 12, 3), c(16, 14, 9), c(14, 9, 9),
  c(12, 9, 8), c(18, 1, 5), c(17, 15, 4), c(7, 20, 5),
  c(12, 20, 2), c(18, 12, 1), c(11, 12, 3), c(3, 10, 2),
  c(1, 14, 2), c(8, 10, 7), c(4, 13, 2), c(2, 13, 2),
  c(9, 11, 9), c(3, 16, 3), c(12, 17, 10), c(8, 16, 1),
  c(14, 11, 3), c(7, 14, 5), c(17, 10, 3), c(13, 15, 7),
  c(2, 20, 9), c(19, 9, 2), c(2, 3, 4), c(20, 12, 2),
  c(6, 5, 10), c(5, 3, 8), c(11, 7, 8), c(10, 5, 9),
  c(14, 16, 6), c(11, 8, 8), c(7, 13, 9), c(1, 18, 6),
  c(13, 18, 4), c(20, 6, 10), c(7, 9, 8), c(1, 7, 7),
  c(3, 4, 6), c(5, 9, 4), c(4, 7, 10), c(10, 11, 10),
  c(9, 1, 1), c(11, 2, 7), c(10, 6, 7), c(16, 2, 5),
  c(14, 13, 1), c(12, 5, 2), c(19, 15, 10), c(17, 3, 3),
  c(3, 14, 6), c(8, 2, 5), c(8, 14, 5), c(4, 9, 7),
  c(5, 12, 6), c(8, 20, 8), c(11, 1, 10), c(9, 17, 2),
  c(16, 15, 4), c(12, 10, 3), c(11, 18, 8), c(20, 13, 9),
  c(18, 13, 8), c(17, 19, 5), c(3, 13, 4), c(16, 20, 7),
  c(16, 11, 8), c(4, 14, 5), c(2, 10, 7), c(9, 10, 10),
  c(5, 15, 10), c(3, 19, 5), c(10, 1, 8), c(14, 20, 10),
  c(19, 20, 8), c(15, 17, 3), c(6, 19, 2), c(11, 17, 1),
  c(15, 2, 8), c(2, 15, 2), c(9, 5, 3), c(5, 2, 2),
  c(11, 6, 1), c(10, 2, 8)
)

# Criar grafo com mapeamento de índices sequenciais
todas_cidades <- unique(unlist(lapply(arestas2, function(x) x[1:2])))
mapa_cidades <- setNames(seq_along(todas_cidades), todas_cidades)

arestas2_normalizadas <- lapply(arestas2, function(aresta) {
  c(mapa_cidades[as.character(aresta[1])],
    mapa_cidades[as.character(aresta[2])],
    aresta[3])
})

num_cidades <- length(mapa_cidades)
grafo2 <- matrix(0, nrow = num_cidades, ncol = num_cidades)

for (aresta in arestas2_normalizadas) {
  grafo2[aresta[1], aresta[2]] <- aresta[3]
  grafo2[aresta[2], aresta[1]] <- aresta[3]  # Não direcionado
}

# Inicializar matriz de feromônio
feromonio <- matrix(1, nrow = num_cidades, ncol = num_cidades)

# Heurística inicial
heuristica <- ifelse(grafo2 > 0, 1 / grafo2, 0)



# Escolha o grafo a ser usado
grafo <- grafo2  # Alterar para grafo2  conforme necessário

# Execução do ACO
for (iteracao in 1:num_iteracoes) {
  caminhos <- list()
  custos <- numeric()
  
  for (formiga in 1:num_formigas) {
    # Tente construir uma solução para a formiga
    caminho <- construir_solucao(formiga, feromonio, heuristica, grafo)
    custo <- calcular_custo(caminho, grafo)
    caminhos[[formiga]] <- caminho
    custos[formiga] <- custo
  }
  
  # Atualizar a melhor solução encontrada
  for (i in 1:num_formigas) {
    if (custos[i] < menor_custo) {
      menor_custo <- custos[i]
      melhor_caminho <- caminhos[[i]]
    }
  }
  
  # Atualizar feromônio
  feromonio <- atualizar_feromonio(feromonio, caminhos, custos, taxa_evaporacao)
}

# Resultado final
cat("Melhor caminho encontrado:", melhor_caminho, "\n")
cat("Custo do melhor caminho:", menor_custo, "\n")
