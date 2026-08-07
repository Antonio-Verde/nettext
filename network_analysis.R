library(readtext)  
library(quanteda) 
library(quanteda.textstats)  
library(igraph)
library(ggplot2)

pdf_corpus <- readtext::readtext("C:/Users/rcarl/OneDrive/Desktop/Esame NetText/PDF UFFICIALI")
corpus_crisi <- quanteda::corpus(pdf_corpus, docid_field = "doc_id", text_field = "text")

tokens_crisi <- tokens(corpus_crisi,
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_numbers = TRUE,
                       remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("it")) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(pattern = c("\\b\\w{1,2}\\b"))  

#DFM

dfm_crisi <- dfm(tokens_crisi)


dfm_crisi_binaria <- dfm_crisi
dfm_crisi_binaria[dfm_crisi_binaria > 0] <- 1

#Binarizzazione corretta

dfm_crisi_binaria <- dfm_weight(dfm_crisi, scheme = "boolean")

 
dfm_crisi_binaria_filtrata <- dfm_trim(dfm_crisi_binaria, min_docfreq = 2) #compaiono in almeno 2 documenti.

df_dtm_presenza_assenza <- convert(dfm_crisi_binaria_filtrata, to = "data.frame")

head(df_dtm_presenza_assenza)


#matrice di co-occorrenze parola-parola

matrice_binaria <- as.matrix(dfm_crisi_binaria_filtrata)

matrice_cooccorrenza <- t(matrice_binaria) %*% matrice_binaria


#grafo, non orientato, pesato

grafo_parole <- graph_from_adjacency_matrix(as.matrix(matrice_cooccorrenza),
                                            mode = "undirected",
                                            weighted = TRUE,
                                            diag = FALSE)


grafo_ridotto <- delete_edges(grafo_parole, E(grafo_parole)[weight < 150])

componenti <- components(grafo_ridotto)
componente_maggiore_id <- which.max(componenti$csize)
nodi_componente_maggiore <- V(grafo_ridotto)[componenti$membership == componente_maggiore_id]
grafo_componente_maggiore <- induced_subgraph(grafo_ridotto, nodi_componente_maggiore)
table(degree(grafo_ridotto)>0)

#GRAFO COMPONENTE MAGGIORE:

adj_mat_maggiore <- as_adjacency_matrix(grafo_componente_maggiore, attr = "weight", sparse = FALSE)

library(network)

grafo_network_maggiore <- network(adj_mat_maggiore, directed = FALSE, matrix.type = "adjacency")
degree_nodi_maggiore <- igraph::degree(grafo_componente_maggiore)
peso_archi_maggiore <- igraph::E(grafo_componente_maggiore)$weight / 300

palette_colori_magg <- colorRampPalette(c("lightblue", "yellow", "violet", "red"))(max(degree_nodi_maggiore) + 1)
colori_nodi_magg <- palette_colori_magg[degree_nodi_maggiore + 1]

library(GGally)

ggnet2(grafo_network_maggiore,
       size = sqrt(degree_nodi_maggiore),
       edge.size = peso_archi_maggiore,
       edge.color = "grey",
       color = colori_nodi_magg,
       label = TRUE,
       label.size = 4,
       label.color = "black",
       legend.size = FALSE,
       legend.position = "none") +
  ggtitle("Grafo parole - Componente principale")

vcount(grafo_componente_maggiore)
ecount(grafo_componente_maggiore)
ncol(dfm_crisi_binaria_filtrata)



# Betweenness centrality
betweenness_nodi <- igraph::betweenness(grafo_componente_maggiore, directed = FALSE)

# Closeness centrality
closeness_nodi <- igraph::closeness(grafo_componente_maggiore)

# Creo tabella con tutte le misure

tabella_centralita <- data.frame(
  parola = igraph::V(grafo_componente_maggiore)$name,
  degree = degree_nodi_maggiore,
  betweenness = betweenness_nodi,
  closeness = closeness_nodi
)

# Ordino per degree decrescente

tabella_centralita <- tabella_centralita[order(-tabella_centralita$degree), ]

# Visualizzo la tabella completa

View(tabella_centralita)


# max per ogni misura

max_degree <- tabella_centralita$parola[which.max(tabella_centralita$degree)]
max_betweenness <- tabella_centralita$parola[which.max(tabella_centralita$betweenness)]
max_closeness <- tabella_centralita$parola[which.max(tabella_centralita$closeness)]


top_n <- 20
tabella_top_degree <- head(tabella_centralita, top_n)

ggplot(tabella_top_degree, aes(x = reorder(parola, degree), y = degree)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Top 20 parole più centrali (degree)",
       x = "Parola",
       y = "Degree") +
  theme_minimal(base_size = 14)

tabella_betweenness <- data.frame(
  parola = igraph::V(grafo_componente_maggiore)$name,
  betweenness = betweenness_nodi
)

# Ordino per betweenness decrescente
tabella_betweenness <- tabella_betweenness[order(-tabella_betweenness$betweenness), ]



tabella_top_betweenness <- head(tabella_betweenness, top_n)

ggplot(tabella_top_betweenness, aes(x = reorder(parola, betweenness), y = betweenness)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Top 20 parole con Betweenness più alta",
       x = "Parola",
       y = "Betweenness") +
  theme_minimal(base_size = 14)




tabella_closeness <- data.frame(
  parola = igraph::V(grafo_componente_maggiore)$name,
  closeness = closeness_nodi
)

# Ordino per closeness decrescente
tabella_closeness <- tabella_closeness[order(-tabella_closeness$closeness), ]

# Seleziono le prime 20 parole con closeness più alta

tabella_top_closeness <- head(tabella_closeness, top_n)

# Plot con ggplot2
ggplot(tabella_top_closeness, aes(x = reorder(parola, closeness), y = closeness)) +
  geom_bar(stat = "identity", fill = "seagreen") +
  coord_flip() +
  labs(title = "Top 20 parole con Closeness più alta",
       x = "Parola",
       y = "Closeness") +
  theme_minimal(base_size = 14)

###CLIQUES
library(igraph)
cliques_list <- maximal.cliques(grafo_componente_maggiore)
length(cliques_list)

colors <- c("red", "blue", "green", "yellow", "purple", "orange", "cyan", "magenta", "brown")
colors <- rep(colors, length.out = length(cliques_list))


subgraphs <- lapply(cliques_list, function(cq) induced_subgraph(grafo_componente_maggiore, cq))


layouts <- lapply(subgraphs, layout_with_fr)


n_plots <- min(length(subgraphs), 9)  
n_col <- 3
n_row <- ceiling(n_plots / n_col)

par(mfrow = c(n_row, n_col), mar = rep(1,4))

for (i in seq_len(n_plots)) {
  sg <- subgraphs[[i]]
  vertex_colors <- rep(colors[i], vcount(sg))
  plot(sg,
       layout = layouts[[i]],
       vertex.color = vertex_colors,
       vertex.label = V(sg)$name,
       vertex.label.cex = 0.7,
       vertex.size = 15,
       main = paste("Clique", i))
}

clique_sizes <- sapply(cliques_list, length)


summary(clique_sizes)


which.max(clique_sizes)


V(grafo_componente_maggiore)[cliques_list[[which.max(clique_sizes)]]]$name

##COMMUNITY DETECTION##

# Community detection con Louvain
community_louvain <- igraph::cluster_louvain(grafo_componente_maggiore)

# Vettore dei cluster per ogni nodo
membership_louvain <- membership(community_louvain)

modularita_louvain <- igraph::modularity(community_louvain)

print(modularita_louvain)


length(unique(membership_louvain))


layout_fr <- igraph::layout_with_fr(grafo_componente_maggiore)


coord_df <- as.data.frame(layout_fr)
colnames(coord_df) <- c("x", "y")


palette_colori_community <- rainbow(length(unique(membership_louvain)))
colori_nodi_community <- palette_colori_community[membership_louvain]


ggnet2(grafo_network_maggiore,
       size = sqrt(degree_nodi_maggiore),
       edge.size = peso_archi_maggiore,
       edge.color = "grey",
       color = colori_nodi_community,
       label = TRUE,
       label.size = 4,
       label.color = "black",
       legend.size = FALSE,
       legend.position = "none",
       layout.par = coord_df) +
  ggtitle("Grafo parole - Componente principale - Community (Louvain) con Layout FR")



tabella_centralita$community <- membership_louvain

View(tabella_centralita)





parole_community <- data.frame(
  parola = igraph::V(grafo_componente_maggiore)$name,
  community = membership_louvain
)

parole_community_sorted <- parole_community[order(parole_community$community), ]

unique(parole_community_sorted$community)

View(parole_community_sorted)

##COMMUNITY DETECTION CON WALKTRAP##

# Walktrap
community_walktrap <- igraph::cluster_walktrap(grafo_componente_maggiore)

# Membership
membership_walktrap <- membership(community_walktrap)

# Modularità
modularita_walktrap <- igraph::modularity(community_walktrap)
print(modularita_walktrap)


tabella_centralita$community_walktrap <- membership_walktrap


num_community_walktrap <- length(unique(membership_walktrap))


layout_fr <- igraph::layout_with_fr(grafo_componente_maggiore)


coord_df <- as.data.frame(layout_fr)
colnames(coord_df) <- c("x", "y")


palette_colori_community <- rainbow(length(unique(membership_walktrap)))
colori_nodi_community <- palette_colori_community[membership_walktrap]


ggnet2(grafo_network_maggiore,
       size = sqrt(degree_nodi_maggiore),
       edge.size = peso_archi_maggiore,
       edge.color = "grey",
       color = colori_nodi_community,
       label = TRUE,
       label.size = 4,
       label.color = "black",
       legend.size = FALSE,
       legend.position = "none",
       layout.par = coord_df) +
  ggtitle("Grafo parole - Componente principale - Community (Louvain) con Layout FR")



parole_community_walktrap <- data.frame(
  parola = igraph::V(grafo_componente_maggiore)$name,
  community = membership_walktrap
)

parole_community_sorted_walktrap <- parole_community_walktrap[order(parole_community_walktrap$community), ]

unique(parole_community_sorted_walktrap$community)

View(parole_community_sorted_walktrap)





library(robin)

graph <- prepGraph(grafo_componente_maggiore, file.format = "igraph")


# Louvain vs Walktrap
set.seed(100)
compLW <- robinCompare(graph = graph,
                       method1 = "louvain",
                       method2 = "walktrap")  


plot(compLW)

set.seed(100)
compI  <- robinCompare(graph = graph, method1 = "walktrap", method2 = "infomap")

set.seed(100)
compLa <- robinCompare(graph = graph, method1 = "walktrap", method2 = "labelProp")

set.seed(100)
compF  <- robinCompare(graph = graph, method1 = "walktrap", method2 = "fastGreedy")


plotMultiCompare(compI, compLa, compLW, compF)

library(networkD3)

igraph::V(grafo_componente_maggiore)$community <- membership_walktrap  

graph_d3 <- igraph_to_networkD3(grafo_componente_maggiore, group = V(grafo_componente_maggiore)$community)

forceNetwork(Links = graph_d3$links,
             Nodes = graph_d3$nodes,
             Source = 'source',
             Target = 'target',
             NodeID = 'name',
             Group = 'group',
             opacity = 1,
             linkDistance = JS("function(d) { return 30; }"),
             charge = JS("function(d) { return -100; }"),
             fontSize = 16,
             fontFamily = "Arial")





---
  library(dplyr)

community_sizes_walktrap <- parole_community_walktrap %>%
  group_by(community) %>%
  summarise(size = n()) %>%
  arrange(desc(size))

library(ggplot2)

ggplot(community_sizes_walktrap, aes(x = reorder(factor(community), size), y = size, fill = factor(community))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Comunità per Dimensione (Walktrap)",
       x = "Community",
       y = "Numero di Parole") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")



##MULTILAYER

library(readtext)
library(quanteda)
library(quanteda.textstats)
library(igraph)
library(multinet)

pdf_ft <- readtext("C:/Users/rcarl/OneDrive/Desktop/Esame NetText/PDF_FT")
pdf_ws <- readtext("C:/Users/rcarl/OneDrive/Desktop/Esame NetText/PDF_WSJ")

corpus_ft <- corpus(pdf_ft, docid_field = "doc_id", text_field = "text")
corpus_ws <- corpus(pdf_ws, docid_field = "doc_id", text_field = "text")

processa_corpus <- function(corp) {
  tokens(corp,
         remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("en")) %>%
    tokens_remove(pattern = "\\b\\w{1,2}\\b")
}

tokens_ft <- processa_corpus(corpus_ft)
tokens_ws <- processa_corpus(corpus_ws)


top_ft <- names(topfeatures(dfm(tokens_ft), 20))
top_ws <- names(topfeatures(dfm(tokens_ws), 20))

##CO-OCCORRENZA
crea_cooc_top <- function(tokens, top_words) {
  dfm_tok <- dfm(tokens)
  dfm_top <- dfm_keep(dfm_tok, pattern = top_words)
  fcm(dfm_top, context = "document", tri = FALSE)
}

cooc_ft <- crea_cooc_top(tokens_ft, top_ft)
cooc_ws <- crea_cooc_top(tokens_ws, top_ws)

#adj
graph_ft <- graph_from_adjacency_matrix(as.matrix(cooc_ft), mode = "undirected", weighted = TRUE, diag = FALSE)
graph_ws <- graph_from_adjacency_matrix(as.matrix(cooc_ws), mode = "undirected", weighted = TRUE, diag = FALSE)


V(graph_ft)$name <- paste0(V(graph_ft)$name, "_FT")
V(graph_ws)$name <- paste0(V(graph_ws)$name, "_WS")
V(graph_ft)$layer <- "FT"
V(graph_ws)$layer <- "WS"


g_all <- igraph::union(graph_ft, graph_ws, byname = TRUE)


nodi_ft <- V(graph_ft)$name
nodi_ws <- V(graph_ws)$name
parole_ft <- gsub("_FT$", "", nodi_ft)
parole_ws <- gsub("_WS$", "", nodi_ws)
comuni <- intersect(parole_ft, parole_ws)

inter_edges <- data.frame(
  from = paste0(comuni, "_FT"),
  to   = paste0(comuni, "_WS")
)


g_all <- add_edges(g_all, t(as.matrix(inter_edges)), attr = list(weight = 1, interlayer = TRUE))


E(g_all)$weight[is.na(E(g_all)$weight)] <- 1



com <- cluster_louvain(g_all)
V(g_all)$community <- membership(com)
colors <- rainbow(length(unique(V(g_all)$community)))
vertex_col <- colors[V(g_all)$community]



library(RColorBrewer)


layout_fr <- layout_with_fr(g_all)

colors <- brewer.pal(length(unique(V(g_all)$community)), "Dark2")
vertex_col <- colors[V(g_all)$community]

edge_col <- ifelse(is.na(E(g_all)$interlayer), "gray80",
                   ifelse(E(g_all)$interlayer, adjustcolor("violet", alpha.f = 0.7), "gray80"))

vertex_size <- 9            
label_size  <- 0.7         
label_font  <- 2            

#PLOT

plot(
  g_all,
  layout = layout_fr,
  vertex.color = vertex_col,
  vertex.size = vertex_size,
  vertex.label = V(g_all)$name,
  vertex.label.cex = label_size,
  vertex.label.color = "black",
  vertex.label.font = label_font,
  edge.color = edge_col,
  edge.width = ifelse(!is.na(E(g_all)$interlayer) & E(g_all)$interlayer, 2.2, 0.4),
  main = "Multilayer – Top 20 FT vs WS"
)


