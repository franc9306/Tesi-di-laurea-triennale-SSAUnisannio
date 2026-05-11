library(FactoMineR)
library(factoextra)
library('corrr')
library(ggcorrplot)
library(corrplot)
dataset <- read.csv2("myfile.csv",header = TRUE , sep=";")
boxplot(scale(dataset[2:5]),
        main="Boxplot Indicatori del Grado di soddisfazione",
        col = c("#FFE0B2", "#64AAD2", "#F57C00","#FFF300",))

datasetnew <- dataset[,-1]
rownames(datasetnew) <- dataset[,1]
#Selezioniamo solo le variabili attive 
numerical_data <- datasetnew [1:36,1:14]
#normalizziamo i dati 
data_normalized <- scale(numerical_data)
head(data_normalized)
#costruisco la matrice delle correlazioni dai dati standardizzati
corr_matrix <- cor(data_normalized)
corr_matrix
ggcorrplot(corr_matrix)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           outline.col = "white")
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)
p.mat <- cor_pmat(numerical_data)
ggcorrplot(corr_matrix, hc.order = TRUE,
           type = "lower", p.mat= p.mat)
#applichiamo la pca 
data.pca <- princomp(corr_matrix)
# Summary della pca 
summary(data.pca)

risultati.pca <-PCA(numerical_data, graph = FALSE)
print(risultati.pca)

eig.val <- get_eigenvalue(risultati.pca)
print(eig.val)

data.pca$loadings[, 1:3]

fviz_eig(risultati.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(risultati.pca)

var
#coordinate
head(var$coord)
#coseni
head(var$cos2)
# Contributi componenti principali 
head(var$contrib)

#cerchio delle correlazioni
fviz_pca_var(risultati.pca, col.var = "black")

fviz_pca_var(data.pca, col.var = "black")

fviz_cos2(risultati.pca, choice = "var", axes = 1:2)

fviz_pca_var(risultati.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) # Avoid text overlapping

fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
fviz_contrib(risultati.pca, choice = "var", axes = 1, top = 60)
fviz_contrib(risultati.pca, choice = "var", axes = 2, top = 10)

fviz_pca_ind(risultati.pca)


set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)

# Color variables by groups
fviz_pca_var(risultati.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

#variabili supplementari 
res.desc <- dimdesc(risultati.pca, axes = c(1,2), proba = 0.05)
res.desc$Dim.1
ind <- get_pca_ind(risultati.pca)
ind
fviz_pca_ind(risultati.pca)
res.pca_2 <- PCA(datasetnew[1:24], quanti.sup = 15:24, graph=FALSE)
res.pca_2$quanti.sup
fviz_pca_var(res.pca_2)
fviz_pca_var(res.pca_2,
             col.var = "black",     # variabili attive
             col.quanti.sup = "red" # variabili Supplementari 
)
fviz_pca_var(res.pca_2, invisible = "var")

p_2 <- fviz_pca_ind(res.pca_2, col.ind.sup = "blue", repel = TRUE)
p_2 <- fviz_add(p_2, res.pca_2$quanti.sup$coord, color = "red")
p_2

pca <- prcomp(numerical_data,  scale = TRUE)

pca$rotation

head(pca$x)




res.pca <- PCA(datasetnew,  graph = FALSE)
res.pca$eig
str(dataset)



fviz_screeplot(res.pca, addlabels = TRUE)
res.pca$ind$coord
fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_biplot(res.pca, repel = TRUE)
fviz_pca_biplot(res.pca,
                label="var",
                habillage = dataset$classe.di.laurea)
fviz_pca_biplot(res.pca,
                label = "var",
                col.ind = "cos2",
                col.var = "black",
                gradient.cols = c("blue","green","red"))



#Biplot con Ellissi dei singoli gruppi STEM
res.pca<-PCA(datasetnew[-25],graph=FALSE)


fviz_pca_biplot(res.pca,
                col.ind=dataset$gruppi.stem,
                addEllipses = TRUE, label ="var",
                col.var = "black", repel = TRUE,
                legend.title = "Gruppi STEM")
