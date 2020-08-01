
library(proxy)

#Suppose you would like to use a SVM model with a radial basis function on some regression data. 
# Based on these tags, what other four models would constitute the most diverse set?

tag <- read.csv("data/tag_data.csv", row.names = 1)
tag <- as.matrix(tag)

## Select only models for regression
regModels <- tag[tag[,"Regression"] == 1,]

all <- 1:nrow(regModels)

## Seed the analysis with the SVM model
start <- grep("(svmRadial)", rownames(regModels), fixed = TRUE)
pool <- all[all != start]

## Select 4 model models by maximizing the Jaccard
## dissimilarity between sets of models
nextMods <- maxDissim(regModels[start,,drop = FALSE], 
                      regModels[pool, ], 
                      method = "Jaccard",
                      n = 4)

rownames(regModels)[c(start, nextMods)]
