wine <- read.csv("C:\\Users\\f28553145859\\Documents\\Miriam\\Datasets\\winequality-red.csv", sep=";")

library(ggplot2)
library(purrr)
library(rlang)

#verifica��o de dados missing na base
matrixplot(wine)
aggr(wine)

#comportamento da vari�vel qualidade
summary(wine$quality)
aggr(wine$quality)
hist(wine$quality)

#inclui uma coluna classificando os vinhos em "bom ou "ruim" 
wine <- wine %>%
  mutate(
    quality = as.character(quality),
    status = ifelse(quality <= 5, "bom", "ruim"))

#n�o parece haver opini�o predominante quando se trata de acidez vs. densidade
wine %>% 
  ggplot() +
  geom_point(aes(x = citric.acid, y = density, color = status)) +
  labs(x = "Acidez", y = "Desnidade") +
  scale_color_manual(values = c( "dark blue", "red"))

#analise bivariada 
boxplot(wine$fixed.acidity           ~ wine$status)
boxplot(wine$volatile.acidity        ~ wine$status)
boxplot(wine$citric.acid             ~ wine$status)
boxplot(wine$residual.sugar          ~ wine$status)
boxplot(wine$chlorides               ~ wine$status)
boxplot(wine$free.sulfur.dioxide     ~ wine$status)
boxplot(wine$total.sulfur.dioxide    ~ wine$status)
boxplot(wine$density                 ~ wine$status)
boxplot(wine$pH                      ~ wine$status)
boxplot(wine$sulphates               ~ wine$status)
boxplot(wine$alcohol                 ~ wine$status)

library(tidyverse)
library(tidymodels)

#argumento strata garante a mesma propor��o nos 2 conjuntos
wine_split <- initial_split(data=wine, strata = quality, prop = 0.8)
training_wine <- training(wine_split)
testing_wine <- testing(wine_split)

#valida��o cruzada, neste caso k=5 divide a base em 5 partes
folds <- vfold_cv(training_wine, v=5, strata= quality)

#pr� processamento
#queremos modelar a variavel quality em fun��o de todas as outras
wine_rec <- recipe(quality~., data=training_wine)

#especificar modelo
lr_mod <- logistic_reg() %>%
  set_engine("glm")

#workflow - encapusular tudo 
lr_workflow <- workflow() %>%
  add_recipe(wine_rec) %>%
  add_model(lr_mod)

#fit - indicando os dados brutos � indicado o modelo, treina cada modelo (5)
lr_fit <- lr_workflow %>%
  fit_resamples(folds,
                metrics= metric_set(accuracy, sens, spec),
                control= control_resamples(save_pred = TRUE))

#media das metricas dos modelos
collect_metrics(lr_fit)

#com FALSE, metricas de cada um deles
collect_metrics(lr_fit, summarize = FALSE)

final_fit <- lr_workflow %>%
  fit(training_wine)

predict(final_fit, testing_wine, type="prob")
  
