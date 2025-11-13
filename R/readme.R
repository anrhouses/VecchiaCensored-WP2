
#You can implement frequentist or bayesian, censored or not, with SVC or not or mix, with shared range or not, with wanted number of neig of vecchia
# all of it with only two functions, fit_moddel to estimate model parameters, and prediction to predict at new lccations,
# you can also obtain simulation and preidction of svc coeficien using the function prediction !
library("VecchiaCensored")
# laod test data
data("grille_GP") # load data for gaussian process estimation
data("test_GP") # load data for gaussian process prediction
data("grille_SVC") # load data for SVC estimation
data("test_GP") # load data for SVC prediction

# example of use

# 1 - GP
#### FREQUENTISTE

## no censuré

model_freq_no_cen=VecchiaCensored::fit_model(grille_GP$Z,locs_obs = grille_GP[,1:2],X_obs = grille_GP[,c("X1","X2")],M = 30,censored_indices = rep(0,length(grille_GP$Z)))
summary.fit_model(model_freq_no_cen) # print parameters estimation result

prediction=VecchiaCensored::prediction(model_freq_no_cen,locs_pred = test_GP[,1:2],X_pred = test_GP[,c("X1","X2")],M = 30) # perform prediction

plot(test_GP$Z,prediction$prediction) # comapre result to real data
cor(test_GP$Z,prediction$prediction)


## censuré
model_freq_cen=VecchiaCensored::fit_model(Y_obs = grille_GP$censored,locs_obs = grille_GP[,1:2],X_obs = grille_GP[,c("X1","X2")],M = 30,censored_indices = grille_GP$is_censored)
summary.fit_model(model_freq_cen)  # print parameters estimation result


prediction=VecchiaCensored::prediction(model_freq_cen,locs_pred = test_GP[,1:2],X_pred = test_GP[,c("X1","X2")],M = 30)# perform prediction
plot(test_GP$Z,prediction$prediction)
cor(test_GP$Z,prediction$prediction) # same result for 20% of censored data !


### BAYESIAN

## no censuré
model_bay_no_cen=VecchiaCensored::fit_model(grille_GP$Z,locs_obs = grille_GP[,1:2],X_obs = grille_GP[,c("X1","X2")],M = 10,censored_indices = rep(0,length(grille_GP$Z)),bayesian = TRUE,chains = 3,iter_warmup = 100,iter_sampling = 300,parallel_chains = 3)

summary.fit_model(model_bay_no_cen) # print mean posteriors and plot density plots of posteriors




prediction=VecchiaCensored::prediction(model_bay_no_cen,locs_pred = test_GP[,1:2],X_pred = test_GP[,c("X1","X2")],M = 30)
plot(test_GP$Z,prediction$prediction)
cor(test_GP$Z,prediction$prediction) # same result as frequentist

## censored

model_bay_cen=VecchiaCensored::fit_model(grille_GP$censored,locs_obs = grille_GP[,1:2],X_obs = grille_GP[,c("X1","X2")],M = 10,censored_indices = grille_GP$is_censored,bayesian = TRUE,chains = 3,iter_warmup = 100,iter_sampling = 300,parallel_chains = 3)

summary.fit_model(model_bay_no_cen)


prediction=VecchiaCensored::prediction(model_bay_cen,locs_pred = test_GP[,1:2],X_pred = test_GP[,c("X1","X2")],M = 30)
plot(test_GP$Z,prediction$prediction)
cor(test_GP$Z,prediction$prediction) # same result, even tho data is censored




# 2 - SVC


#### FREQUENTISTE
# using the package "varycoef" that implement no approximation

fit_vc=varycoef::SVC_mle(Z~X1+X2-1,data=grille_SVC,locs=(grille_SVC[,1:2]))

vc_pred=predict(fit_vc,newlocs = as.matrix(test_SVC[1:2]),newX =as.matrix(test_SVC[,c("X1","X2")]), newW = as.matrix(test_SVC[,c("X1","X2")]))



## no censuré, comparing to varycoef

model_freq_no_cen=VecchiaCensored::fit_model(grille_SVC$Z,locs_obs = grille_SVC[,1:2],X_obs = grille_SVC[,c("X1","X2")],M = 30,censored_indices = rep(0,length(grille_SVC$Z)),svc_indices = 1:2)
summary.fit_model(model_freq_no_cen) # print parameters estimation result

prediction=VecchiaCensored::prediction(model_freq_no_cen,locs_pred = test_SVC[,1:2],X_pred = test_SVC[,c("X1","X2")],M = 30) # perform prediction


plot(test_SVC$Z,vc_pred$y.pred) # poor result from varycoef, because model is more complex than simple GP
cor(test_SVC$Z,vc_pred$y.pred) # lost some accuracy

plot(test_SVC$Z,prediction$prediction) # comapre result to real data
cor(test_SVC$Z,prediction$prediction) # same result as varycoef, but faster !


#### Bayesian
# censored


model_bay_cen=VecchiaCensored::fit_model(grille_SVC$censored,locs_obs = grille_SVC[,1:2],X_obs = grille_SVC[,c("X1","X2")],M = 10,censored_indices = grille_SVC$is_censored,bayesian = TRUE,chains = 3,iter_warmup = 100,iter_sampling = 300,parallel_chains = 3,svc_indices = c(1,2))

summary.fit_model(model_bay_cen)

# prediction with simuations and pred of coefitions !
prediction=VecchiaCensored::prediction(model_bay_cen,locs_pred = test_SVC[,1:2],X_pred = test_SVC[,c("X1","X2")],M = 30,simulations = TRUE,pred_coef = TRUE)


# prediction of response variable
prediction$prediction
# prediction of coeficients
prediction$coef_prediction
# simulations
dim(prediction$simulations) # gives n_test * n_iterations simulations . n_iter = n_chain*iter_sampling. 3 chains of 300 iterations post warmup in this example





