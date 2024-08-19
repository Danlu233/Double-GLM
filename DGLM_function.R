DGLM_function = function(mean.formula, dispersion.pred, data, method = "gam") {
  
  epsilon = 1e-07
  maxit = 100
  
  iter = 0
  dev = 0
  devdiff = 1
  data$W_t = 1
  
  ini.model = glm(formula = mean.formula, family = quasipoisson(link = "log"), data = data)
  
  while (devdiff > epsilon && iter < maxit) {
    
    ED_glm = update(ini.model, weights = W_t, data = data)
    
    d = 2 * (data$rdas_any * log(data$rdas_any/ED_glm$fitted.values) - (data$rdas_any - ED_glm$fitted.values))
    
    data$d = d
    dispersion.formula = reformulate(dispersion.pred, response = "d")
    
    if (method == "gam") {
      d_glm = gam(dispersion.formula, family = Gamma(link = "log"), data=data, select = T, method = "REML")
    } else if (method == "glm"){
      d_glm = glm(dispersion.formula, family = Gamma(link = "log"), data=data)
    }
    
    var_y = d_glm$fitted.values * ED_glm$fitted.values
    
    d_gmu = 1/ED_glm$fitted.values
    
    data$W_t = 1/(var_y*(d_gmu)^2)
    
    dev_t = 2 * sum(data$rdas_any * log(data$rdas_any/ED_glm$fitted.values) - (data$rdas_any - ED_glm$fitted.values))
    
    devdiff = abs(dev_t - dev)
    
    dev = dev_t
    
    iter = iter + 1
    
  }
  
  output = list(ED_glm = ED_glm, d_glm = d_glm, dev = dev, iter = iter)
  
  return(output)
  
}
