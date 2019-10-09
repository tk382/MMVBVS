plot_gamma = function(result, title = ""){
  gamma = result$gamma
  nniter = nrow(gamma)
  colors <- c("lightblue", "darkblue")
  datgamma = as.data.frame(gamma[1:nniter, ])
  T = ncol(gamma)
  colnames(datgamma) = paste0('v',1:T)
  rownames(datgamma) = paste0("iter",1:nniter)
  datgamma$id = rownames(datgamma)
  datgamma$id = factor(datgamma$id, levels = datgamma$id)
  colnames(datgamma) = factor(colnames(datgamma), levels = colnames(datgamma))
  mdatgamma = reshape2::melt(datgamma, id = "id")
  colnames(mdatgamma)[3] = 'gamma'
  mdatgamma$gamma = as.factor(mdatgamma$gamma)
  ggplot(mdatgamma,
         aes(x = variable, y = id, fill = gamma)) +
    geom_tile(color = 'white') +
    scale_fill_discrete()+
    ylab("iterations")+
    xlab("gamma")+
    ggtitle(title)
}
plot_beta = function(result, title = ""){
    beta = result$beta
    T = ncol(beta)
    nniter = nrow(beta)
    datbeta = as.data.frame(beta[1:nniter,])
    colnames(datbeta) = paste0("v",1:T)
    rownames(datbeta) = paste0("iter",1:nniter)
    datbeta$id = rownames(datbeta)
    datbeta$id = factor(datbeta$id, levels=datbeta$id)
    datbeta2= reshape::melt(datbeta, id = "id")
    datbeta2$xx = rep(1:nniter, T)
    ggplot(datbeta2, aes(x=xx, y=value, col=variable))+
      geom_line(alpha=0.7)+
      ylab('beta')+
      xlab('iteration')+ggtitle(title)

}
beta_dist = function(result, title = ""){
  beta = result$beta[-1,]
  T = ncol(beta)
  nniter = nrow(beta)
  datbeta = as.data.frame(beta[1:nniter,])
  colnames(datbeta) = paste0("v",1:T)
  rownames(datbeta) = paste0("iter",1:nniter)
  datbeta$id = rownames(datbeta)
  datbeta$id = factor(datbeta$id, levels=datbeta$id)
  datbeta2= reshape::melt(datbeta, id = "id")
  datbeta2$xx = rep(1:nniter, T)
  ggplot(datbeta2, aes(x = variable, y = value)) +
    geom_violin()
}


plot_sigma = function(result, title = ""){
  Sigma = apply(result$Sigma,c(1,2),mean)
  T = nrow(Sigma)
  Sigma = as.data.frame(Sigma)
  colnames(Sigma) = paste0("v", 1:T)
  Sigma$id = paste0("v", 1:T)
  msig = reshape2::melt(Sigma, id = "id")
  msig$id = factor(msig$id, levels=paste0("v",1:10))
  msig$variable = factor(msig$variable, levels=paste0("v",1:10))
  ggplot(msig, aes(x = id, y = variable, fill = value)) +
    geom_tile() +
    scale_fill_gradient(limits=c(0,1.2)) +
    ylab("") + xlab("") +
    geom_text(aes(label = round(value, 2)))
}
