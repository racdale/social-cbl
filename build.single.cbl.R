library(tm)
library(ff)

setwd('~/Dropbox/social-cbl')

sents = readChar('sents.txt', file.info('sents.txt')$size)
sents = paste('. ',sents,sep='') # att utterance boundary in slot 1
sents = substr(sents,1,nchar(sents)-1) # get rid of trailing space
word.seq = unlist(strsplit(sents," "))
words = sort(unique(word.seq))

### reverse bigram transition for local chunks
btp.mat = matrix(0,nrow=length(words),ncol=length(words))
b.bis = data.frame(w1=word.seq[length(word.seq):2],w2=word.seq[(length(word.seq)-1):1],stringsAsFactors=F)
b.bis.cols = t(apply(b.bis,1,function (x) { 
  return(c(which(x[1]==words),which(x[2]==words))) 
}))
apply(b.bis.cols,1,function (x) { 
  btp.mat[x[1],x[2]]<<-btp.mat[x[1],x[2]]+1;return()
})
btp.mat = t(apply(btp.mat,1,function (x) {
  return(x/sum(x));
}))
big.chunks = which(btp.mat>mean(btp.mat[btp.mat>0]),arr.ind=T)
big.chunks = cbind(words[big.chunks[,2]],words[big.chunks[,1]])

### semanto-chunks through some k window size
semantico.func = function(words,word.seq,k) {
  cooc.mat = matrix(0,nrow=length(words),ncol=length(words))
  b.code = unlist(lapply(word.seq,function (x) {
    return(which(x==words))
  }))
  windows = embed(b.code,k)
  result = apply(windows,1,function (x) { 
    inds = expand.grid(x,x)
    inds = inds[inds[,1]!=inds[,2],]
    inds = arrayIndex2vectorIndex(as.matrix(inds),dim(cooc.mat)) #ff function, similar to MATLAB ind2sub
    cooc.mat[inds]<<-cooc.mat[inds]+1
    return()
  })
  cooc.mat = t(apply(cooc.mat,1,function (x) {
    return(x/sum(x));  
  }))
  semantico.groups = which(cooc.mat>(mean(cooc.mat[cooc.mat>0])),arr.ind=T)
  semantico.groups = cbind(words[semantico.groups[,2]],words[semantico.groups[,1]])
  semantico.class = setClass("semantico.class",slots=c(semantico.groups="matrix",cooc.mat="matrix"))
  return(semantico.class(semantico.groups=semantico.groups,cooc.mat=cooc.mat)) 
}
s = semantico.func(words,word.seq,k<-20)
plot(plot.semantico.groups(s@semantico.groups))

### shows k-window topic structure with a network
library(igraph)
plot.semantico.groups = function(semantico.groups) {
  edges = semantico.groups
  alphabetorder = function(x) { # alphabetize to uniqify
    s = edges[x,]
    edges[x,] <<- edges[x,order(s)]
    return()
  }
  result = sapply(1:dim(edges)[1],alphabetorder)
  edges = unique(edges)
  net = graph.data.frame(edges,directed=F)
  return(net)
}
plot(plot.semantico.groups(s@semantico.groups))

### monologue generator; condition chunks by rolling window!
# we need btp.mat and cooc.mat
ftp.mat = t(apply(btp.mat,2,function (x) {
  return(x/sum(x));  
}))
speak = c(1) # initialize
for (i in 1:20) { # roll without any topical constraints
  nexts = ftp.mat[speak[length(speak)],]
  nextsi = which(nexts>0)
  nextsv = nexts[nextsi]
  speak = c(speak,nextsi[which(runif(1)<cumsum(nextsv))[1]])  
}
speak = c(speak,1) # utterance boundary at the end

svdsol = svd(s@cooc.mat)
cooc.mat2 = (svdsol$u[,1:2] * svdsol$d[1:2]) %*% svdsol$v[1:2,]
#cooc.mat2[cooc.mat2<0]=0





speak = c(1,21,2,20,5,1,21,2,20,5,1,21) # initialize
context = c(1:length(words))*0
history = data.frame()
for (i in 1:5000) { # roll with topical constraints
  local.next = ftp.mat[speak[length(speak)],]  
  if (i>0) {
    cixs = aggregate(speak[(length(speak)-9):length(speak)]*0+1,by=list(speak[(length(speak)-9):length(speak)]),sum)
    context = colMeans(cooc.mat2[cixs$Group.1,])
    cosine.context = apply(cooc.mat2,1,function(x) {
      return(sum(context*x)/(sqrt(sum(x^2))*sqrt(sum(context^2))))
    })    
    } else {
    cosine.context = 1:22*0
  }
  nexts = local.next*(cosine.context)
  nexts[nexts<0] = 0
  nexts = nexts / sum(nexts)
  nextsi = which(nexts>0)
  nextsv = nexts[nextsi]
  history = rbind(history,c(speak[length(speak)],nexts))  
  speak = c(speak,nextsi[which(runif(1)<cumsum(nextsv))[1]])
  #context = context/2 # decay
  #context[speak[length(speak)]] = context[speak[length(speak)]] + 1
  #topic.decay = topic.decay/5
  #topic.decay[speak[length(speak)]] = topic.decay[speak[length(speak)]] - 1
}
speak = c(speak,1) # utterance boundary at the end
words[speak]

plot(svdsol$u[,1],svdsol$u[,2],col='white')
text(svdsol$u[,1]+runif(22)/100,svdsol$u[,2]+runif(22)/100,words)

s = semantico.func(words,words[speak],k<-10)
plot(plot.semantico.groups(s@semantico.groups))

entropy(s@cooc.mat[2,])


