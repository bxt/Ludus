# install.packages("seqinr")
require("seqinr")

dna2rnaTable <- read.csv("data/DNA_codogen_-_RNA.csv",sep=";",dec=",")
rna2acdTable <- read.csv("data/RNA_-_Aminosaeure.csv",sep=";",dec=",", fileEncoding="ISO-8859-1")
rna2acdTable <- data.frame(rna2acdTable,row.names=1)


dna2rna <- function(dna) {
  dna2rnaTable[toupper(dna)]
}

concat <- function(...) do.call(paste, c(...,list(sep="")))

rna2amins <- function(rna) {
  v <- toupper(rna)
  codons <- tapply( v, (seq_along(v)-1) %/% 3, concat)
  rna2acdTable[codons,]
}

dna2amins <- function(dna) rna2amins(t(dna2rna(dna))[,1])

seq1 <- read.fasta("data/ABi_Solid_Seq1.fasta")
seq2 <- read.fasta("data/ABi_Solid_Seq2.fasta")
ebola <- read.fasta("data/ebola.fasta")

dna2rna(seq1$ABi_Solid_Seq1)
rna2amins(c("A","A","A","G","A","G","C","U","C","C","U","U","G","A","A","C","G","A"))
dna2amins(c("T","A","C","T","C","G","C","T","G"))


dna2rna(rev(c("A","A","C","C","A","C","G","A","C","T","A","C")))
dna2amins(rev(c("A","A","C","C","A","C","G","A","C","T","A","C")))





constant <- function(c) function(...) c

a <- c("A","E","H","R","E","N")
b <- c("M","A","E","H","T","E")
b <- c(" "," "," "," ","M","A","E","H","T","E"," "," "," "," "," ")

a <- seq1$ABi_Solid_Seq1
a <- seq2$ABi_Solid_Seq2
b <- ebola$`gb|AF086833.2|:14060-17752`

semiglobalAlginment <- function(a, b, gap=-1, match=3, missmatch=-1) {
  
  I <- outer(a,b,function(a1,a2) ifelse(a1==a2,match,missmatch))
  
  M <- matrix(0,length(a)+1,length(b)+1)
  
  for (i in 1:length(a)+1) {
    M[i,1] = (M[i-1,1] + gap)
  }
  
  for (i in 1:length(a)+1) {
    for (j in 1:length(b)+1) {
      M[i,j] = max(M[i-1,j-1]+I[i-1,j-1], M[i-1,j]+gap, M[i,j-1]+gap)
    }
  }
  
  lastLine = M[length(a),]
  print(backtracking(a,b,M,startJ=which(lastLine==max(lastLine))[1]),missmatch=missmatch)
  max(lastLine)
}

backtracking <- function (a,b,M,startJ=NA,missmatch=-1) {
  h = dim(M)[1]
  w = dim(M)[2]
  if(is.na(startJ)) startJ = w 
  i = h
  j = startJ
  aS = c()
  bS = c()
  while(i >1 && j > 1) {
    m = max(M[i-1,j-1], M[i-1,j], M[i,j-1])
    if ((M[i,j]-M[i,j-1]) == missmatch && m == M[i,j-1]) {
      j<-j-1
      aS = c("-",aS)
      bS = c(b[j],bS)
    } else if (m == M[i-1,j-1]) {
      i<-i-1
      j<-j-1
      aS = c(a[i],aS)
      bS = c(b[j],bS)
    } else if (m == M[i,j-1]) {
       j<-j-1
       aS = c("-",aS)
       bS = c(b[j],bS)
    } else { # m == M[i-1,j]
      i<-i-1
      aS = c(a[i],aS)
      bS = c("-",bS)
    }
  }
  while(i >1) {
    i<-i-1
    aS = c(a[i],aS)
    bS = c("-",bS)
  }
  while(j > 1) {
    j<-j-1
    aS = c("-",aS)
    bS = c(b[j],bS)
  }
  res <- matrix(c(aS,bS),length(aS),2)
  res
}

semiglobalAlginment(a,b)

globalAlginment <- function(a, b, gap=-1, match=3, missmatch=-1) {
  
  I <- outer(a,b,function(a1,a2) ifelse(a1==a2,match,missmatch))
  
  M <- matrix(0,length(a)+1,length(b)+1)
  
  for (i in 1:length(a)+1) {
    M[i,1] = (M[i-1,1] + gap)
  }
  for (j in 1:length(b)+1) {
    M[1,j] = (M[1,j-1] + gap)
  }
  
  for (i in 1:length(a)+1) {
    for (j in 1:length(b)+1) {
      M[i,j] = max(M[i-1,j-1]+I[i-1,j-1], M[i-1,j]+gap, M[i,j-1]+gap)
    }
  }
  
  print(backtracking(a,b,M))
  M[length(a)+1,length(b)+1]
}
#globalAlginment(a,b)

