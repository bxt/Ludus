# install.packages("zoo")
require("zoo")

mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}

# Load data:
names <- read.table("data/names.csv", header=TRUE)
hydphob <- t(read.csv("data/Hydrophobizitaeten.csv",sep=";",dec=","))
medprot <- read.csv("data/Durchschnittliche_Verteilung_der_Aminoaeuren.csv",sep=";",dec=",")
vtmlPjonesT <- read.csv("data/VTML-Jones.csv",sep=";",dec=",")
vtmlPjonesT <- data.frame(vtmlPjonesT ,row.names=1)
prots <- merge(medprot,names,by.x="Aminosäure",by.y="letter")
prots <- data.frame(prots,row.names=1)
prots <- cbind(prots,hydphob[match(rownames(prots),rownames(hydphob))])
prots <- cbind(prots,vtmlPjonesT[match(rownames(prots),rownames(vtmlPjonesT)),])
colnames(prots) <- c("freq","name","hydphob","pi","tau")
prots$freq <- prots$freq/100

"Proteine data:"
prots

# How to read fasta?
aqp4 <- "MSDRPTARRWGKCGPLCTRENIMVAFKGVWTQAFWKAVTAEFLAMLIFVLLSLGSTINWGGTEKPLPVDMVLISLCFGLSIATMVQCFGHISGGHINPAVTVAMVCTRKISIAKSVFYIAAQCLGAIIGAGILYLVTPPSVVGGLGVTMVHGNLTAGHGLLVELIITFQLVFTIFASCDSKRTDVTGSIALAIGFSVAIGHLFAINYTGASMNPARSFGPAVIMGNWENHWIYWVGPIIGAVLAGGLYEYVFCPDVEFKRRFKEAFSKAAQQTKGSYMEVEDNRSQVETDDLILKPGVVHVIDVDRGEEKKGKDQSGEVLSSV"
aqp4 <- unlist(strsplit(aqp4,split=""))
aqp4ann = prots[aqp4 ,]
aqp4hydphobs = aqp4ann$hydphob

wm <- weighted.mean(prots$hydphob, prots$freq)
"Mittlere Hydrophobizität im Durchschnittsprotein:"
wm 

"Standardabweichung der Hydrophobizitäten im Durchschnittsprotein:"
sqrt(sum(prots$freq*((prots$hydphob-wm)^2)))

"Mittlere Hydrophobizität im Aquaporin-4 Isoform a:"
mean(aqp4hydphobs)

"Standardabweichung der Hydrophobizitäten im Aquaporin-4 Isoform a:"
sd(aqp4hydphobs)

size <- 11
aqp4rollmeans = rollmean(aqp4hydphobs,size)
aqp4rollsds = rollapply(aqp4hydphobs,size,sd)
plot(aqp4rollmeans,type="l")
segments(1:length(aqp4rollmeans),aqp4rollmeans+aqp4rollsds,1:length(aqp4rollmeans),aqp4rollmeans-aqp4rollsds,col="gray")
lines(aqp4rollmeans)
# Uniprot transmembrane annotions at 37-57 65-85 116-136 156-176 185-205 232-252
# see http://www.ncbi.nlm.nih.gov/protein/4502181?report=graph
segments(c(37,65,116,156,185,232),0,c(57,85,136,176,205,252),0,col="blue")
# TMHMM results at 33-55  70-92  112-134  154-176  189-211  231-253
segments(c(33,70,112,154,189,231),0.1,c(55,92,134,176,211,253),0.1,col="red")
# Threshold
segments(0,1.9,length(aqp4rollmeans),1.9,col="green")

# sdeviations
dev.new()
plot(aqp4rollsds,type="l")
segments(0,2.05,length(aqp4rollmeans),2.05,col="green") # Threshold
# Applied threshold
aqp4rollsds > 2.05

# roll median
dev.new()
aqp4rollmedians = rollmedian(aqp4hydphobs,size)
plot(aqp4rollmedians,type="l")
segments(0,2,length(aqp4rollmeans),2,col="green") # Threshold
# Applied threshold
aqp4rollmedians > 2


"Hydrophobe bereiche Transmembranbereiche wohl bei 30-50  65-75  120-130  155-165  180-190  230-240  287-300"


dev.new()
logratio = log(prots$tau / prots$pi)
barplot(logratio),names=rownames(prots), main="Log Ratio (Jones tau / VTML pi)")

"Spearman-Korrelation zw. log ( Jones tau / VTML pi) zur Hydrophobizität der Aminosäuren"
cor(logratio, aqp4hydphobs , method="spearman")


dev.new()
aqp4ann$loc <- "X"
aqp4ann$loc[0:36] <- "D1"
aqp4ann$loc[37:57] <- "T1"
aqp4ann$loc[58:64] <- "D2"
aqp4ann$loc[65:85] <- "T2"
aqp4ann$loc[86:115] <- "D3"
aqp4ann$loc[116:136] <- "T3"
aqp4ann$loc[137:155] <- "D4"
aqp4ann$loc[156:176] <- "T4"
aqp4ann$loc[177:185] <- "D5"
aqp4ann$loc[185:205] <- "T5"
aqp4ann$loc[206:231] <- "D6"
aqp4ann$loc[232:252] <- "T6"
aqp4ann$loc[252:length(aqp4ann$loc)] <- "D7"

aqp4ann$loc = factor(aqp4ann$loc,c("D1","T1","D2","T2","D3","T3","D4","T4","D5","T5","D6","T6","D7"))

boxplot(hydphob ~ loc, aqp4ann, ylab = "Hydrophobicity")   
