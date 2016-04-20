## Cálculo de preferencias para la dieta de delfí mular


hkeStock <- read.table("hke.out/hke.LenN.out", comment.char=";")
head(hkeStock)
hkeStock$V8 <- as.numeric(as.character(substr(hkeStock$V5,4,6)))
hkeStock <- hkeStock[hkeStock$V8 %in% 9:49,]
hkeStock$V9 <- hkeStock$V6 * hkeStock$V7
quarter <- tapply(hkeStock$V9, INDEX=list(hkeStock$V1,hkeStock$V2), FUN=sum)
hakeStock <- data.frame(weightStock=apply(quarter,1,mean))
apply(hakeStock,2,mean) 
# Mean annual hake stock (in tn) 10234533 from 9 to 49cm
# Meand proportion of hake in bottlenose dolphins stomach 36.36383 (23.24895, 50.21732)


otherStock <- read.table("hke-ttr.data/ttr.other", comment.char=";")
quarter <- tapply(otherStock$V5, INDEX=list(otherStock$V1, otherStock$V2), FUN=mean)
otherfoodStock <- data.frame(weightStock=apply(quarter,1,mean))
apply(otherfoodStock,2,mean) 
# Mean annual otherfood stock (in tn) 2881649637 from common dolphin preylengths
# Meand proportion of gadidae in bottlenose dolphins stomach 53.76156 (40.70941, 68.62966)


# Regla de 3 para calcular preferencias:

# 10234533 * 1 -> 36.36383
# 2881649637 * x -> 53.76156

#(53.76156 * 10234533 * 1)/36.36383 = 2881649637 * x
x = ((53.76156 * 10234533 * 1)/36.36383)/2881649637

#Preferencia por la merluza 1 y por los gádidos x = 0.005250844
 






