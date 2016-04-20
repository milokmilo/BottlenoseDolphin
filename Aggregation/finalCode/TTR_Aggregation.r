################################################################################################## #
#                     AGGREGATION FILES FOR BOTLLENOSE DOLPHIN
#              Aggregation files for ages and length groups for Gadget
#             created: C.Saavedra (camilo.saavedra@vi.ieo.es) 18/04/2016
################################################################################################## #

# outputs
# "../ttr.len1.agg"
# "../ttr.len5.agg"
# "../ttr.len20.agg"
# "../ttr.alllen.agg"
# "../ttr.age.agg"
# "../ttr.allage.agg"
# "../ttr.area.agg"


#### Length ####

lenIni <- 100
lenFin <- 350

len1 <- data.frame(len = paste("len", lenIni:lenFin, sep=""), 
                 min = lenIni:lenFin,
                 max = seq(lenIni + 1, lenFin + 1))

write.table(len1, "../ttr.len1.agg", 
            row.names = FALSE, sep = "\t", na=" ", quote = FALSE, col.names = FALSE)


len5 <- data.frame(len = paste("len", seq(lenIni, lenFin, 5), sep=""), 
                   min = seq(lenIni, lenFin, 5),
                   max = seq(lenIni + 5, lenFin + 5, 5))

write.table(len1, "../ttr.len5.agg", 
            row.names = FALSE, sep = "\t", na=" ", quote = FALSE, col.names = FALSE)


len20 <- data.frame(len = paste("len", seq(lenIni, lenFin, 20), sep = ""), 
                   min = seq(lenIni, lenFin, 20),
                   max = seq(lenIni + 20, lenFin + 20, 20))

write.table(len20, "../ttr.len20.agg", 
            row.names = FALSE, sep = "\t", na = " ", quote = FALSE, col.names = FALSE)


alllengths <- data.frame("alllengths", lenIni, lenFin)

write.table(alllengths, "../ttr.alllen.agg", 
            row.names = FALSE, sep = "\t", na  =" ", quote = FALSE, col.names = FALSE)


#### Age ####

ageIni <- 0
ageFin <- 50


age <- data.frame(age = paste("age", ageIni:ageFin, sep=""), 
                   value = ageIni:ageFin)

write.table(age, "../ttr.age.agg", 
            row.names = FALSE, sep = "\t", na=" ", quote = FALSE, col.names = FALSE)


allages <- matrix(c("allages", ageIni:ageFin), nrow = 1, ncol = length(c("allages", ageIni:ageFin)))

write.table(allages, "../ttr.allage.agg", 
            row.names = FALSE, sep = "\t", na  =" ", quote = FALSE, col.names = FALSE)


#### Area ####

area <- data.frame("area1", 1)

write.table(area, "../ttr.area.agg", 
            row.names = FALSE, sep = "\t", na  =" ", quote = FALSE, col.names = FALSE)





