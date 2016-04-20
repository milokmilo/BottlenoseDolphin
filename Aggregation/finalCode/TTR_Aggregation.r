################################################################################################## #
#                     AGGREGATION FILES FOR BOTLLENOSE DOLPHIN
#              Aggregation files for ages and length groups for Gadget
#             created: C.Saavedra (camilo.saavedra@vi.ieo.es) 18/04/2016
################################################################################################## #

# Gadget
# "../ttr.len1.agg"
# "../ttr.len5.agg"
# "../ttr.len20.agg"
# "../ttr.alllen.agg"
# "../ttr.age35.agg"
# "../ttr.age35_0-10.agg"
# "../ttr.age35_8-35.agg"
# "../ttr.allage35.agg"
#"../ttr.allage0-10.agg"
#"../ttr.allage8-35.agg"
# "../ttr.area.agg"


#### Length ####

lenIni <- 80
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
ageFin <- 35


age35 <- data.frame(age = paste("age", ageIni:ageFin, sep=""), 
                   value = ageIni:ageFin)

write.table(age35, "../ttr.age35.agg", 
            row.names = FALSE, sep = "\t", na=" ", quote = FALSE, col.names = FALSE)


age35_010 <- data.frame(age = paste("age", ageIni:10, sep=""), 
                    value = ageIni:10)

write.table(age35_010, "../ttr.age35_0-10.agg", 
            row.names = FALSE, sep = "\t", na=" ", quote = FALSE, col.names = FALSE)


age35_835 <- data.frame(age = paste("age", 8:ageFin, sep=""), 
                    value = 8:ageFin)

write.table(age35_835, "../ttr.age35_8-35.agg", 
            row.names = FALSE, sep = "\t", na=" ", quote = FALSE, col.names = FALSE)


allages35 <- matrix(c("allages", ageIni:ageFin), nrow = 1, ncol = length(c("allages", ageIni:ageFin)))

write.table(allages35, "../ttr.allage35.agg", 
            row.names = FALSE, sep = "\t", na  =" ", quote = FALSE, col.names = FALSE)


allages010 <- matrix(c("allages", ageIni:10), nrow = 1, ncol = length(c("allages", ageIni:10)))

write.table(allages010, "../ttr.allage0-10.agg", 
            row.names = FALSE, sep = "\t", na  =" ", quote = FALSE, col.names = FALSE)


allages835 <- matrix(c("allages", ageIni:35), nrow = 1, ncol = length(c("allages", ageIni:35)))

write.table(allages835, "../ttr.allage8-35.agg", 
            row.names = FALSE, sep = "\t", na  =" ", quote = FALSE, col.names = FALSE)



#### Area ####

area <- data.frame("area1", 1)

write.table(area, "../ttr.area.agg", 
            row.names = FALSE, sep = "\t", na  =" ", quote = FALSE, col.names = FALSE)





