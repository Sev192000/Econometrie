#ouverture des données
Yang2016 = read.csv("Yang2016.csv", header = T, sep = ",")
Yang2016
head(Yang2016)
names(Yang2016)
#est-ce que ça marche?
attach(Yang2016)

Yang2016_2 = data.frame(Firms,Date,as.numeric(as.character(CA_0)),as.numeric(as.character(CA_1)),
                        as.numeric(as.character(CA_2)),as.numeric(as.character(VCD_0)),
                        as.numeric(as.character(VCD_1)),as.numeric(as.character(VCD_2)),
                        as.numeric(as.character(KZ)),as.numeric(as.character(Oscore)),
                        as.numeric(as.character(size)),as.numeric(as.character(BM)),
                        as.numeric(as.character(CER64)),as.numeric(as.character(BHAR_1)),
                        as.numeric(as.character(BHAR_2)),as.numeric(as.character(BHAR_3)),
                        as.numeric(as.character(CER31)))
names(Yang2016_2) = c("Firms","Date","CA_0","CA_1","CA_2","VCD_0","VCD_1","VCD_2","KZ","Oscore","size","BM","CER64","BHAR_1","BHAR_2","BHAR_3","CER31")

detach(Yang2016)

attach(Yang2016_2)

DCA = c()
n = as.numeric(nrow(Yang2016_2))

for (i in (seq(0 : (n - 1)))){
    x = c(CA_0[i], CA_1[i], CA_2[i])
    y = c(VCD_0[i], VCD_1[i], VCD_2[i])
    reg1 = (lm(x ~ y))
    DCA = rbind(DCA, mean(residuals(reg1)))}

ABCA = DCA - mean(DCA)
Yang2016_3 = cbind(Yang2016_2, DCA, ABCA)

head(Yang2016_3[, c(1,18:19)])

detach(Yang2016_2)
