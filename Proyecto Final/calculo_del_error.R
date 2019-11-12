library(Hmisc)
library(cluster.datasets)

infecReales = c(0.5967,
          0.5617,
          0.5363,
          0.5357,
          0.5323,
          0.5415,
          0.5558,
          0.5283,
          0.5411,
          0.5414,
          0.5442,
          0.5962,
          0.5461,
          0.5652,
          0.5651,
          0.5946,
          0.5558,
          0.5843,
          0.5789,
          0.6125,
          0.5962,
          0.6056,
          0.6124,
          0.6297,
          0.6035,
          0.6165,
          0.5799,
          0.5544,
          0.5747,
          0.5518,
          0.5933,
          0.5956,
          0.5787,
          0.5881,
          0.5267,
          0.545,
          0.528,
          0.4816,
          0.477,
          0.5065,
          0.5062,
          0.5058,
          0.5354,
          0.5349,
          0.54,
          0.56,
          0.57,
          0.557,
          0.5587,
          0.5633,
          0.5729,
          0.5871)

infecEuler = c(0.5967,
               0.5917667,
               0.5868951,
               0.5820846,
               0.5773349,
               0.5726455,
               0.5680159,
               0.5634456,
               0.558934,
               0.5544808,
               0.5500853,
               0.545747,
               0.5414654,
               0.53724,
               0.5330701,
               0.5289554,
               0.5248951,
               0.5208889,
               0.516936,
               0.5130359,
               0.5091881,
               0.5053921,
               0.5016471,
               0.4979528,
               0.4943084,
               0.4907135,
               0.4871674,
               0.4836697,
               0.4802196,
               0.4768168,
               0.4734605,
               0.4701503,
               0.4668855,
               0.4636657,
               0.4604902,
               0.4573585,
               0.4542701,
               0.4512244,
               0.4482209,
               0.445259,
               0.4423381,
               0.4394579,
               0.4366176,
               0.4338168,
               0.4310551,
               0.4283317,
               0.4256463,
               0.4229984,
               0.4203874,
               0.4178128,
               0.4152741,
               0.4127709)

infecAdams = c(0.9,
               0.8981551,
               0.8963105,
               0.8944662,
               0.8926223,
               0.8907789,
               0.8889361,
               0.8870939,
               0.8852525,
               0.8834118,
               0.881572,
               0.8797332,
               0.8778954,
               0.8760586,
               0.8742231,
               0.8723887,
               0.8705557,
               0.8687241,
               0.866894,
               0.8650654,
               0.8632384,
               0.861413,
               0.8595895,
               0.8577677,
               0.8559478,
               0.8541299,
               0.852314,
               0.8505003,
               0.8486887,
               0.8468793,
               0.8450722,
               0.8432675,
               0.8414653,
               0.8396655,
               0.8378683,
               0.8360738,
               0.8342819,
               0.8324928,
               0.8307066,
               0.8289232,
               0.8271428,
               0.8253654,
               0.823591,
               0.8218198,
               0.8200518,
               0.8182871,
               0.8165257,
               0.8147676,
               0.813013,
               0.8112618,
               0.8095142,
               0.8077702)

errRelativoAdams = c()
for (i in 1:length(infecReales))
  errRelativoAdams[i] <- abs(infecReales[i] - infecAdams[i])

errRelativoEuler = c()
for (i in 1:length(infecReales))
  errRelativoEuler[i] <- abs(infecReales[i] - infecEuler[i])

errAbsAdams = c()
for (i in 1:length(infecReales))
  errAbsAdams[i] <- (errRelativoAdams[i]/infecReales[i])*100

errAbsEuler = c()
for (i in 1:length(infecReales))
  errAbsEuler[i] <- (errRelativoEuler[i]/infecReales[i])*100

suma = 0
promRelativoAdams = c()
for (i in 1:length(errRelativoAdams)) {
  suma = suma + errRelativoAdams[i]
  promRelativoAdams[i] = 0
}
promRelativoAdams[1] = suma / length(errRelativoAdams)

suma = 0
promRelativoEuler = 0
for (i in 1:length(errRelativoEuler)) {
  suma = suma + errRelativoEuler[i]
  promRelativoEuler[i] = 0
}
promRelativoEuler[1] = suma / length(errRelativoEuler)

suma = 0
promAbsAdams = c()
for (i in 1:length(errAbsAdams)) {
  suma = suma + errAbsAdams[i]
  promAbsAdams[i] = 0
}
promAbsAdams[1] = suma / length(errAbsAdams)

suma = 0
promAbsEuler = 0
for (i in 1:length(errAbsEuler)) {
  suma = suma + errAbsEuler[i]
  promAbsEuler[i] = 0
}
promAbsEuler[1] = suma / length(errAbsEuler)

errores = cbind(errRelativoAdams,errRelativoEuler,errAbsAdams,errAbsEuler,
                promRelativoAdams,promRelativoEuler,
                promAbsAdams,promAbsEuler)
data(errores)
View(errores)