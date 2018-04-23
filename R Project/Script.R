library("RevoScaleR")
library("ggplot2")
library("FactoMineR")
library("factoextra")
library("ellipse")
library("PerformanceAnalytics")
library("psych")
library("dplyr")
library("missMDA")

# uspostavljanje konekcije na bazu

# connStr <- "Driver=SQL Server;Server=localhost;Database=ohl_data;Uid=sa;Pwd=19Zeljo21" #sql auth

connStr <- "Driver=SQL Server;Server=localhost;Database=ohl_data;Trusted_Connection = Yes" # win auth

sqlShareDir <- paste("D:\\R_SerializationDir\\", Sys.getenv("USERNAME"), sep = "")
sqlWait <- TRUE
sqlConsoleOutput <- FALSE

cc <- RxInSqlServer(connectionString = connStr, shareDir = sqlShareDir, wait = sqlWait, consoleOutput = sqlConsoleOutput)
rxSetComputeContext(cc)

sql = "SELECT * FROM OHLData";

data = RxSqlServerData(sqlQuery = sql, connectionString = connStr, rowsPerRead = 5000)

#info = rxGetVarInfo(data = data)
#start.time <- proc.time()
#rxSummary(~AidName, data = data)
#used.time <- proc.time() - start.time
#print(paste("Vrijeme potrebno procesoru da sumarizira podatke: ", round(used.time[1] + used.time[2], 2), "s, ukupno vrijeme: ", round(used.time[3], 2), "s.", sep = "")) # na zadnjem pozivu mu je trebala minuta

dataset = rxImport(data);

#summary(dataset)

# pretvaranje vrijednosti audiograma iz karaktera u numerièke
audiograms = lapply(dataset[, 9:22], function(x) as.numeric(as.character(x)));
audiograms = data.frame(audiograms);

nas = list()
for (i in 1:length(audiograms))
    nas[i] = sum(is.na(audiograms[[i]]))

# provjera uspješnosti konverzije
all(nas == 0)

# dataset[, 9:22] = audiograms
summary(audiograms)

# EDA - frekvencija: minimuma, maksimuma, negativnih brojeva, brojeva iznad gornje granice
numberOfMins = lapply(audiograms, function(x) sum(x == min(x)));
numberOfMaximums = lapply(audiograms, function(x) sum(x == max(x)));
numberOfNegNumbers = lapply(audiograms, function(x) sum(x < 0));
numberOfBigNumbers = lapply(audiograms, function(x) sum(x > 100));

#numberOfMins = data.frame(numberOfMins)
#numberOfMaximums = data.frame(t(numberOfMaximums))
#numberOfNegNumbers = data.frame(t(numberOfNegNumbers))
#numberOfBigNumbers = data.frame(t(numberOfBigNumbers))

library(ggplot2)

# minimums

numberOfMins = data.frame(sapply(numberOfMins, c))
names(numberOfMins) = 'mins'
ggplot(numberOfMins, aes(row.names(numberOfMins), mins)) + geom_bar(stat = "identity", fill = "#581845") + labs(x = "", y = "", title = "Br. minimuma u svakoj od\nvarijabli audiograma", subtitle = "Br. vrijednosti: 1 114 966");

# maximums

numberOfMaximums = data.frame(sapply(numberOfMaximums, c))
names(numberOfMaximums) = 'maximums'
ggplot(numberOfMaximums, aes(row.names(numberOfMaximums), maximums)) + geom_bar(stat = "identity", fill = "#581845") + labs(x = "", y = "", title = "Br. maksimuma u svakoj od varijabli audiograma", subtitle = "Br. vrijednosti: 1 114 966");

# zanimljivo
summary(numberOfMaximums)
ggplot(numberOfMaximums, aes(row.names(numberOfMaximums), maximums)) + geom_bar(stat = "identity", fill = "#581845") + labs(x = "", y = "", title = "Br. maksimuma u svakoj od varijabli audiograma", subtitle = "Br. vrijednosti: 1 114 966") + scale_y_log10();

numberOfNegNumbers = data.frame(sapply(numberOfNegNumbers, c))
names(numberOfNegNumbers) = 'negNumbers'
ggplot(numberOfNegNumbers, aes(row.names(numberOfNegNumbers), negNumbers)) + geom_bar(stat = "identity", fill = "#581845") + labs(x = "", y = "", title = "Broj neg. vrijednosti u svakoj od varijabli audiograma", subtitle = "Br. vrijednosti: 1 114 966");

numberOfBigNumbers = data.frame(sapply(numberOfBigNumbers, c))
names(numberOfBigNumbers) = 'bigNumbers'
ggplot(numberOfBigNumbers, aes(row.names(numberOfBigNumbers), bigNumbers)) + geom_bar(stat = "identity", fill = "#581845") + labs(x = "", y = "", title = "Broj vrijednosti veæih od 100 u svakoj od varijabli audiograma", subtitle = "Br. vrijednosti: 1 114 966") + scale_y_log10();

numberOfBigNumbers = mutate(numberOfBigNumbers, bigNumbersPercentage = bigNumbers / 1114966 * 100)
row.names(numberOfBigNumbers) = row.names(numberOfMaximums)
ggplot(numberOfBigNumbers, aes(row.names(numberOfBigNumbers), bigNumbersPercentage)) + geom_bar(stat = "identity", fill = "#581845") + labs(x = "", y = "Postotak", title = "Postotak vrijednosti veæih od 100\nu svakoj od varijabli audiograma") + scale_y_continuous(labels = function(x) paste(x, "%", sep = ""))

correlation = cor(audiograms, use = "everything", method = "pearson")

lCorr = correlation[1:7, 1:7] # za lijevo uho
rCorr = correlation[8:14, 8:14] # za desno uho

colorfun <- colorRamp(c("red", "white", "blue"), space = "Lab")
ellipse::plotcorr(correlation[1:7, 1:7], col = rgb(colorfun((correlation[1:7, 1:7] + 1) / 2), maxColorValue = 255), mar = c(0.1, 0.1, 0.1, 0.1))

# testovi na normalnu distribuciju
# grafik gustoæe
p1 = ggplot(audiograms) +
    geom_density(aes(L500, colour = "L500"), adjust = 15) +
    geom_density(aes(L1k, colour = "L1k"), adjust = 15) +
    geom_density(aes(L2k, colour = "L2k"), adjust = 15) +
    geom_density(aes(L3k, colour = "L3k"), adjust = 15) +
    geom_density(aes(L4k, colour = "L4k"), adjust = 15) +
    geom_density(aes(L6k, colour = "L6k"), adjust = 15) +
    geom_density(aes(L8k, colour = "L8k"), adjust = 15) +
    labs(x = "Decibeli [Db]", y = "Gustoæa", colour = NULL, title = "Grafik gustoæe vrijednosti audiograma za lijevo uho") +
    xlim(-10, 50);

p1

# Q-Q graf
noOutliers = data.frame(lapply(audiograms, function(x) { sapply(x, function(y) { if (y > 500) y = NA else y }) }))

params1 = list(mean(noOutliers$L500, na.rm = TRUE), sd(noOutliers$L500, na.rm = TRUE))
params3 = list(mean(noOutliers$L1k, na.rm = TRUE), sd(noOutliers$L1k, na.rm = TRUE))
params4 = list(mean(noOutliers$L2k, na.rm = TRUE), sd(noOutliers$L2k, na.rm = TRUE))
params5 = list(mean(noOutliers$L3k, na.rm = TRUE), sd(noOutliers$L3k, na.rm = TRUE))
params6 = list(mean(noOutliers$L4k, na.rm = TRUE), sd(noOutliers$L4k, na.rm = TRUE))
params7 = list(mean(noOutliers$L6k, na.rm = TRUE), sd(noOutliers$L6k, na.rm = TRUE))
params8 = list(mean(noOutliers$L8k, na.rm = TRUE), sd(noOutliers$L8k, na.rm = TRUE))

p2 = ggplot(noOutliers) +
    stat_qq(aes(sample = L500, colour = "L500"), dparams = params1, na.rm = TRUE) +
    stat_qq(aes(sample = L1k, colour = "L1k"), dparams = params3, na.rm = TRUE) +
    stat_qq(aes(sample = L2k, colour = "L2k"), dparams = params4, na.rm = TRUE) +
    stat_qq(aes(sample = L3k, colour = "L3k"), dparams = params5, na.rm = TRUE) +
    stat_qq(aes(sample = L4k, colour = "L4k"), dparams = params6, na.rm = TRUE) +
    stat_qq(aes(sample = L6k, colour = "L6k"), dparams = params7, na.rm = TRUE) +
    stat_qq(aes(sample = L8k, colour = "L8k"), dparams = params8, na.rm = TRUE) +
    labs(x = "Gaussova distribucija", y = "Distribucija uzorka", colour = NULL, title = "Q-Q grafik vrijednosti za lijevo uho");
p2

# oblik svih Q-Q grafova podsjeæa na eksponencijalnu f-ju, što ukazuje na skewed data (http://data.library.virginia.edu/understanding-q-q-plots)
# i zbilja, na osnovu grafova gustoæe, može se vidjeti da je data skewed right
# mjere: Pearson's moment coefficient of skewness
#        Pearson's first skewness coefficient (mode skewness)
#        Pearson's second skewness coefficient (median skewness)
#        Groeneveld & Meeden’s coefficient
# skew: asimetrièan; nakrivo

# Shapiro-Wilk’s method
sampleForShapiro = sample(noOutliers$L500, 5000)
shapiro.test(sampleForShapiro) # W = 0.88022, p-value < 2.2e-16 < 0.05 -> distribucija uzorka se razlikuje od normalne

# izgled uzorka
ggplot(sampleForShapiroDF, aes(sample)) + geom_histogram() + labs(x = "Uzorak", y = NULL, title = "Distribucija uzorka varijable L500")

# zakljuèak - velièina uzorka možda ima uticaja, ali najveæi problem je asimetriènost podataka koji su right-skewed
# mjerenje asimetriènosti: Pearsonov alternativni koeficijent
sampleSkew = 3 * (mean(sampleForShapiro) - median(sampleForShapiro)) / sd(sampleForShapiro);
library("PerformanceAnalytics")
skewness(sampleForShapiro, TRUE, "moment")

# brisanje outliera rezultira blago negativnim skewom u uzorku, a blago pozitivnim u cijeloj varijabli; sample je blizak normalnoj distribuciji
noOutliers2L5 = sapply(noOutliers$L500, function(x) { if (is.na(x)) NA else if (x > 40) NA else x })
summary(noOutliers2L5)
skewness(noOutliers2L5, TRUE, "moment")

# zakljuèak 2 -> asimetriènost je uzrokovana outlierima
# negative kurtosis - platykurtic
kurtosis(noOutliers$L500, na.rm = TRUE, method = "sample")
kurtosis(noOutliers2L5, na.rm = TRUE, method = "sample")

# ponovljen Shapiro test sa uzorkom iz kojeg su "outlieri" odstranjeni
sampleForShapiro = sapply(sampleForShapiro, function(x) { if (x > 40) NA else x })
shapiro.test(sampleForShapiro) # W = 0.92838, p-value = 4.398e-05 - poveæan p-value, iako i dalje < 0.05

shapiro.test(sampleForShapiro[1:10]) # W = 0.84997, p-value = 0.05804 - ovo ilustrira osjetljivost testa na velièinu uzorka

ggplot(sampleForShapiroDF, aes(sample)) + geom_density(fill = "green", color = "green", alpha = 0.1) + labs(x = "Uzorak", y = "Gustoæa", title = "Grafik gustoæe uzorka iz varijable L500")

# PCA - priprema
# Bartlettov test sferiènosti
n <- nrow(audiograms[1:7])
p <- ncol(audiograms[1:7])
chi2 <- -(n - 1 - (2 * p + 5) / 6) * log(det(lCorr))
ddl <- p * (p - 1) / 2
print(chi2)
print(ddl)
print(pchisq(chi2, ddl, lower.tail = F)) # p-value za provjeru da li je Bartlettov test statistièki važan. U ovom sluèaju jeste, meðutim to je zbog velikog broja varijabli
# Some references advise to use this test only if the ratio ‘n:p’
# (number of instances divided by the number of variables) is lower than 5. http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/en_Tanagra_KMO_Bartlett.pdf

# KMO
KMO(lCorr) # 0.84 - meritorious

KMO(lCorr[1:6, 1:6]) # izbacivanje L8k varijable ne mijenja ništa

# determinanta matrice korelacija može ukazati na probleme sa multikolinearnošæu, iako ista ne smeta PCA
# osim toga, determinanta može ukazati na overral shape of data
det(correlation[1:7, 1:7]) # 0.001021131, > 0.00001
det(correlation[8:14, 8:14]) # 0.0009499547, > 0.00001

#PCA
pc1 <- principal(lCorr, nfactors = length(lCorr[1,]), rotate = "none") # na poèetku br faktora je jednak broju varijabli
plot(pc1$values, type = "b") # scree plot

# na osnovu eigenvrijednosti, kao i grafika, možemo odrediti da postoje 3 glavne komponente
# rezultati: ponovno pozivanje PCA pokazuje da èak i samo jedna komponenta može da objasni sve varijalbe sa uspješnošæu 0.98.
# teoretski, svakoj osobi bi se mogao dodijeliti "skor" te dodijeliti postavke na osnovu tog skora.
# meðutim, treba uzeti u obzir da dataset potièe od ljudi sa veæinski zdravim sluhom; par onih sa gubitkom se svelo na outliere.
# zbog toga, logièno da dataset prikazuje malu varijaciju

# 8k se toliko razlikuje da je dobilo vlastitu komponentu. (ko kad je veæina vrijednosti 999) EDIT / nije veæina,samo 28%
# TODO: ponoviti za odstranjenim outlierima

# krajnji zakljuèak: trenutni podaci bi se mogli podijeliti na niske, srednje i visoke frekvencije
# 500, 1k, 2k - niske,
# 3k, 4k - srednje,
# 6k, 8k - visoke

# buduæi da matrica korelacija komponenti nije nula, možemo zakljuèiti da su komponente ipak meðusobno povezane

# PCA, naèin br. 2...

na_count <- sapply(noOutliers, function(y) sum(length(which(is.na(y)))))
na_count = data.frame(na_count)

# kompletan dataset za analizu
data = dataset[1:length(dataset[[1]]), 9:28]
data[, 1:14] = audiograms

# izdvojeni aktivni
data.active = data[1:floor(length(data[[1]]) * 0.7), 1:14]

#1: sa outlierima
pca.res = PCA(data.active, graph = FALSE)
print(pca.res)
fviz_eig(pca.res, addlabels = TRUE) # eigenvrijednosti

#2: sa NA-ovima
data[, 1:14] = data.frame(lapply(data[, 1:14], function(x) { sapply(x, function(y) { if (y > 500) y = NA else y }) }))

#3: sa srednjima vrijednostima umjesto NA-ova
imputedOutliers = imputePCA(noOutliers)