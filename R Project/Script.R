library("RevoScaleR")

# connStr <- "Driver=SQL Server;Server=localhost;Database=ohl_data;Uid=sa;Pwd=19Zeljo21" #sql auth

connStr <- "Driver=SQL Server;Server=localhost;Database=ohl_data;Trusted_Connection = Yes" # win auth

sqlShareDir <- paste("D:\\R_SerializationDir\\", Sys.getenv("USERNAME"), sep = "")
sqlWait <- TRUE
sqlConsoleOutput <- FALSE

cc <- RxInSqlServer(connectionString = connStr, shareDir = sqlShareDir, wait = sqlWait, consoleOutput = sqlConsoleOutput)
rxSetComputeContext(cc)

sql = "SELECT * FROM OHLData";

data = RxSqlServerData(sqlQuery = sql, connectionString = connStr, rowsPerRead = 5000)

info = rxGetVarInfo(data = data)
start.time <- proc.time()
rxSummary(~AidName, data = data)
used.time <- proc.time() - start.time
print(paste("It takes CPU Time=", round(used.time[1] + used.time[2], 2), " seconds, Elapsed Time=",
            round(used.time[3], 2), " seconds to summarize the data.", sep = ""))

dataset = rxImport(data);

# summary
summary(dataset)


# convert to numeric values
audiograms = lapply(dataset[, 9:22], function(x) as.numeric(as.character(x)));

nas = list()
for (i in 1:length(audiograms))
    nas[i] = sum(is.na(audiograms[[i]]))

# provjera uspješnosti konverzije
all(nas == 0)

dataset[, 9:22] = audiograms
summary(dataset[, 9:22])

numberOfMins = list()

for (i in 1:length(audiograms)) {
    numberOfMins[i] = sum(audiograms[[i]] == min(audiograms[[i]]))
}

names(numberOfMins) = dimnames(dataset[, 9:22])[[2]]