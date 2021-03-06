dfsEnv <- globalenv()
dfsEnv$adj <- c("1,2","2,1","2,3","3,2","2,4","4,2","4,33",
                "33,4","4,5","5,4","5,14","14,5","5,13",
                "13,5","4,7","7,4","3,6","6,3","6,7",
                "7,6","6,9","9,6","8,9","9,8","9,10",
                "10,9","10,11","11,10","7,11","11,7","11,12",
                "12,11","12,13","13,12","14,15","15,14","15,18",
                "18,15","15,24","24,15","24,12","12,24","12,16",
                "16,12","17,16","16,17","17,18","18,17","10,17",
                "17,10","9,20","20,9","20,18","18,20","18,19",
                "19,18","19,16","16,19","19,23","23,19","24,23",
                "23,24","20,21","21,20","21,22","22,21","22,23",
                "23,22","24,29","29,24","24,25","25,24","25,26",
                "26,25","14,26","26,14","26,27","27,26","27,28",
                "28,27","25,28","28,25","28,29","29,28","29,30",
                "30,29","30,31","31,30","31,22","22,31","21,31",
                "31,21","30,34","34,30","34,30","30,34","30,32","32,30")
dfsEnv$HashSetAdj <- c("7,6", "15,24", "18,19", "20,18", "9,6", "9,8", "10,11", "18,15",
                       "14,15", "18,17", "19,23", "9,20", "31,30", "29,28", "25,24",
                       "24,12", "30,29", "25,26", "24,15", "25,28", "21,22", "10,17",
                       "21,20", "29,24", "22,31", "13,12", "17,18", "17,16", "14,26",
                       "31,21", "17,10", "24,29", "23,19", "31,22", "7,11", "32,30",
                       "18,20", "28,27", "24,23", "28,25", "24,25", "2,1", "28,29",
                       "2,3", "29,30", "2,4", "4,2", "20,21", "13,5", "11,7", "21,31",
                       "6,3", "4,5", "4,7", "4,33", "6,7", "12,11", "6,9", "16,12",
                       "12,13", "16,19", "8,9", "16,17", "27,26", "26,14", "23,22",
                       "23,24", "27,28", "12,16", "20,9", "15,14", "5,13", "5,14",
                       "11,10", "11,12", "15,18", "12,24", "19,16", "19,18", "9,10",
                       "30,31", "30,32", "34,30", "26,25", "26,27", "30,34", "1,2",
                       "3,2", "10,9", "22,23", "14,5", "22,21", "3,6", "5,4", "7,4", "33,4")
dfsEnv$srt <- sort(x = dfsEnv$adj, decreasing = FALSE)
dfsEnv$splt <- strsplit(x = dfsEnv$HashSetAdj, split = ",")
dfsEnv$vis <- hash::hash()
street_nodes <- 34
for (i in 1:street_nodes) {
  hash::.set(dfsEnv$vis, keys=i, values=FALSE)
}
dfsEnv$path <- c()
dfsEnv$allPaths <- list()

dfs <- function (loc, des) {
  result <- c()

  if (loc == des) {
    pathSize = length(dfsEnv$path)
    for (i in 0:pathSize) {
      result <- append(x = result, values = dfsEnv$path[i])
    }
    result <- append(x = result, values = des)

    lastVectorOfAll <- (length(dfsEnv$allPaths) + 1)
    dfsEnv$allPaths[[lastVectorOfAll]] <- result
    return()
  }

  if (hash::values(dfsEnv$vis, loc)) {
    return()
  }

  hash::values(dfsEnv$vis, keys=loc) <- TRUE

  for (i in 1:length(dfsEnv$splt)) {
    first <- dfsEnv$splt[[i]][1]
    second <- dfsEnv$splt[[i]][2]
    if (first == loc) {
      dfsEnv$path <- append(x = dfsEnv$path, values = first)
      dfs(second, des)
      # lastElem <- length(dfsEnv$path)
      # dfsEnv$path[[lastElem]] <- NULL
      dfsEnv$path <- head(dfsEnv$path, -1)
    }
  }
}

dfs("1", "3")

print(dfsEnv$allPaths)
