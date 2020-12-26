#LO LOGREEEE PORFIN

setwd("C:/Users/DELL/Desktop/Data Science Curso/R_Programming/specdata/specdata")

pollutantmean <- function(id, pollutant = "nitrate"){
  files <- list.files()[id]
  dframe = data.frame()
  for (i in files) {
    data <- read.csv(i)[pollutant]
    NAs <- is.na(data)
    z <- data.frame(pollutant = data[!NAs])
    dframe <- rbind(z, dframe)
  }
  s <- dframe
  sum(s)/nrow(s)
}


complete <- function(id){
  dframe = data.frame()
  files <- list.files()[id]
  for (i in files) {
    data <- read.csv(i)
    cc <- sum(complete.cases(data))
    z <- data.frame(ID = i, nobs = cc)
    dframe <- rbind(dframe, z)
  }
  print(dframe)
} 

##Esta no salio  
corr <- function(threshold = 0){
  files <- list.files()
  dframe = data.frame()
  for (f in files) {
    read.csv(f)
    nit <- f["nitrate"]
    sulf <- f["sulfate"]
    nNAs <- is.na(nit)
    sNAs <- is.na(sulf)
    n <- as.numeric(nit[!nNAs])
    s <- as.numeric(sulf[!nNAs])
    cc <- cor(n[1:threshold], s[1:threshold])
    dframe <- rbind(dframe, cc)
  }
  print(dframe)
}

vec <- function(id, pollutant){
  data <- read.csv(id)[pollutant]
}

