# Estimating VAR ----------------------------------------------------------
#Reproduction of Gertler & Karadi (2015)

library(openxlsx)
library(vars)
library(MASS)
library(strucchange)
library(lmtest)
library(urca)
library(sandwich)

# Read directly after downloading
url <- "https://raw.githubusercontent.com/ErnesztKazmin/ivVar/accdca0e00cbc46661dc144375ad0a1cf8657a1e/Replication/GK2015_Data.xlsx"
download.file(url, destfile = "GK2015.xlsx", mode = "wb")
data <- read.xlsx("GK2015.xlsx")
print(data)

VARselect(data[1:nrow(data), c("gs1", "logcpi","logip", "ebp")], lag.max = 12)

var <- VAR(data[1:nrow(data), c("gs1", "logcpi","logip", "ebp")],
           p = 12,
           type = "const")
summary(var)

plot(irf(var, impulse = "gs1", response = "logip",  n.ahead = 45, ortho = T, seed = 30))
plot(irf(var, impulse = "gs1", response = "logcpi",  n.ahead = 45, ortho = T, seed = 30))
plot(irf(var, impulse = "gs1", response = "ebp",  n.ahead = 45, ortho = T, seed = 30))
plot(irf(var, impulse = "gs1", response = "gs1",  n.ahead = 45, ortho = T, seed = 30))

# Estimating IV-VAR -------------------------------------------------------
devtools::install_github("ErnesztKazmin/ivVar")
library(ivVar)
assignInNamespace("Psi.varest", Psi.varest, ns = "vars")

# Output of the first stage
first_stage(var,
            instrument = (data[1:nrow(data), "ff4_tc"]),
            instrumented = "gs1")[[2]]

# Wald-F test
first_stage(var,
            instrument = (data[1:nrow(data), "ff4_tc"]),
            instrumented = "gs1")[[3]]

coefs <- second_stage(var, instrumented = "gs1",
                      res_model_hat = first_stage(var,
                                                  instrument = (data[1:nrow(data), "ff4_tc"]),
                                                  instrumented = "gs1")[[1]])


plot(irf(var, impulse = "gs1", response = "logip",  n.ahead = 45, ortho = T, seed = 30))
plot(irf(var, impulse = "gs1", response = "logcpi",  n.ahead = 45, ortho = T, seed = 30))
plot(irf(var, impulse = "gs1", response = "gs1",  n.ahead = 45, ortho = T, seed = 30))
plot(irf(var, impulse = "gs1", response = "ebp",  n.ahead = 45, ortho = T, seed = 30))

#Do not forget to reinstall "vars" package after using this package!
#install.packages("vars")
