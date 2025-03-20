library('bcaboot')

data = read.csv('student_score.txt', sep=' ')
Xy = cbind(data$mech, data$vecs, data$alg, data$analy, data$stat)
rfun <- function(Xy){
    ev = eigen(cor(Xy))
    ans = ev$values[1]/sum(ev$values)
}

set.seed(1234)
###
sto <- bcajack(x = Xy, fun = rfun, B = 2000, m = 22, verbose = TRUE)
cat("   Confidence Interval", sto$lims["0.025","bca"], sto$lims["0.5","bca"], sto$lims["0.975","bca"])
