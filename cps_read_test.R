rm(list=ls())
setwd("../Downloads/cpsmar84")
cps84 = read.table("cpsmar84.dat", as.is=T)
head(cps84)
typeof(cps84[1,])
households = NULL
for (row in 1:nrow(cps84)) {
  households[row] = substr(cps84[row,], 7, 8)
}
head(households)
households_numeric = as.numeric(households)
households_numeric[1:100]
household_data = cps84[which(households=="00"),]
label1 = NULL
for (row in 1:nrow(cps84)) {
  label1[row] = substr(cps84[row,], 1, 1)
}
margin.table(label1, 1)
label1[200000:200100]
households[1:100]

h_seq = substr(cps84[which(households=="00"),], 1, 6)
hnumfam = substr(cps84[which(households=="00"),], 11, 12)
hg_st60 = substr(cps84[which(households=="00"),], 39, 40)
hccc_r = substr(cps84[which(households=="00"),], 50, 50)
h = data.frame(h_seq, hnumfam, hg_st60, hccc_r)
head(h)

f_seq = substr(cps84[which(households_numeric>40 & households_numeric<80),], 1, 6)
ffpos = substr(cps84[which(households_numeric>40 & households_numeric<80),], 7, 8)
fkind = substr(cps84[which(households_numeric>40 & households_numeric<80),], 9, 9)
ftype = substr(cps84[which(households_numeric>40 & households_numeric<80),], 10, 10)
frelu18 = substr(cps84[which(households_numeric>40 & households_numeric<80),], 165, 165)
#frelu6 = substr(cps84[which(households_numeric>40 & households_numeric<80),], 148, 148) # could not find
f = data.frame(f_seq, ffpos, fkind, ftype, frelu18)
colnames(f)[1] = "h_seq"
colnames(f)
               
p_seq = substr(cps84[which(households_numeric>0 & households_numeric<40),], 1, 6)
pppos = substr(cps84[which(households_numeric>0 & households_numeric<40),], 7, 8)
a_exprrp = substr(cps84[which(households_numeric>0 & households_numeric<40),], 103, 103)
a_famtyp = substr(cps84[which(households_numeric>0 & households_numeric<40),], 104, 104)
a_famnum = substr(cps84[which(households_numeric>0 & households_numeric<40),], 105, 105)
a_famrel = substr(cps84[which(households_numeric>0 & households_numeric<40),], 106, 106)
a_maritl = substr(cps84[which(households_numeric>0 & households_numeric<40),], 107, 107)
a_sex = substr(cps84[which(households_numeric>0 & households_numeric<40),], 108, 108)
a_race = substr(cps84[which(households_numeric>0 & households_numeric<40),], 109, 109)
a_age = substr(cps84[which(households_numeric>0 & households_numeric<40),], 110, 111)
a_reorgn = substr(cps84[which(households_numeric>0 & households_numeric<40),], 113, 114)
a_hga = substr(cps84[which(households_numeric>0 & households_numeric<40),], 115, 116)
a_hgc = substr(cps84[which(households_numeric>0 & households_numeric<40),], 117, 117)
marsupwt = substr(cps84[which(households_numeric>0 & households_numeric<40),], 118, 128)
wkswork = substr(cps84[which(households_numeric>0 & households_numeric<40),], 134, 135)
hrswk = substr(cps84[which(households_numeric>0 & households_numeric<40),], 136, 137)
rsnnotw = substr(cps84[which(households_numeric>0 & households_numeric<40),], 19, 20)
wsal_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 191, 195)
semp_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 196, 201)
frse_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 202, 207)
# uc_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 1, 6)
# wc_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 1, 6)
ss_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 208, 212)
ssi_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 213, 216)
paw_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 217, 221)
vet_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 233, 237)
# srvs_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 1, 6)
# dsab_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 1, 6)
rtm_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 238, 242)
int_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 222, 226)
div_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 227, 232)
# rnt_val = substr(cps84[which(households_numeric>0 & households_numeric<40),], 1, 6)
ptotval = substr(cps84[which(households_numeric>0 & households_numeric<40),], 248, 254)
pearnval = substr(cps84[which(households_numeric>0 & households_numeric<40),], 255, 261)
pothval = substr(cps84[which(households_numeric>0 & households_numeric<40),], 262, 268)
p = data.frame(p_seq, pppos, a_exprrp, a_famtyp, a_famnum, a_famrel, a_maritl, a_sex, a_race, a_age, a_reorgn,
               a_hga, a_hgc, marsupwt, wkswork, hrswk, rsnnotw, wsal_val, semp_val, frse_val, ss_val, ssi_val,
               paw_val, vet_val, rtm_val, int_val, div_val, ptotval, pearnval, pothval)
colnames(p)[1] = "h_seq"

complete_84 = merge(p, f, by="h_seq")
complete_84 = merge(complete_84, h, by="h_seq")
dim(p)
dim(f)
dim(h)
dim(complete_84)
head(complete_84)
a = c(1, 2, 3, 4, 5)
b = c(0, 0, 0, 1, 1)
c = c(1, 1, 2, 3, 4, 5)
d = c(1, 0, 1, 1, 0, 0)
first = data.frame(a, b)
second = data.frame(c, d)
colnames(second) = c("a", "d")
head(second)
third = merge(first, second, by="a")
third # this worked correctly