rm(list=ls())
setwd("Desktop/MiriamProgramming/r/eitc")
cps89 = read.table("cpsmar89.dat", as.is=T)
cps90 = read.table("cpsmar90.dat", as.is=T)
cps91 = read.table("cpsmar91.dat", as.is=T)
households = NULL
for (row in 1:nrow(cps89)) {
  households[row] = as.numeric(substr(cps89[row,], 7, 8))
}


read.cps = function(cps) {
  #households = NULL
  #for (row in 1:nrow(cps)) {
    #households[row] = as.numeric(substr(cps[row,], 7, 8))
  #}
  
  h_seq = substr(cps[which(households==0),], 2, 6)
  hnumfam = as.numeric(substr(cps[which(households==0),], 23, 24))
  hg_st60 = as.numeric(substr(cps[which(households==0),], 40, 41))
  hccc_r = as.numeric(substr(cps[which(households==0),], 58, 58))
  h = data.frame(h_seq, hnumfam, hg_st60, hccc_r)
  
  f_seq = substr(cps[which(households>0 & households<40),], 2, 6)
  ffpos = as.numeric(substr(cps[which(households>0 & households<40),], 7, 8))
  f_seq_pos = substr(cps[which(households>0 & households<40),], 2, 8)
  fkind = as.numeric(substr(cps[which(households>0 & households<40),], 9, 9))
  ftype = as.numeric(substr(cps[which(households>0 & households<40),], 10, 10))
  frelu18 = as.numeric(substr(cps[which(households>0 & households<40),], 29, 29))
  frelu6 = as.numeric(substr(cps[which(households>0 & households<40),], 28, 28))
  f = data.frame(f_seq, ffpos, f_seq_pos, fkind, ftype, frelu18)
  colnames(f)[1] = "h_seq"
  
  p_seq = substr(cps[which(households>40 & households<80),], 2, 6)
  pppos = as.numeric(substr(cps[which(households>40 & households<80),], 7, 8))
  p_f_pos = paste(p_seq, substr(cps[which(households>40 & households<80),], 44, 45), sep="")
  a_exprrp = as.numeric(substr(cps[which(households>40 & households<80),], 13, 14))
  a_famtyp = as.numeric(substr(cps[which(households>40 & households<80),], 31, 31))
  a_famnum = as.numeric(substr(cps[which(households>40 & households<80),], 29, 30))
  a_famrel = as.numeric(substr(cps[which(households>40 & households<80),], 32, 32))
  a_maritl = as.numeric(substr(cps[which(households>40 & households<80),], 17, 17))
  a_sex = as.numeric(substr(cps[which(households>40 & households<80),], 20, 20))
  a_race = as.numeric(substr(cps[which(households>40 & households<80),], 25, 25))
  a_age = as.numeric(substr(cps[which(households>40 & households<80),], 15, 16))
  a_reorgn = as.numeric(substr(cps[which(households>40 & households<80),], 27, 28))
  a_hga = as.numeric(substr(cps[which(households>40 & households<80),], 22, 23))
  a_hgc = as.numeric(substr(cps[which(households>40 & households<80),], 24, 24))
  marsupwt = as.numeric(substr(cps[which(households>40 & households<80),], 66, 73))
  wkswork = as.numeric(substr(cps[which(households>40 & households<80),], 171, 172))
  hrswk = as.numeric(substr(cps[which(households>40 & households<80),], 181, 182))
  rsnnotw = as.numeric(substr(cps[which(households>40 & households<80),], 170, 170))
  wsal_val = as.numeric(substr(cps[which(households>40 & households<80),], 243, 248))
  semp_val = as.numeric(substr(cps[which(households>40 & households<80),], 256, 261))
  frse_val = as.numeric(substr(cps[which(households>40 & households<80),], 263, 267))
  uc_val = as.numeric(substr(cps[which(households>40 & households<80),], 278, 282))
  wc_val = as.numeric(substr(cps[which(households>40 & households<80),], 285, 289))
  ss_val = as.numeric(substr(cps[which(households>40 & households<80),], 291, 295))
  ssi_val = as.numeric(substr(cps[which(households>40 & households<80),], 297, 300))
  paw_val = as.numeric(substr(cps[which(households>40 & households<80),], 305, 309))
  vet_val = as.numeric(substr(cps[which(households>40 & households<80),], 317, 321))
  srvs_val = as.numeric(substr(cps[which(households>40 & households<80),], 337, 342))
  dsab_val = as.numeric(substr(cps[which(households>40 & households<80),], 360, 365))
  rtm_val = as.numeric(substr(cps[which(households>40 & households<80),], 379, 384))
  int_val = as.numeric(substr(cps[which(households>40 & households<80),], 386, 390))
  div_val = as.numeric(substr(cps[which(households>40 & households<80),], 393, 397))
  rnt_val = as.numeric(substr(cps[which(households>40 & households<80),], 399, 403))
  ptotval = as.numeric(substr(cps[which(households>40 & households<80),], 440, 447))
  pearnval = as.numeric(substr(cps[which(households>40 & households<80),], 448, 455))
  pothval = as.numeric(substr(cps[which(households>40 & households<80),], 457, 464))
  p = data.frame(pppos, p_f_pos, a_exprrp, a_famtyp, a_famnum, a_famrel, a_maritl, a_sex, a_race, a_age, a_reorgn,
                 a_hga, a_hgc, marsupwt, wkswork, hrswk, rsnnotw, wsal_val, semp_val, frse_val, uc_val, wc_val, 
                 ss_val, ssi_val, paw_val, vet_val, srvs_val, dsab_val, rtm_val, int_val, div_val, ptotval, pearnval, pothval)
  #colnames(p)[1] = "h_seq"
  colnames(p)[2] = "f_seq_pos"
  
  print(dim(h))
  print(dim(f))
  print(dim(p))
  first = merge(p, f, by="f_seq_pos")
  print(dim(first))
  second = merge(first, h, by="h_seq")
  print(dim(second))
  return(second)
}


table_89 = read.cps(cps89)
table_90 = read.cps(cps90)
table_91 = read.cps(cps91)
head(table_91)
nchar(cps89[1,])
typeof(table_89$a_age)
ages = as.numeric(table_89$a_age)
mean(ages, na.rm=TRUE)
head(table_89$a_age)
head(ages)
summary(ages)
summary(table_89$a_age)
mean(table_89$a_age, na.rm=TRUE)
weighted.mean(table_89$wsal_val, table_89$marsupwt)
median(table_89$wsal_val, na.rm = TRUE)
head(table_89$wsal_val)
median(table_89$pearnval)
pearnval = as.numeric(substr(cps89[which(households>40 & households<80),], 448, 455))
typeof(pearnval)
pearnval = substr(cps89[which(households>40 & households<80),], 448, 45)
head(pearnval)
a = as.numeric(pearnval)
head(a)
a
table(table_89$a_race)