setwd("Desktop/MiriamProgramming/r/eitc")
cps85 = read.table("cpsmar85.dat", as.is=T)
cps86 = read.table("cpsmar86.dat", as.is=T)
cps87 = read.table("cpsmar87.dat", as.is=T)

h_seq[1:20]
is_household[1:20]
cps85[1:10,]
cps87[1:20,]
table(hnumfam)
table(hnumfam2)
#paste(substr(cps85[1:20,], 10, 10), is_household)

small = head(cps85, 10000)
t85 = read.cps85.take2(cps85)

read.cps85.take2 = function(cps) {
  is_household = as.numeric(substr(cps[,1], 7, 8))
  plist = cps[which(is_household>0 & is_household<41),]
  p = NROW(plist)
  current_h = NULL
  current_f = NULL
  h_seq = rep(NA, p)
  hnumfam = rep(NA, p)
  htotval = rep(NA, p)
  ffpos = rep(NA, p)
  f_seq_pos = rep(NA, p)
  fkind = rep(NA, p)
  ftype = rep(NA, p)
  frelu18 = rep(NA, p)
  ftotval = rep(NA, p)
  pppos = rep(NA, p)
  a_maritl = rep(NA, p)
  a_sex = rep(NA, p)
  a_race = rep(NA, p)
  a_age = rep(NA, p)
  a_hga = rep(NA, p)
  a_hgc = rep(NA, p)
  marsupwt = rep(NA, p)
  wkswork = rep(NA, p)
  hrswk = rep(NA, p)
  rsnnotw = rep(NA, p)
  wsal_val = rep(NA, p)
  semp_val = rep(NA, p)
  frse_val = rep(NA, p)
  ss_val = rep(NA, p)
  ssi_val = rep(NA, p)
  paw_val = rep(NA, p)
  vet_val = rep(NA, p)
  int_val = rep(NA, p)
  ptotval = rep(NA, p)
  pearnval = rep(NA, p)
  pothval = rep(NA, p)
  counter = 0
  for (row in 1:nrow(cps)) {
    if (is_household[row]==0) {
      current_h = cps[row,]
    } else if (is_household[row]>40) {
      current_f = cps[row,]
    } else {
      current_p = cps[row,]
      counter = counter + 1
      
      h_seq[counter] = substr(current_h, 1, 6)
      hnumfam[counter] = as.numeric(substr(current_h, 11, 12))
      htotval[counter] = as.numeric(substr(current_h, 72, 80))
      
      ffpos[counter] = as.numeric(substr(current_f, 7, 8))
      f_seq_pos[counter] = substr(current_f, 1, 8)
      fkind[counter] = as.numeric(substr(current_f, 9, 9))
      ftype[counter] = as.numeric(substr(current_f, 10, 10))
      frelu18[counter] = as.numeric(substr(current_f, 151, 151))
      ftotval[counter] = as.numeric(substr(current_f, 105, 113))
      
      pppos[counter] = as.numeric(substr(current_p, 7, 8))
      a_maritl[counter] = as.numeric(substr(current_p, 107, 107))
      a_sex[counter] = as.numeric(substr(current_p, 108, 108))
      a_race[counter] = as.numeric(substr(current_p, 109, 109))
      a_age[counter] = as.numeric(substr(current_p, 110, 110))
      a_hga[counter] = as.numeric(substr(current_p, 115, 116))
      a_hgc[counter] = as.numeric(substr(current_p, 117, 117))
      marsupwt[counter] = as.numeric(substr(current_p, 118, 128))
      wkswork[counter] = as.numeric(substr(current_p, 134, 135))
      hrswk[counter] = as.numeric(substr(current_p, 136, 137))
      rsnnotw[counter] = as.numeric(substr(current_p, 19, 20))
      wsal_val[counter] = as.numeric(substr(current_p, 191, 195))
      semp_val[counter] = as.numeric(substr(current_p, 196, 201))
      frse_val[counter] = as.numeric(substr(current_p, 202, 207))
      ss_val[counter] = as.numeric(substr(current_p, 208, 212))
      ssi_val[counter] = as.numeric(substr(current_p, 213, 216))
      paw_val[counter] = as.numeric(substr(current_p, 217, 221))
      vet_val[counter] = as.numeric(substr(current_p, 233, 237))
      int_val[counter] = as.numeric(substr(current_p, 222, 226))
      ptotval[counter] = as.numeric(substr(current_p, 248, 254))
      pearnval[counter] = as.numeric(substr(current_p, 255, 261))
      pothval[counter] = as.numeric(substr(current_p, 262, 268))
    }
  }
  t = data.frame(h_seq, hnumfam, htotval, ffpos, f_seq_pos, fkind, ftype, frelu18, ftotval, pppos, a_maritl, a_sex, a_race, a_age,
                 a_hga, a_hgc, marsupwt, wkswork, hrswk, rsnnotw, wsal_val, semp_val, frse_val, ss_val, ssi_val, paw_val, vet_val,
                 int_val, ptotval, pearnval, pothval)
  return(t)
}


read.cps = function(cps) {
  
  is_household = as.numeric(substr(cps[,1], 7, 8))
  
  h_seq = substr(cps[which(is_household==0),], 1, 6)
  hnumfam = as.numeric(substr(cps[which(is_household==0),], 11, 12))
  #hg_st60 = as.numeric(substr(cps[which(is_household==0),], 40, 41))
  #hccc_r = as.numeric(substr(cps[which(is_household==0),], 58, 58))
  htotval = as.numeric(substr(cps[which(is_household==0),], 72, 80))
  h = data.frame(h_seq, hnumfam, htotval)
  
  f_seq = substr(cps[which(is_household>0 & is_household<40),], 1, 6)
  ffpos = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 7, 8))
  f_seq_pos = substr(cps[which(is_household>0 & is_household<40),], 1, 8)
  fkind = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 9, 9))
  ftype = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 10, 10))
  frelu18 = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 151, 151))
  #frelu6 = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 28, 28))
  ftotval = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 105, 113))
  f = data.frame(f_seq, ffpos, f_seq_pos, fkind, ftype, frelu18, ftotval)
  colnames(f)[1] = "h_seq"
  
  p_seq = substr(cps[which(is_household>40 & is_household<80),], 1, 6)
  pppos = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 7, 8))
  #p_f_pos = paste(p_seq, substr(cps[which(is_household>40 & is_household<80),], 44, 45), sep="") #105?
  #a_exprrp = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 13, 14))
  #a_famtyp = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 31, 31))
  #a_famnum = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 29, 30))
  #a_famrel = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 32, 32))
  a_maritl = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 107, 107))
  a_sex = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 108, 108))
  a_race = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 109, 109))
  a_age = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 110, 110))
  #a_reorgn = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 27, 28))
  a_hga = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 115, 116))
  a_hgc = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 117, 117))
  marsupwt = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 118, 128))
  wkswork = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 134, 135))
  hrswk = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 136, 137))
  rsnnotw = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 19, 20))
  
  wsal_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 191, 195))
  semp_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 196, 201))
  frse_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 202, 207))
  #uc_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 278, 282))
  #wc_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 285, 289))
  ss_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 208, 212))
  ssi_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 213, 216))
  paw_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 217, 221))
  vet_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 233, 237))
  #srvs_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 337, 342))
  #dsab_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 360, 365))
  #rtm_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 379, 384))
  int_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 222, 226))
  #div_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 393, 397))
  #rnt_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 399, 403))
  ptotval = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 248, 254))
  pearnval = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 255, 261))
  pothval = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 262, 268))
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