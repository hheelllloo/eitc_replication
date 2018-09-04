read.cps = function(cps) {
  
  is_household = as.numeric(substr(cps[,1], 7, 8))
  
  h_seq = substr(cps[which(is_household==0),], 2, 6)
  hnumfam = as.numeric(substr(cps[which(is_household==0),], 23, 24))
  hg_st60 = as.numeric(substr(cps[which(is_household==0),], 40, 41))
  hccc_r = as.numeric(substr(cps[which(is_household==0),], 58, 58))
  htotval = as.numeric(substr(cps[which(is_household==0),], 248, 255))
  h = data.frame(h_seq, hnumfam, hg_st60, hccc_r, htotval)
  
  f_seq = substr(cps[which(is_household>0 & is_household<40),], 2, 6)
  ffpos = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 7, 8))
  f_seq_pos = substr(cps[which(is_household>0 & is_household<40),], 2, 8)
  fkind = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 9, 9))
  ftype = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 10, 10))
  frelu18 = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 29, 29))
  frelu6 = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 28, 28))
  ftotval = as.numeric(substr(cps[which(is_household>0 & is_household<40),], 205, 212))
  f = data.frame(f_seq, ffpos, f_seq_pos, fkind, ftype, frelu18, ftotval)
  colnames(f)[1] = "h_seq"
  
  p_seq = substr(cps[which(is_household>40 & is_household<80),], 2, 6)
  pppos = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 7, 8))
  p_f_pos = paste(p_seq, substr(cps[which(is_household>40 & is_household<80),], 44, 45), sep="")
  a_exprrp = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 13, 14))
  a_famtyp = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 31, 31))
  a_famnum = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 29, 30))
  a_famrel = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 32, 32))
  a_maritl = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 17, 17))
  a_sex = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 20, 20))
  a_race = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 25, 25))
  a_age = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 15, 16))
  a_reorgn = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 27, 28))
  a_hga = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 22, 23))
  a_hgc = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 24, 24))
  marsupwt = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 66, 73))
  wkswork = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 171, 172))
  hrswk = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 181, 182))
  rsnnotw = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 170, 170))
  wsal_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 243, 248))
  semp_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 256, 261))
  frse_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 263, 267))
  uc_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 278, 282))
  wc_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 285, 289))
  ss_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 291, 295))
  ssi_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 297, 300))
  paw_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 305, 309))
  vet_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 317, 321))
  srvs_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 337, 342))
  dsab_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 360, 365))
  rtm_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 379, 384))
  int_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 386, 390))
  div_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 393, 397))
  rnt_val = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 399, 403))
  ptotval = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 440, 447))
  pearnval = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 448, 455))
  pothval = as.numeric(substr(cps[which(is_household>40 & is_household<80),], 457, 464))
  hours = wkswork * hrswk
  youth = as.numeric(a_famrel>2 & (a_age<19 | (a_age>=19 & a_age<24 & rsnnotw==4)))
  not_ind_child = a_famrel < 3 | youth==1
  taxunit = rep(NA, NROW(cps[which(is_household>40),]))
  kl6 = youth==1 & a_age<6
  p = data.frame(pppos, p_f_pos, a_exprrp, a_famtyp, a_famnum, a_famrel, a_maritl, a_sex, a_race, a_age, a_reorgn,
                 a_hga, a_hgc, marsupwt, wkswork, hrswk, rsnnotw, wsal_val, semp_val, frse_val, uc_val, wc_val, 
                 ss_val, ssi_val, paw_val, vet_val, srvs_val, dsab_val, rtm_val, int_val, div_val, ptotval, pearnval, pothval, hours, youth, taxunit, not_ind_child, kl6)
  #colnames(p)[1] = "h_seq"
  colnames(p)[2] = "f_seq_pos"
  
  #print(dim(h))
  #print(dim(f))
  #print(dim(p))
  first = merge(p, f, by="f_seq_pos")
  #print(dim(first))
  second = merge(first, h, by="h_seq")
  #print(dim(second))
  return(second)
}