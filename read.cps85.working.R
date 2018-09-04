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
  relhead = rep(NA, p)
  a_famrel = rep(NA, p)
  hours = rep(NA, p)
  youth = rep(NA, p)
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
      a_age[counter] = as.numeric(substr(current_p, 110, 111))
      a_hga[counter] = as.numeric(substr(current_p, 115, 116))
      a_hgc[counter] = as.numeric(substr(current_p, 117, 117))
      marsupwt[counter] = as.numeric(substr(current_p, 118, 128))
      wkswork[counter] = as.numeric(substr(current_p, 134, 135))
      hrswk[counter] = as.numeric(substr(current_p, 136, 137))
      rsnnotw[counter] = as.numeric(substr(current_p, 133, 133))
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
      relhead[counter] = as.numeric(substr(current_p, 103, 103))
      a_famrel[counter] = as.numeric(substr(current_p, 106, 106))
    }
  }
  hours = hrswk * wkswork
  youth = as.numeric((relhead==4 | relhead==5 | a_famrel>2) & (a_age<19 | (a_age>=19 & a_age<24 & rsnnotw==4)))
  #youth = as.numeric((a_famrel>2) & (a_age<19 | (a_age>=19 & a_age<24 & rsnnotw==4)))
  t = data.frame(h_seq, hnumfam, htotval, ffpos, f_seq_pos, fkind, ftype, frelu18, ftotval, pppos, a_maritl, a_sex, a_race, a_age,
                 a_hga, a_hgc, marsupwt, wkswork, hrswk, rsnnotw, wsal_val, semp_val, frse_val, ss_val, ssi_val, paw_val, vet_val,
                 int_val, ptotval, pearnval, pothval, relhead, a_famrel, hours, youth)
  return(t)
}