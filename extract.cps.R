#this one for 89, 90, 91
extract.cps = function(cps) {
  cps$below_hs = as.numeric(cps$a_hga<12 | (cps$a_hga==12 &cps$a_hgc!=1))
  cps$hs = as.numeric(cps$a_hga==12 & cps$a_hgc==1)
  cps$int_h_seq = as.numeric(levels(cps$h_seq))[cps$h_seq]
  cps$taxunit = 10000 * cps$int_h_seq + 100 * as.numeric(cps$ffpos)
  cps$taxunit[which(!cps$not_ind_child)] = cps$taxunit[which(!cps$not_ind_child)] + cps$pppos[which(!cps$not_ind_child)]
  table_tyouth = aggregate(cps$youth, by=list(Category=cps$taxunit), FUN=sum)
  colnames(table_tyouth) = c("taxunit", "tyouth")
  cps2 = merge(cps, table_tyouth, by="taxunit")
  a = cps2[which(cps2$a_sex==2),] #just females
  b = a[which(a$a_maritl>3 & a$a_maritl!=6),] #just unmarried (find out how to exclude those separated in previous year)
  c = b[which(b$a_age>15 & b$a_age<45),] #between age of 16 and 44 (ask)
  d = c[which(c$rsnnotw!=1 & c$rsnnotw!=4),] #remove sick, disabled, or in school
  e = d[which(d$hours>0 | d$pearnval==0),] #remove those with zero hours worked and nonzero earnings
  f = e[which(e$pearnval>=0 & e$pothval>=0),] #remove those with negative earnings or negative unearned income
  g = f[which(f$youth==0),]
  return(g)
}

#this one for 85, 86, 87
extract.cps2 = function(cps) {
  cps$below_hs = as.numeric(cps$a_hga<13 | (cps$a_hga==13 &cps$a_hgc!=1))
  cps$hs = as.numeric(cps$a_hga==13 & cps$a_hgc==1)
  #cps$not_ind_child = (cps$a_famrel < 3 & cps$relhead!=4 & cps$relhead!=5) | cps$youth==1
  cps$not_ind_child = (cps$a_famrel < 3) | cps$youth==1
  #print(table(cps$not_ind_child))
  cps$taxunit = rep(NA, NROW(cps))
  cps$kl6 = cps$youth==1 & cps$a_age<6
  cps$int_h_seq = as.numeric(levels(cps$h_seq))[cps$h_seq]
  cps$taxunit = 10000 * cps$int_h_seq + 100 * as.numeric(cps$ffpos)
  cps$taxunit[which(!cps$not_ind_child)] = cps$taxunit[which(!cps$not_ind_child)] + cps$pppos[which(!cps$not_ind_child)]
  table_tyouth = aggregate(cps$youth, by=list(Category=cps$taxunit), FUN=sum)
  colnames(table_tyouth) = c("taxunit", "tyouth")
  cps2 = merge(cps, table_tyouth, by="taxunit")
  a = cps2[which(cps2$a_sex==2),] #just females
  b = a[which(a$a_maritl>4 & a$a_maritl!=7),] #just unmarried (find out how to exclude those separated in previous year)
  c = b[which(b$a_age>15 & b$a_age<45),] #between age of 16 and 44 (ask)
  d = c[which(c$rsnnotw!=1 & c$rsnnotw!=3),] #remove sick, disabled, or in school
  e = d[which(d$hours>0 | d$pearnval==0),] #remove those with zero hours worked and nonzero earnings
  f = e[which(e$pearnval>=0 & e$pothval>=0),] #remove those with negative earnings or negative unearned income
  #print(table(f$a_famrel))
  g = f[which(f$youth==0),]
  return(g)
}