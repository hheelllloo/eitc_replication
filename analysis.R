setwd("Desktop/MiriamProgramming/r/eitc")
source("read.cps.R")
source("read.cps85.working.R")
source("extract.cps.R")
cps85 = read.table("cpsmar85.dat", as.is=T)
cps86 = read.table("cpsmar86.dat", as.is=T)
cps87 = read.table("cpsmar87.dat", as.is=T)
cps89 = read.table("cpsmar89.dat", as.is=T)
cps90 = read.table("cpsmar90.dat", as.is=T)
cps91 = read.table("cpsmar91.dat", as.is=T)
t85 = read.cps85.take2(cps85)
t86 = read.cps85.take2(cps86)
t87 = read.cps85.take2(cps87)
t89 = read.cps(cps89)
t90 = read.cps(cps90)
t91 = read.cps(cps91)
rm(list=c("cps85", "cps86", "cps87", "cps89", "cps90", "cps91"))
a85 = extract.cps2(t85)
a86 = extract.cps2(t86)
a87 = extract.cps2(t87)
a89 = extract.cps(t89)
a90 = extract.cps(t90)
a91 = extract.cps(t91)
rm(list=c("t85", "t86", "t87", "t89", "t90", "t91"))

se = function(x) sqrt(var(x)/length(x))
install.packages("Hmisc")
library("Hmisc")

participation = function(data) {
  for (set in data) {
    worked = as.numeric(set$hrswk>0)
    print(weighted.mean(worked, set$marsupwt))
  }
}

participation(list(no_children1, children1, no_children2, children2))
participation(list(children2[which(children2$a_hga < 9),], no_children2[which(no_children2$a_hga<9),]))
table(after_eitc$a_hga)
# check completed
table(after_eitc$a_hgc)
before_eitc = rbind(a85, a86, a87)
after_eitc = rbind(a89, a90, a91)
no_children1 = before_eitc[which(before_eitc$tyouth==0),]
children1 = before_eitc[which(before_eitc$tyouth>0),]
no_children2 = after_eitc[which(after_eitc$tyouth==0),]
children2 = after_eitc[which(after_eitc$tyouth>0),]
participation(list(children2[which(children2$below_hs==1),], no_children2[which(no_children2$below_hs==1),], children2[which(children2$below_hs==0 & children2$hs==0),]))
participation(list(children2[which(children2$hs==1),], no_children2[which(no_children2$hs==1),], children2[which(children2$below_hs==0 & children2$hs==0),]))
nrow(children2[which(children2$below_hs==1),])
nrow(no_children2[which(no_children2$below_hs==1),])
nrow(children2[which(children2$below_hs==0 & children2$hs==0),])


nrow(children1) + nrow(children2) + nrow(no_children1) + nrow(no_children2)
nrow(no_children1)

nrow(children1[which(children1$below_hs==1),])
nrow(no_children1[which(no_children1$below_hs==1),])
nrow(children1[which(children1$below_hs==0 & children1$hs==0),])
participation(list(children1[which(children1$below_hs==1),], no_children1[which(no_children1$below_hs==1),], children1[which(children1$below_hs==0 & children1$hs==0),]))
participation(list(children1[which(children1$hs==1),], no_children1[which(no_children1$hs==1),], children1[which(children1$below_hs==0 & children1$hs==0),]))

se(worked2)
se(worked3)
# the below two commands do not work
sqrt(wtd.var(worked2, no_children2$marsupwt))
sqrt(wtd.var(worked3, children2$marsupwt))


treatment1 = children1[which(children1$a_hga<9),]
control11 = no_children1[which(no_children1$a_hga>8 & no_children1$a_hga<13),]
control12 = children1[which(children1$a_hga>12),]
treatment2 = children2[which(children2$a_hga<9),]
control21 = no_children2[which(no_children2$a_hga>8 & no_children2$a_hga<13),]
control22 = children2[which(children2$a_hga>12),]
worked10 = as.numeric(treatment1$hrswk>0)
worked11 = as.numeric(control11$hrswk>0)
worked12 = as.numeric(control12$hrswk>0)
worked13 = as.numeric(treatment2$hrswk>0)
worked14 = as.numeric(control21$hrswk>0)
worked15 = as.numeric(control22$hrswk>0)
weighted.mean(worked10, treatment1$marsupwt)
weighted.mean(worked11, control11$marsupwt)
weighted.mean(worked12, control12$marsupwt)
weighted.mean(worked13, treatment2$marsupwt)
weighted.mean(worked14, control21$marsupwt)
weighted.mean(worked15, control22$marsupwt)
