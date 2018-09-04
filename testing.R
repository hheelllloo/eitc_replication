rm(list=ls())
setwd("Desktop/MiriamProgramming/r/eitc")
cps89 = read.table("cpsmar89.dat", as.is=T)
cps90 = read.table("cpsmar90.dat", as.is=T)
cps91 = read.table("cpsmar91.dat", as.is=T)

t_89 = read.cps(cps89)
t_89[6:12,]


hlist = cps89[which(households==0),]
flist = cps89[which(households>0 & households<=40),]
plist = cps89[which(households>40),] #144687 people records

is_household = as.numeric(substr(cps89[,1], 7, 8))

60629 + 70454 + 144687
table(substr(hlist, 23, 24)) #number of families
table(substr(hlist, 21, 22)) #number of people
table(substr(flist, 11, 12)) #number of people - every family has at least 1 person

table(substr(hlist, 21, 22), substr(hlist, 23, 24))
head(hlist)

hnumper = as.numeric(substr(hlist, 21, 22))
hnumfam = as.numeric(substr(hlist, 23, 24))
table(hnumper)
sum(hnumper) #the same as the number of people records which is good
sum(hnumfam) #same as number of families which is good
hlist2 = hlist[which(hnumper>0)] #only households with at least 1 person
sum(as.numeric(substr(hlist2, 21, 22))) #the number of people did not change which is good
hnumfam2 = as.numeric(substr(hlist2, 23, 24))
table(hnumfam2)
hlist3 = hlist[which(substr(hlist, 21, 22)=="00")]

fnumper = as.numeric(substr(flist, 11, 12))
sum(fnumper) #147957 people in families - higher than number of people uh-oh
table(fnumper)
minors = as.numeric(substr(flist, 29, 29))
sum(minors)
147957 - 40751
70454 - 55335
h_nof = hlist[which(substr(hlist, 23, 24)=="00")]
table(substr(h_nof, 21, 22)) #now 0 people not in family
sum(as.numeric(substr(h_nof, 21, 22)))
147957 - 137244
sum(as.numeric(substr(flist, 28, 28))) #people under 6
sum(as.numeric(substr(plist, 32, 32))==0) #19663 people in one person family
table(substr(plist, 32, 32))

p.nof = plist[which(substr(plist, 32, 32)=="0")]
p.f = plist[which(substr(plist, 32, 32)!="0")]
125024 + 19663

p.nof2 = plist[which(substr(plist, 35, 36)=="10")]
p.nof3 = plist[which(substr(plist, 35, 36)=="11")]
15793 + 3870 #19663