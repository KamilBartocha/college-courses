a<-c(10, -3, 6)
10:5
seq(-5, 5, by=2)
rep(c(2,3),4)
x<-c(2,4,5)
y<-c(1,3,2)
x+y

a[2] # drugi element
a[-2] # wszystkie oprócz drugiego
a[c(1,5)] # pierwszy i piąty
a>0 # wektor logiczny - które większe od zera
a[a>0] # elementy większe od zera

owoce<-c(1,5,8,10)
names(owoce)<-c("jablko","gruszka","banan","sliwka")
obiad<-owoce[c("jablko","banan")]

vect<-c(1,2,1,3,1,2,4,3,2); vect
fact<-factor(vect); fact
summary(fact)
plot(fact)


tbl=1:20
dim(tbl)=c(4,5) # wektor staje sie tablica o wymiarach 4,5
tbl
tbl[2,3] # element na pozycji 2,3
tbl[2,] # drugi wiersz
tbl[,3] # trzecia kolumna
tbl<-cbind(tbl,3)
tbl<-rbind(log(tbl[1,]),tbl)


