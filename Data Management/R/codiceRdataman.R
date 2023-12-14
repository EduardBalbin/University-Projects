top10k<- read.csv("top10k_spotify.csv")

#trovare l'osservazione con più generi
a<-c()
for (i in 1:6643){
  a[i]<-length(unlist(str_split(top10k$Artist.Genres[i],',')))
}
which.max(a)
a[4639]

generi<-matrix(0,nrow=6643,ncol=32)


for (i in 1:6643){
  for( j in 1:length(unlist(str_split(top10k$Artist.Genres[i],',')))){
    generi[i,j]<-unlist(str_split(top10k$Artist.Genres[i],','))[j]
  }
}
write.csv(generi, "generi_obs.csv")
rownames(generi)<-top10k$Artist.Name.s.
generi_artista<-generi

generi_canzone<-generi
rownames(generi_canzone)<-top10k$Track.Name

generi_album<-generi
rownames(generi_canzone)<-top10k$Album.Name

generi_album<-cbind(generi_album,top10k$Country)
generi_artista<-cbind(generi_artista,top10k$Country)
generi_canzone<-cbind(generi_canzone,top10k$Country)
write.csv(generi_album, "generi_album.csv")
write.csv(generi_album, "generi_artista.csv")
write.csv(generi_album, "generi_canzone.csv")
generi2<-cbind(top10k[,c(2,4,6,9)],generi)
write.csv(generi2, "generi2.csv")

radio2<- read.csv("radio_stations.csv")
radio <- read.csv("radio_metadata.csv")
generi <- read.csv("~/generi2.csv", row.names=1)
library(stringr)
a<-c()
for (i in 1:64230){
  a[i]<-length(unlist(str_split(radio$genre[i],',')))
}
which.max(a)
a[40]
amagg1<-which(a>1)
radio1<-radio[-amagg1,]
ha2generi<-which(a[amagg1]<3)
a[amagg1[ha2generi]]
radio2<-radio[amagg1[ha2generi],]
ha3generi<-which(a[amagg1]==3)
radio3<-radio[amagg1[ha3generi],]
dim(radio1)[1]+dim(radio2)[1]+dim(radio3)[1]
dim(radio)[1]
write.csv(radio1, "radio1genere.csv")

generiradio2<-matrix(0,nrow=294,ncol=2)
for (i in 1:294){
  for( j in 1:2){
    generiradio2[i,j]<-unlist(str_split(radio2$genre[i],','))[j]
  }
}
radio2completa<-cbind(radio2[,-3],generiradio2)
write.csv(radio2completa, "radio2generi.csv")

generiradio3<-matrix(0,nrow=112,ncol=3)
for (i in 1:112){
  for( j in 1:3){
    generiradio3[i,j]<-unlist(str_split(radio3$genre[i],','))[j]
  }
}
radio3completa<-cbind(radio3[,-3],generiradio3)
write.csv(radio2completa, "radio3generi.csv")


generi2 <- read.csv("~/generi2.csv", row.names=1)
a<-c()
for (i in 1:6643){
  a[i]<-length(unlist(str_split(generi2$Country[i],',')))
}
which.max(a)
a[2]

countries<-matrix(0,nrow=6643,ncol=184)
for (i in 1:6643){
  for( j in 1:length(unlist(str_split(generi2$Country[i],',')))){
    countries[i,j]<-unlist(str_split(generi2$Country[i],','))[j]
  }
}
countries_complete<-cbind(generi2[,c(1:3)],countries)
write.csv(countries_complete, "countries_complete.csv")

countries_generi<-cbind(generi2[,-4],countries)
write.csv(countries_generi, "countries_generi.csv")


cg<- read.csv("~/countries_generi.csv", row.names=1)
radio1 <- read.csv("~/radio1genere.csv", row.names=1)
sologeneri<-cg[,c(4:35)]
solopaesi<-cg[,c(36:219)]
#dato che tutti i generi sono in minuscolo, anche nel 
#dataset radio mettiamo tutto in minuscolo
radio1$genre<-tolower(radio1$genre)
#trovo il primo 0 per la prima canzone
which(sologeneri[1,]==0)[1]-1

listacanzoni<-vector("list",6643)
sologeneri[1465,]
sologeneri2<-sologeneri[-4639,]
a=0
for(j in 1:dim(sologeneri2)[1]){
  nome_oggetto <- paste0("song", j)
  assign(nome_oggetto, c())
  for(i in 1:which(sologeneri2[j,]==0)[1]-1){
    assign(nome_oggetto, c(get(nome_oggetto),
                           which(radio1$genre==sologeneri2[j,i])))
  }
  a=a+1
  print(a)
  listacanzoni[[j]]<-get(nome_oggetto)
}
a
canzonecon32generi<-c()
for(i in 1:32){
  canzonecon32generi<-c(canzonecon32generi,
                        which(radio1$genre==sologeneri[1465,i]))
}
listacanzoni[[6643]]<-canzonecon32generi
listacanzoni2<-listacanzoni[c(1:1464,6643,1465:6642)]
all(listacanzoni[[6643]]==listacanzoni2[[1465]])
all(listacanzoni[[1465]]==listacanzoni2[[1466]])
nonrm<-c("cg","radio1","sologeneri","solopaesi","listacanzoni2","sologeneri2")
rimuovere <- setdiff(ls(), nonrm)
rm(list = rimuovere)
#da usare è lista canzoni 2

a<-c()
for(i in 1:6643){
  a[i]<-length(listacanzoni2[[i]])
}
a
canzoniconcorrispondenza<-which(a>0)
listacanzoni2$indice<-c(1:6643)
listaok<-listacanzoni2[c(canzoniconcorrispondenza)]
nome_canzone_listaok<-cg$Track.Name[canzoniconcorrispondenza]
top10k_spotify <- read.csv("top10k_spotify.csv")
trackURI_listaok<-top10k_spotify$Track.URI[canzoniconcorrispondenza]
artist_uri_listaok<-top10k_spotify$Artist.URI.s.[canzoniconcorrispondenza]
artist_nome_listaok<-top10k_spotify$Artist.Name.s.[canzoniconcorrispondenza]
#in solopaesi ogni paese è preceduto da uno spazio, qui li rimuovo tutti
for(i in 1:6643){
  for(j in 1:184){
    solopaesi[i,j]<-sub(" ", "",solopaesi[i,j])
  }
}

unique(listaok[[]])
a<-c()
for(i in 1:3114){
  a[i]<-listaok[[1]]==listaok[[i]]
}  
radiougualiallaprimacanzone=which(a)

b<-c()
for(i in 1:3114){
  b[i]<-listaok[[3]]==listaok[[i]]
}  
radiougualiallasecondacanzone<-which(b)
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone))

c<-c()
for(i in 1:3114){
  c[i]<-listaok[[6]]==listaok[[i]]
}  
radiougualiallasestacanzone<-which(c)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone))

d<-c()
for(i in 1:3114){
  d[i]<-listaok[[8]]==listaok[[i]]
}  
radiougualiallaottavacanzone<-which(d)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone))

e<-c()
for(i in 1:3114){
  e[i]<-listaok[[14]]==listaok[[i]]
}  
radiougualialla14canzone<-which(e)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone))

f<-c()
for(i in 1:3114){
  f[i]<-listaok[[35]]==listaok[[i]]
}  
radiougualialla35canzone<-which(f)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone,radiougualialla35canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone,radiougualialla35canzone))

g<-c()
for(i in 1:3114){
  g[i]<-listaok[[39]]==listaok[[i]]
}  
radiougualialla39canzone<-which(g)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone,radiougualialla35canzone,
              radiougualialla39canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone,radiougualialla35canzone,
       radiougualialla39canzone))

h<-c()
for(i in 1:3114){
  h[i]<-listaok[[67]]==listaok[[i]]
}  
radiougualialla67canzone<-which(h)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone,radiougualialla35canzone,
              radiougualialla39canzone,radiougualialla67canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone,radiougualialla35canzone,
       radiougualialla39canzone,radiougualialla67canzone))

k<-c()
for(i in 1:3114){
  k[i]<-listaok[[75]]==listaok[[i]]
}  
radiougualialla75canzone<-which(k)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone,radiougualialla35canzone,
              radiougualialla39canzone,radiougualialla67canzone,
              radiougualialla75canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone,radiougualialla35canzone,
       radiougualialla39canzone,radiougualialla67canzone,
       radiougualialla75canzone))

l<-c()
for(i in 1:3114){
  l[i]<-listaok[[104]]==listaok[[i]]
}  
radiougualialla104canzone<-which(l)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone,radiougualialla35canzone,
              radiougualialla39canzone,radiougualialla67canzone,
              radiougualialla75canzone,radiougualialla104canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone,radiougualialla35canzone,
       radiougualialla39canzone,radiougualialla67canzone,
       radiougualialla75canzone,radiougualialla104canzone))

m<-c()
for(i in 1:3114){
  m[i]<-listaok[[122]]==listaok[[i]]
}  
radiougualialla122canzone<-which(m)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone,radiougualialla35canzone,
              radiougualialla39canzone,radiougualialla67canzone,
              radiougualialla75canzone,radiougualialla104canzone,
              radiougualialla122canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone,radiougualialla35canzone,
       radiougualialla39canzone,radiougualialla67canzone,
       radiougualialla75canzone,radiougualialla104canzone,
       radiougualialla122canzone))

n<-c()
for(i in 1:3114){
  n[i]<-listaok[[221]]==listaok[[i]]
}  
radiougualialla221canzone<-which(n)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone,radiougualialla35canzone,
              radiougualialla39canzone,radiougualialla67canzone,
              radiougualialla75canzone,radiougualialla104canzone,
              radiougualialla122canzone,radiougualialla221canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone,radiougualialla35canzone,
       radiougualialla39canzone,radiougualialla67canzone,
       radiougualialla75canzone,radiougualialla104canzone,
       radiougualialla122canzone,radiougualialla221canzone))

o<-c()
for(i in 1:3114){
  o[i]<-listaok[[347]]==listaok[[i]]
}  
radiougualialla347canzone<-which(o)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone,radiougualialla35canzone,
              radiougualialla39canzone,radiougualialla67canzone,
              radiougualialla75canzone,radiougualialla104canzone,
              radiougualialla122canzone,radiougualialla221canzone,
              radiougualialla347canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone,radiougualialla35canzone,
       radiougualialla39canzone,radiougualialla67canzone,
       radiougualialla75canzone,radiougualialla104canzone,
       radiougualialla122canzone,radiougualialla221canzone,
       radiougualialla347canzone))

p<-c()
for(i in 1:3114){
  p[i]<-listaok[[660]]==listaok[[i]]
}  
radiougualialla660canzone<-which(p)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone,radiougualialla35canzone,
              radiougualialla39canzone,radiougualialla67canzone,
              radiougualialla75canzone,radiougualialla104canzone,
              radiougualialla122canzone,radiougualialla221canzone,
              radiougualialla347canzone,radiougualialla660canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone,radiougualialla35canzone,
       radiougualialla39canzone,radiougualialla67canzone,
       radiougualialla75canzone,radiougualialla104canzone,
       radiougualialla122canzone,radiougualialla221canzone,
       radiougualialla347canzone,radiougualialla660canzone))[1000:2000]

r<-c()
for(i in 1:3114){
  r[i]<-listaok[[1252]]==listaok[[i]]
}  
radiougualialla1252canzone<-which(r)
length(sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
              radiougualiallasestacanzone,radiougualiallaottavacanzone,
              radiougualialla14canzone,radiougualialla35canzone,
              radiougualialla39canzone,radiougualialla67canzone,
              radiougualialla75canzone,radiougualialla104canzone,
              radiougualialla122canzone,radiougualialla221canzone,
              radiougualialla347canzone,radiougualialla660canzone,
              radiougualialla1252canzone)))
sort(c(radiougualiallaprimacanzone,radiougualiallasecondacanzone,
       radiougualiallasestacanzone,radiougualiallaottavacanzone,
       radiougualialla14canzone,radiougualialla35canzone,
       radiougualialla39canzone,radiougualialla67canzone,
       radiougualialla75canzone,radiougualialla104canzone,
       radiougualialla122canzone,radiougualialla221canzone,
       radiougualialla347canzone,radiougualialla660canzone,
       radiougualialla1252canzone))[3001:3114]

#canz di riferimento per radio diverse:
#1,3,6,8,14,35,39,67,75,104,122,221,347,660,1252

#associare le radio alle canzoni, canzone 1 e tutte le uguali
c1possibradio<-radio1[listaok[[1]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
listamatch<-listaok
for(i in radiougualiallaprimacanzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 3 e tutte le uguali
c1possibradio<-radio1[listaok[[3]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualiallasecondacanzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 6 e tutte le uguali
c1possibradio<-radio1[listaok[[6]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualiallasestacanzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 8 e tutte le uguali
c1possibradio<-radio1[listaok[[8]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualiallaottavacanzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 14 e tutte le uguali
c1possibradio<-radio1[listaok[[14]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla14canzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 35 e tutte le uguali
c1possibradio<-radio1[listaok[[35]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla35canzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 39 e tutte le uguali
c1possibradio<-radio1[listaok[[39]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla39canzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 67 e tutte le uguali
c1possibradio<-radio1[listaok[[67]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla67canzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 75 e tutte le uguali
c1possibradio<-radio1[listaok[[75]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla75canzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 104 e tutte le uguali
c1possibradio<-radio1[listaok[[104]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla104canzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 122 e tutte le uguali
c1possibradio<-radio1[listaok[[122]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla122canzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 221 e tutte le uguali
c1possibradio<-radio1[listaok[[221]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla221canzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 347 e tutte le uguali
c1possibradio<-radio1[listaok[[347]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla347canzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 660 e tutte le uguali
c1possibradio<-radio1[listaok[[660]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla660canzone){
  listamatch[[i]]<-radioc1
}

#associare le radio alle canzoni, canzone 1252 e tutte le uguali
c1possibradio<-radio1[listaok[[1252]],c(1,2,5)]
a<-c()
for(i in 1:dim(c1possibradio)[1]){
  a[i]<-any(solopaesi[1,]==c1possibradio$country_code[i])
}
a
radioc1<-c1possibradio[which(a),]
for(i in radiougualialla1252canzone){
  listamatch[[i]]<-radioc1
}

datenere<-listamatch

names(listamatch)<-trackURI_listaok
listamatch[[1]]


a<-c()
for(i in 1:3114){
  a[i]<-length(listamatch[[i]][,1])
}
max(a)
prova<-matrix(0,3114,5975)
iter=0
for(i in 1:3114){
  for(j in 1:length(listamatch[[i]][,1])){
    prova[i,j]<-listamatch[[i]][j,2]
  }
  iter=iter+1
  print(iter)
}
prova2<-cbind(trackURI_listaok,prova)
prova3<-cbind(nome_canzone_listaok,prova2)
prova4<-cbind(artist_nome_listaok,prova3)
prova5<-cbind(artist_uri_listaok,prova4)
write.csv(prova5, "match_finale3.csv")

match_finale <- read.csv("~/match_finale3.csv", row.names=1)
a<-match_finale[,c(1:1500)]
write.csv(a,"match1_1.csv")

b<-match_finale[,c(1501:3000)]
write.csv(b,"match1_2.csv")

c<-match_finale[,c(3001:4500)]
write.csv(c,"match1_3.csv")
match_finale <- read.csv("~/match_finale.csv", row.names=1)
mf<-match_finale
tot0<-0
for(i in 1:3114){
  tot0<-tot0+sum(mf[i,]=="0")
}
completeness<-tot0/(dim(mf)[1]*dim(mf)[2])
#0.4495975
any(duplicated(mf$artist_uri_listaok))
any(duplicated(mf$artist_nome_listaok))
any(duplicated(mf$nome_canzone_listaok))
any(duplicated(mf$trackURI_listaok))
#possiamo vedere che per diverse canzoni l'artitsta si ripete, così come il
#suo URI, mentre l'URI della canzone è univoco, al contrario ci sono dei nomi
#duplicati per le canzoni, ciò poichè magari vengono create 2 osservazioni con 
#la stessa canzone, con qualche caratteristica diversa come l'URI dell'artista, 
#perciò il nome apparirà più volte, mentre l'URI identificherà universalmente
#la singola osservazione
a<-c()
for(i in 1:3114){
  a[i]<-any(duplicated(mf[i,]))
}
any(a)
#there are no duplicated in a single observation



d<-match_finale[,c(4501:5979)]
write.csv(d,"match1_4.csv")


