library(dplyr)
library(readr)

setwd('/home/ghilman/kpu_crawls/caleg/profil_caleg/')

file_list <- list.files(pattern = '.csv')
file_error <- vector()
profil_caleg = data_frame()
for (file in file_list){
  caleg_id = strsplit(file, "_")[[1]][5]
  
  # if the merged dataset does exist, append to it
  if (exists("profil_caleg")){
    temp_profil_caleg <-read_delim(file, delim = "|")
    if(nrow(temp_profil_caleg)!=18){
      file_error = c(file_error, file)
      print(file)
      next
    }
    temp_profil_caleg = data.frame(t(temp_profil_caleg))
    colnames(temp_profil_caleg) = as.character(unlist(temp_profil_caleg[1,]))
    row.names(temp_profil_caleg) = NULL
    temp_profil_caleg = temp_profil_caleg[-1,]
    temp_profil_caleg$caleg_id = caleg_id
    profil_caleg<-rbind(profil_caleg, temp_profil_caleg)
    rm(temp_profil_caleg)
  }
  
}
profil_caleg %>% group_by(caleg_id) %>% summarise(a = n()) %>% filter(a>1)

setwd('/home/ghilman/kpu_crawls/caleg/list_caleg/')

file_list <- list.files(pattern = '.csv')

rm(list_caleg)
list_caleg = data.frame()
for (file in file_list){
  # if the merged dataset doesn't exist, create it
  if (!exists("list_caleg")){
    list_caleg <- read_delim(file, delim = "|", progress = F)
    print(paste(file, "hello"))
  }
  
  # if the merged dataset does exist, append to it
  if (exists("list_caleg")){
    temp_list_caleg <-read_delim(file, delim = "|", progress = F)
    print(file)
    list_caleg<-rbind(list_caleg, temp_list_caleg)
    rm(temp_list_caleg)
  }
  
}
list_caleg %>% group_by(id) %>% summarise(a = n()) %>% filter(a>1)

list_caleg_f = list_caleg %>% select(namaKab, id, stringJenisKelamin, partai, Dapil, provinsi)
colnames(list_caleg_f)[1] = 'kota_tinggal'
profil_caleg$caleg_id = as.integer(profil_caleg$caleg_id)

fulldat = list_caleg_f %>% left_join(profil_caleg, by=c('id'='caleg_id'))
fulldat = data.frame(lapply(fulldat, function(v) { tolower(v)}))
library(lubridate)

fulldat$umur = floor((Sys.Date()-dmy(fulldat$Tanggal.Lahir))/360)

save.image(file="caleg.RData") 

library(purrr)
library(tidyr)
library(ggplot2)

fulldat$No..Urut = as.numeric(levels(fulldat$No..Urut))[fulldat$No..Urut]
fulldat$Jumlah.Anak = as.numeric(levels(fulldat$Jumlah.Anak))[fulldat$Jumlah.Anak]

fulldat %>%
  select(stringJenisKelamin, partai, No..Urut, Agama, Status.Perkawinan, Jumlah.Anak, Pendidikan, Status.Khusus, umur) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat="count")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6))

