library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(png)
library(ggpubr)

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

list_caleg_f = list_caleg %>% select(namaKab, id, stringJenisKelamin, partai, Dapil, provinsi, noUrut, gelarDepan, gelarBelakang)
colnames(list_caleg_f)[1] = 'kota_tinggal'
profil_caleg$caleg_id = as.integer(profil_caleg$caleg_id)

fulldat = list_caleg_f %>% left_join(profil_caleg[,-c(1,3,4,8)], by=c('id'='caleg_id'))
fulldat = data.frame(lapply(fulldat, function(v) { tolower(v)}))

fulldat$noUrut = as.numeric(levels(fulldat$noUrut))[fulldat$noUrut]
fulldat$Jumlah.Anak = as.numeric(levels(fulldat$Jumlah.Anak))[fulldat$Jumlah.Anak]
fulldat %>% mutate_if(is.factor, as.character) -> fulldat
fulldat[6573, "Tanggal.Lahir"] = '15-10-1986'
fulldat$umur = floor(as.numeric((Sys.Date()-dmy(fulldat$Tanggal.Lahir))/360))
#fulldat$umur = 2019 - as.numeric(paste0(19,str_sub(fulldat$Nomor.Induk.Kependudukan..NIK., start = -6, end = -5)))


fulldat[which(fulldat$umur>100),"umur"] = fulldat[which(fulldat$umur>100),"umur"]-100


save.image(file="caleg.RData") 

library(purrr)
library(tidyr)
library(ggplot2)


fulldat %>% 
  group_by(partai) %>% 
  summarise(avg = mean(umur,na.rm = T), min_age = min(umur, na.rm = T), max_age = max(umur, na.rm = T), n_tot = n(), n_na= sum(is.na(umur)))

fulldat$umur = cut(fulldat$umur, breaks = 10, dig.lab = 2)

fulldat %>%
  select(stringJenisKelamin, partai, noUrut, Agama, Status.Perkawinan, Jumlah.Anak, Pendidikan, Status.Khusus, umur) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat="count")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6))

for(pty in unique(fulldat$partai)){
  df = filter(fulldat, partai==pty)
  df = df %>%
    select(stringJenisKelamin, noUrut, Agama, Status.Perkawinan, Jumlah.Anak, Pendidikan, Status.Khusus, umur) %>%
    mutate(is_na = if_else(is.na(umur), 1, 0)) %>%
    gather()
    png(paste0('pict/',pty, '.png'), width = 1200, height = 868)
    print(ggplot(df, aes(value)) +
      facet_wrap(~ key, scales = "free") +
      #background_image(readPNG(paste0('../bendera/', pty, '.png')))+
      geom_histogram(stat = 'count')+
      theme_light()+
      theme(plot.title = element_text(size = 40, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1, size=12))+
      labs( title= paste("Partai", toupper(pty)), y="Total Caleg", x = "")
    )
    dev.off()
}

