library(rvest)
library(dplyr)
library(stringr)
library(httr)

setwd('/home/ghilman/kpu_crawls/caleg/')

link = content(GET(URLencode("https://infopemilu.kpu.go.id/pileg2019/pencalonan/allparpol.json")), as="text")
smmry_parpol =  link %>% 
  jsonlite::fromJSON(.)
#write.csv(smmry_parpol, "summary_parpol.csv")

provinsi = read.csv("provinsi.csv")
# these null vars are used for index error finder
i_null = j_null = k_null = l_null = vector()
i=j=k=l=1
# these prog vars are used for running back the for loop if somehow error comes up
i_prog=1;j_prog=1;k_prog=1;l_prog=1
for(i in i_prog:nrow(provinsi)){
  possibleError = tryCatch({
    link = content(GET(URLencode(paste0('https://infopemilu.kpu.go.id/pileg2019/pencalonan/', provinsi$id[i] ,'/dcs-dpr.json'))), as="text")
    Sys.sleep(sample(seq(1, 3, by=0.001), 1))
    dapil = link %>% 
      jsonlite::fromJSON(.) %>%
      select(id, nama)
  }, error = function(e) e)
  
  if(inherits(possibleError, "error")){
    i_null = c(i_null, i)
    j_null = c(j_null, j)
    k_null = c(k_null, k)
    l_null = c(l_null, l)
    print(paste("error", i, j, k, l))
    next
  }
  for(j in j_prog:nrow(dapil)){
    for (k in k_prog:nrow(smmry_parpol)){
      possibleError = tryCatch({
        link = content(GET(URLencode(paste('https://infopemilu.kpu.go.id/pileg2019/pencalonan/pengajuan-calon', dapil$id[j], smmry_parpol$id[k], 'dcs', sep = '/'))), as="text")
        Sys.sleep(sample(seq(1, 3, by=0.001), 1))
        caleg_partai = link %>% 
          jsonlite::fromJSON(.) 
        if(length(caleg_partai)==0){
          next
        }
        caleg_partai$partai = smmry_parpol$akronimParpol[k]
        caleg_partai$Dapil = dapil$nama[j]
        caleg_partai$provinsi = provinsi$provinsi[i]
        
        write.table(arrange(caleg_partai, noUrut), paste('list_caleg/caleg_partai',smmry_parpol$akronimParpol[k], provinsi$id[i], dapil$id[j], provinsi$provinsi[i], dapil$nama[j], 'clean.csv', sep='_'), sep = '|', row.names = F, quote = F)
      }, error = function(e) e)
      
      if(inherits(possibleError, "error")){
        i_null = c(i_null, i)
        j_null = c(j_null, j)
        k_null = c(k_null, k)
        l_null = c(l_null, l)
        print(paste("error", i, j, k, l))
        next
      }
      for(l in l_prog:nrow(caleg_partai)){
        Sys.sleep(sample(seq(1, 3, by=0.001), 1))
        possibleError = tryCatch({
          link = content(GET(URLencode(paste0('https://infopemilu.kpu.go.id/pileg2019/pencalonan/calon/', caleg_partai$id[l]))))
          caleg = link %>% 
            html_nodes('div.ibox-content:nth-child(1)') %>%
            html_text(trim = T)
          
          if(length(caleg)==0){
            next
          }
          
          clean_caleg = caleg %>% 
            gsub(pattern = '\\s{2,}', replacement= '|') %>% 
            gsub(pattern = 'Gelar Akademis Depan\\|Gelar', replacement = 'Gelar Akademis Depan\\|\\|Gelar') %>%
            gsub(pattern = 'Gelar Akademis Belakang\\|Nama', replacement = 'Gelar Akademis Belakang\\|\\|Nama') %>%
            gsub(pattern = 'Motivasi\\|Target', replacement = 'Motivasi\\|\\|Target') %>%
            gsub(pattern = 'Target/Sasaran$', replacement = 'Target/Sasaran\\|\\|') %>%
            str_split(pattern = '\\|') %>% '[['(1)
          idx_target = grep('Target/Sasaran',clean_caleg)
          idx_motivasi = grep('Motivasi',clean_caleg)
          
          clean_caleg = c(clean_caleg[1:idx_target], paste0(clean_caleg[(idx_target+1):length(clean_caleg)], collapse = ' '))
          idx_target = grep('Target/Sasaran',clean_caleg)
          clean_caleg = c(clean_caleg[1:idx_motivasi], paste0(clean_caleg[(idx_motivasi+1):(idx_target-1)], collapse = ' '), clean_caleg[idx_target:(idx_target+1)])
          
          col = clean_caleg[c(T, F)]
          val = clean_caleg[c(F, T)]
          col_val = data.frame(col, val)
          write.table(col_val, paste('profil_caleg/profil_caleg',  caleg_partai$idDapil[l], caleg_partai$partai[l], caleg_partai$id[l], caleg_partai$noUrut[l], caleg_partai$Dapil[l], caleg_partai$provinsi[l], caleg_partai$nama[l], 'clean.csv', sep = "_"), sep = "|", row.names = F, quote = F)
          print(paste('profil_caleg',  caleg_partai$idDapil[l], caleg_partai$partai[l], caleg_partai$id[l], caleg_partai$noUrut[l], caleg_partai$Dapil[l], caleg_partai$provinsi[l], caleg_partai$nama[l],sep = "_"))
        }, error = function(e) e)
        
        if(inherits(possibleError, "error")){
          i_null = c(i_null, i)
          j_null = c(j_null, j)
          k_null = c(k_null, k)
          l_null = c(l_null, l)
          print(paste("error", i, j, k, l))
          next
        }
      }
      if(l==nrow(caleg_partai)) l_prog<<-1
      
    }
    if(k==nrow(smmry_parpol)) k_prog<<-1
    
  }
  if(j==nrow(dapil)) j_prog<<-1
  
}
