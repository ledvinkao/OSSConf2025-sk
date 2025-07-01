xfun::pkg_attach2("tidyverse", # podmínkou je nainstalovaný minimálně balíček xfun
                  "terra", # pro práci s rastrovými geodaty
                  "RCzechia")

kraje <- kraje() |> 
  as_tibble() |> # toto je pro převod na lepší formát
  st_sf()

r <- rast(dir("geodata",
              pattern = "^SSP.*\\.asc$",
              recursive = T,
              full.names = T))

crs(r) <- "epsg:32633"

vahy <- r |> 
  cellSize()

kraje <- kraje |> 
  select(kraj = NAZ_CZNUTS3)

r2 <- r * vahy

names(r2) <- names(r)

kraje <- extract(r2,
                 kraje,
                 fun = sum,
                 bind = T,
                 na.rm = T) |> 
  st_as_sf() |> 
  as_tibble() |> 
  st_sf()

kraje <- extract(vahy,
                 kraje,
                 fun = sum,
                 bind = T,
                 na.rm = T) |> 
  st_as_sf() |> 
  st_drop_geometry() |> 
  as_tibble()

kraje <- kraje |> 
  pivot_longer(cols = starts_with("SSP"),
               names_to = "vrstva",
               values_to = "val_num") |> 
  mutate(val_num = val_num / area,
         val_num = round(val_num, 1))

kraje

refer <- dir("geodata",
             pattern = "^1991.*\\.asc$",
             full.names = T) |> 
  map(\(x) rast(x))

rozsah <- refer[[2]] |> 
  ext()

rozsah2 <- refer[[1]] |> 
  ext()

refer <- refer |> 
  map(\(x) crop(x, rozsah)) |> 
  map(\(x) crop(x, rozsah2))

refer <- refer |> 
  map(\(x) {ext(x) <- rozsah
  return(x)}) |> 
  rast()

crs(refer) <- "epsg:32633"

vahy2 <- cellSize(refer)

refer2 <- refer * vahy2

names(refer2) <- names(refer)

refer2

kraje2 <- kraje() |> 
  select(kraj = NAZ_CZNUTS3)

kraje2 <- extract(refer2,
                  kraje2,
                  fun = sum,
                  bind = T,
                  na.rm = T) |> 
  st_as_sf() |> 
  as_tibble() |> 
  st_sf()

kraje2 <- extract(vahy2,
                  kraje2,
                  fun = sum,
                  bind = T,
                  na.rm = T) |> 
  st_as_sf() |> 
  st_drop_geometry() |> 
  as_tibble()

kraje2

kraje2 <- kraje2 |> 
  pivot_longer(cols = 2:3,
               names_to = "vrstva",
               values_to = "val_num") |> 
  mutate(val_num = val_num / area,
         val_num = round(val_num, 1))

kraje <- kraje |> 
  mutate(klic1 = str_split_i(vrstva, # velmi šiková funkce na vytáhnutí části textu
                             "_",
                             2))

kraje2 <- kraje2 |> 
  mutate(klic2 = str_split_i(vrstva,
                             "_",
                             2))

kraje <- kraje |> 
  select(-area) |> # zbavujeme se zbytečné plochy
  left_join(kraje2 |> 
              select(-area), # zde také (dokonce uvnitř join funkce)
            join_by(kraj,
                    klic1 == klic2)) |> # využíváme celkem nového pomocníka join_by()
  mutate(val_num = if_else(klic1 == "T", val_num.x - val_num.y, val_num.x / val_num.y * 100), # lze nastavit i podmínku ve funkci mutate()
         val_num = round(val_num, 1))

# prohlížíme výsledek
kraje