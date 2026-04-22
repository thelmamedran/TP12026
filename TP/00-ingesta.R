library(googlesheets4)
library(tidyverse)
library(ggplot2)

# agregamos lo siguinte para que no intente
# autenticarse con una cuenta de google
gs4_deauth()
url = "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit?pli=1&gid=580479207#gid=580479207"

