library(googlesheets4)

# agregamos lo siguinte para que no intente
# autenticarse con una cuenta de google
gs4_deauth()
url = "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit?pli=1&gid=580479207#gid=580479207"
datos <- read_sheet(url, sheet = 2, skip = 1)

str(datos)
