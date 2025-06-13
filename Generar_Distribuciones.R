set.seed(2025)
library(openxlsx)
library(tidyverse)

diametro_piston <- round(rnorm(1000, mean = 50, sd = 0.5), digits = 5)

resistencia_material <- rt(30, df = 5) * 10 + 100

tiempo_entre_fallos <- rexp(500, rate = 0.05)

germinacion_semillas <- rbinom(60, size = 20, prob = 0.75)

tamano_grano_arroz <- round(runif(900, min = 3, max = 5), digits = 2)

plagas_parcela <- rpois(70, lambda = 2)


# 1. Simulación de variables
set.seed(2025)
diametro_piston        <- round(rnorm(1000, mean = 50, sd = 0.5), 5)
resistencia_material   <- rt(30, df = 5) * 10 + 100
tiempo_entre_fallos    <- rexp(500, rate = 0.05)
germinacion_semillas   <- rbinom(60, size = 20, prob = 0.75)
tamano_grano_arroz     <- round(runif(900, min = 3, max = 5), 2)
plagas_parcela         <- rpois(70, lambda = 2)

# Variables del taller
peso_pieza_madera      <- rnorm(120, mean = 12, sd = 1)
numero_defectos        <- rpois(80, lambda = 3)
tiempo_ensamble_robot  <- rexp(60, rate = 0.1)

# 2. Preparar lista con datos, contexto y unidad
data_list <- list(
  diametro_piston = list(
    data    = diametro_piston,
    context = "Diámetro de pistones",
    unit    = "mm"
  ),
  resistencia_material = list(
    data    = resistencia_material,
    context = "Resistencia máxima de probetas",
    unit    = "MPa"
  ),
  tiempo_entre_fallos = list(
    data    = tiempo_entre_fallos,
    context = "Tiempo entre fallos de horno secador",
    unit    = "horas"
  ),
  germinacion_semillas = list(
    data    = germinacion_semillas,
    context = "Número de semillas germinadas de lote de 20",
    unit    = "semillas"
  ),
  tamano_grano_arroz = list(
    data    = tamano_grano_arroz,
    context = "Tamaño de granos de arroz",
    unit    = "cm"
  ),
  plagas_parcela = list(
    data    = plagas_parcela,
    context = "Conteo de plagas por parcela",
    unit    = "plagas"
  ),
  peso_pieza_madera = list(
    data    = peso_pieza_madera,
    context = "Peso de piezas de madera",
    unit    = "kg"
  ),
  numero_defectos = list(
    data    = numero_defectos,
    context = "Conteo de defectos en producción",
    unit    = "defectos"
  ),
  tiempo_ensamble_robot = list(
    data    = tiempo_ensamble_robot,
    context = "Tiempo de ensamble de robot",
    unit    = "segundos"
  )
)

# 3. Crear workbook y hojas
wb <- createWorkbook()

for (name in names(data_list)) {
  addWorksheet(wb, name)
  # Escribir contexto y unidad
  writeData(wb, name, paste0("Contexto: ", data_list[[name]]$context),
            startRow = 1, startCol = 1)
  writeData(wb, name, paste0("Unidad: ", data_list[[name]]$unit),
            startRow = 2, startCol = 1)
  # Escribir datos a partir de la fila 4
  writeData(wb, name,
            data.frame(value = data_list[[name]]$data),
            startRow = 4, startCol = 1,
            colNames = TRUE)
}

# 4. Guardar el archivo en la ruta actual
saveWorkbook(wb, file = "Datos.xlsx", overwrite = TRUE)

message("Archivo 'Datos.xlsx' creado exitosamente en la carpeta de trabajo.")



# Gráficas
hist(diametro_piston)


library(fitdistrplus)
library(nortest)

set.seed(12345)
edad_estudiantes <- round(rnorm(120, mean = 22, sd = 7), 4)

hist(edad_estudiantes)

descdist(edad_estudiantes, discrete = F)


fit_norm <- fitdist(edad_estudiantes, "norm")
summary(fit_norm)

res_lillie <- lillie.test(edad_estudiantes) 
print(res_lillie)


variable_exponencial <- rexp(120, 0.7)
hist(variable_exponencial)

var_test <- lillie.test(variable_exponencial)
print(var_test)

descdist(variable_exponencial, discrete = F)
