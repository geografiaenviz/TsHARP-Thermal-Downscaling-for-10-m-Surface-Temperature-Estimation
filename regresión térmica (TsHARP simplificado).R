library(raster)

# =========================
#Codigo creado por Jorge Ibares (Geografia_en_viz)
#Este codigo busca crear una regresion térmica con el
#TsHARP para generar un downscale, se busca un metodo para
#obtener la temperatura superficial a 10m, Aun no disponible en 2026
#Recuerda que es un proxy, que relaciona la temperatura y las áreas verdes.
# CARGAR RASTERS
# =========================

MSAVI <- raster("C:/Users/super/OneDrive/Escritorio/002_SIG/Acuiferos_Qro/MSAVI_2025-05-03.tif")

Temperatura <- raster("C:/Users/super/OneDrive/Escritorio/002_SIG/Acuiferos_Qro/QroTemp_2024-02-03.tif")

# =========================
# AJUSTAR MSAVI A 30m
# (para entrenar modelo)
# =========================

MSAVI_30m <- aggregate(
  MSAVI,
  fact = round(res(Temperatura)[1] / res(MSAVI)[1]),
  fun = mean
)

# Alinear geometría
MSAVI_30m <- resample(MSAVI_30m, Temperatura, method = "bilinear")

# =========================
# CREAR DATAFRAME
# =========================

datos <- stack(Temperatura, MSAVI_30m)

datos_df <- as.data.frame(datos, na.rm = TRUE)

colnames(datos_df) <- c("Temp", "MSAVI")

# =========================
# MODELO DE REGRESIÓN
# =========================

modelo <- lm(Temp ~ MSAVI, data = datos_df)

summary(modelo)

# =========================
# APLICAR MODELO A 10m
# =========================

Temp_10m <- predict(
  MSAVI,
  modelo
)

# =========================
# AJUSTE DE MEDIA
# (conserva energía térmica)
# =========================

Temp_10m_mean <- cellStats(Temp_10m, mean)
Temp_original_mean <- cellStats(Temperatura, mean)

ajuste <- Temp_original_mean - Temp_10m_mean

Temp_10m <- Temp_10m + ajuste

# =========================
# VISUALIZAR
# =========================

plot(Temp_10m,
     main = "Temperatura Downscaling 10m")

# =========================
# EXPORTAR
# =========================

writeRaster(
  Temp_10m,
  filename = "C:/Users/super/OneDrive/Escritorio/002_SIG/Acuiferos_Qro/Temperatura_Downscale_10m_Precisa.tif",
  format = "GTiff",
  overwrite = TRUE
)