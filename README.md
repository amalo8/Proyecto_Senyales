# Clasificador de Imágenes del Cielo

Este trabajo consiste en el desarrollo e implementación de una herramienta capaz de determinar el momento del día y las condiciones meteorológicas a partir de imágenes del cielo.
A diferencia de los enfoques modernos basados en *Deep Learning*, este proyecto prioriza la **extracción manual de características** (color, texturas, histogramas) y el uso de algoritmos de **Machine Learning clásico (Random Forest)** para garantizar la interpretabilidad de los resultados.

## Objetivos

El sistema resuelve dos problemas de clasificación distintos:
1.  **Clasificación Temporal:** Determinar el momento del día (Día/Noche/Crepúsculo).
2.  **Clasificación Meteorológica:** Evaluar condiciones atmosféricas (Despejado/Nublado).

## Estructura del Repositorio

A continuación se detalla el contenido de este repositorio:

├── figuras/                               # Gráficos generados para el informe (boxplots, matrices, etc.)
├── imagenes/                              # Dataset de Imágenes Originales
│   ├── test/                              # Imágenes de prueba (fotos propias)
│   │   ├── Meteorológico/                 
|   |       ├── Despejado/                 
│   │       └── Nublado/   
│   │   └── Temporal/                      
|   |       ├── Día/                 
|   |       ├── Noche/                 
│   │       └── Crepúsculo/ 
│   └── train/                             # Imágenes de entrenamiento (Kaggle/Unsplash)
│   │   ├── Meteorológico/                 
|   |       ├── Despejado/                 
│   │       └── Nublado/   
│   │   └── Temporal/                      
|   |       ├── Día/                 
|   |       ├── Noche/                 
│   │       └── Crepúsculo/ 
├── temporal/                              # Scripts de experimentación temporal
│   ├── imagenes_escaladas/                # Imágenes de test reescaladas (224px)
│   ├── test/                              # Imágenes de prueba (fotos propias)
│   │   └── Temporal/                      
|   |       ├── Día/                 
|   |       ├── Noche/                 
│   │       └── Crepúsculo/ 
│   ├── dia_noche.Rmd                      # Entrenamiento del modelo binario
│   ├── dia_noche_crep.Rmd                 # Entrenamiento del modelo multiclase
│   └── test.Rmd                           # Script de testeo y generación de métricas
├── meteo/                                 # Problema meteorológico
│   ├── imagenes_recortadas/               # Imágenes recortadas con algoritmo preprocesado (ROI)
│   ├── test/                              # Imágenes de prueba (fotos propias)
│   │   ├── Meteorológico/                 
|   |       ├── Despejado/                 
│   │       └── Nublado/   
│   └── train/                             # Imágenes de entrenamiento (Kaggle/Unsplash)
│   │   ├── Meteorológico/                 
|   |       ├── Despejado/                 
│   │       └── Nublado/   
│   └── despejado_nublado.Rmd              # Entrenamiento y test del modelo meteorológico
├── modelos/                               # Modelos Random Forest ya entrenados (.RData)
│   ├── rf_dia_noche.RData                 # Modelo Binario (Día vs Noche)
│   ├── rf_dia_noche_crep.RData            # Modelo Multiclase (+Crepúsculo)
│   ├── rf_despejado_nublado.RData         # Modelo Meteorológico (Despejado vs Nublado)
│   └── rf_..._pca.RData                   # Variaciones de modelos usando PCA
├── Memoria.pdf                            # Documento final del proyecto (Entregable)
├── Memoria.Rmd                            # Código fuente del informe en RMarkdown
├── Proyecto_Senyales.Rproj                # Archivo de proyecto de RStudio
├── README.md                              # Este archivo
├── apa.csl                                # Estilo de citas para la bibliografía
├── bibliografia.bib                       # Archivo de referencias bibliográficas
├── brainstorming.txt                      # Notas e ideas previas
├── funciones_extraccion_caracteristicas.R # Script con las funciones de extracción de características
└── preprocesado.Rmd                       # Script para el escalado y recorte de imágenes
