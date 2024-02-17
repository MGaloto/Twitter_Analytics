

# Twitter Shiny App


<p>
<a href="https://rinterface.github.io/bs4Dash/" rel="nofollow"><img src="https://camo.githubusercontent.com/0c50278e279e08384b5a14b75112e4d65484105a9028e3ebe7c5192f438570a7/68747470733a2f2f72696e746572666163652e636f6d2f696e73742f696d616765732f627334446173682e737667" align="right" width="150" style="max-width: 100%;"></a>
</p>



### Contenido:
<br>
</br>

- [**Introduccion**](https://github.com/MGaloto/twitter_analytics#introduccion)
- [**Estructura**](https://github.com/MGaloto/twitter_analytics#estructura)
- [**Ejecucion**](https://github.com/MGaloto/twitter_analytics#ejecucion)
- [**Conclusion**](https://github.com/MGaloto/twitter_analytics#conclusion)


## Introduccion


<div style="text-align: right" class="toc-box">
 <a href="#top">Volver al Inicio</a>
</div>

<br>
</br>

Aplicacion de Twitter Analytics utilizando el framework [bs4Dash](https://rinterface.github.io/bs4Dash/). El Dashboard contiene la siguientes funcionalidades para interactuar con la API de Twitter y obtener Insights sobre alguna/s palabra/s en los ultimos Twitters para Argentina.


* :large_blue_circle: Usuario que con mas frecuencia utilizo la palabra buscada en los ultimos Twitters.
* :large_blue_circle: Que palabras acompañan la palabra buscada (Analisis de sentimiento).
* :large_blue_circle: Grafico de barras para observar la frecuencia de las distintas palabras que acompañan a la consultada.


<p align="center">
  <img width="650" height="450" src="images/tw.gif">
</p>



## Estructura


<div style="text-align: right" class="toc-box">
 <a href="#top">Volver al Inicio</a>
</div>

<br>
</br>


``` shell
. 
├── docker-compose.yaml
├── docker
├── app.R
└── images
```

- En `docker-compose.yaml` se encuentran configurados los puertos y la imagen que se va a utilizar para correr la app.
- En `docker` se encuentra el Dockerfile y los packages para la app.
- `app.R` es la Shiny App.
- En`images` se encuentran las imagenes del repositorio.


## Ejecucion


<div style="text-align: right" class="toc-box">
 <a href="#top">Volver al Inicio</a>
</div>

<br>
</br>

Para correr la app en un contenedor de Docker hay que utilizar el siguiente comando:

``` shell
docker-compose up -d
```

Este comando va a crear una instancia de una imagen (un contenedor) en el puerto que se encuentra configurado en el docker-compose.yaml. Si la imagen no esta descargada primero lo que va a hacer es descargarla y luego levantar el contenedor.

Para detener y eliminar el contenedor hay que correr: 

``` shell
docker-compose down
```


## Conclusion


<div style="text-align: right" class="toc-box">
 <a href="#top">Volver al Inicio</a>
</div>

<br>
</br>


Como pudimos observar la app contiene distintas funcionalidades para hacer un analisis de palabras y sentimientos en Twitter para Argentina. 

Utilizando la API de Twitter en R se pueden hacer los siguientes analisis:

* Obtener tweets: Se puede buscar y extraer tweets utilizando palabras clave, hashtags, nombres de usuario y otras consultas avanzadas. Esto permite recopilar datos específicos de tweets para su análisis.

* Analizar sentimientos: Se puede utilizar técnicas de procesamiento de lenguaje natural (NLP) para analizar el sentimiento de los tweets. Esto implica determinar si un tweet es positivo, negativo o neutro, lo que puede ser útil para el análisis de opiniones o la detección de tendencias.

* Análisis de tendencias: Se puede identificar las tendencias actuales en Twitter, tanto a nivel global como a nivel local. Esto permite descubrir los temas populares en tiempo real y analizar las conversaciones en torno a ellos.

* Interacciones de usuarios: Se puede obtener información sobre los seguidores, seguidos, menciones y retweets de usuarios específicos. Esto permite comprender las interacciones y relaciones entre los usuarios de Twitter.

* Publicar tweets: Se puede utilizar la API para publicar tweets en tu cuenta de Twitter desde R. Esto es útil para automatizar la publicación de contenido o interactuar con los usuarios.

* Recopilación de datos en tiempo real: Se puede utilizar la API de streaming de Twitter para recopilar datos en tiempo real sobre tweets que cumplan ciertos criterios. Esto es útil para el monitoreo continuo de eventos, seguimiento de menciones de marca o cualquier otra aplicación que requiera datos en tiempo real.

