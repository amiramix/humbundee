

Introducción
=====

¿Qué es Humble Bundle?
-----
Es una tienda en línea donde puedes comprar paquetes de bienes digitales, como videojuegos, eBooks, audiolibros y cómics, ¡a un precio que tú mismo decides! Lo que pagas se divide en tres partes: una va a una organización benéfica, otra para pagar a los desarrolladores o autores, y la última como propina para Humble Bundle. Incluso puedes establecer las proporciones entre esas partes, decidiendo cuánto recibe cada parte de tu dinero. Más detalles: https://www.humblebundle.com/

¿Qué es un descargador de Humble Bundle?
-----
¡Algunos paquetes son grandes! ¡Quiero decir, ¡grandes! Si un paquete contiene 15 juegos en versiones para Windows, Linux y Mac, ya son 45 archivos. Y muy a menudo hay parches adicionales, bandas sonoras, paquetes extra y bonos. No es inusual que un paquete de cómics contenga más de 100 archivos descargables.

Ahora, probablemente no necesites todos esos archivos; puedes simplemente seleccionar la plataforma y el formato que más te interesen y descargar solo esos. Si es ese tu caso, probablemente no necesites un 'descargador' :-)

Para aquellos que prefieren tener los archivos que compraron en su disco, ya sea para un acceso más fácil o como respaldo, un 'descargador' es un programa que automatiza la descarga de esos archivos.

¿Qué es Humbundee?
-----
Sí, es un descargador de Humble Bundle. Es una aplicación escrita en Erlang que, dada una clave de Humble Bundle, descarga los archivos contenidos en ese paquete. Pero no solo eso. Recuerda las sumas de verificación MD5/SHA1 de todos los archivos descargados para no descargar archivos que ya hayan sido descargados previamente. Ah sí, y también permite definir patrones regex para omitir archivos específicos de la descarga si, por ejemplo, no te interesan los paquetes de Linux de 32 bits o los juegos con interfaz en alemán.

¿Por qué Humbundee?
-----
Hmmm. ¿Te refieres a por qué lo escribí o por qué deberías usarlo? :-) No es que haya una feroz competencia, simplemente no podía encontrar nada que tuviera remotamente las funciones que necesitaba. Entonces pensé que gastaría menos tiempo escribiéndolo que descargando todos los archivos en todos los paquetes que he comprado hasta ahora y compraré en el futuro.

Pero veamos qué es capaz de hacer Humbundee:

### Método de descarga

Humbundee descarga archivos utilizando `wget`, asegurándose de que los archivos se recuperen directamente desde su fuente. 

### Verifica sumas de verificación y tamaños
Todos los archivos descargados se verifican para comprobar si tienen el tamaño esperado y si sus sumas de verificación MD5/SHA1 coinciden con las anunciadas en la página del paquete (de hecho, las sumas SHA1 no se anuncian, pero están disponibles en los metadatos que describen el paquete).

### Genera registros útiles
Humbundee mantiene un registro exhaustivo por cada paquete descargado. No solo registra todos los archivos y torrents descargados en el paquete, sino que también registra todos los problemas encontrados y, al final, añade una única línea con el resumen de la descarga. Mira tú mismo:

    2016-02-06 20:29:45.731145> Finished all downloads, Expected: 45, Downloaded: 30, Ignored: 0, Excluded: 15, Errors: 0, Status: ok, closing log.

### Excluye archivos no deseados
Es posible que no quieras descargar paquetes de instalación para Mac OS X o Linux de 32 bits si no tienes esas plataformas. O quizás no hablas alemán y no quieres juegos que incluyan ese idioma. Humbundee te permite definir una lista de patrones regex para excluir descargas si el patrón aparece en un nombre, URL o plataforma asociada a una descarga específica.

### Ignora archivos ya descargados
A veces el mismo juego se incluye en más de un paquete. Eres lo suficientemente inteligente como para no descargar nuevamente juegos que ya descargaste de otros paquetes (si los recuerdas). Un descargador menos inteligente podría intentar descargar todo lo que le arrojes. Pero no Humbundee. Recuerda todos los archivos descargados, no por sus nombres, sino por sus tamaños y sumas de verificación MD5/SHA1, por lo que sabe cuándo un archivo ya se ha descargado previamente incluso si tiene un nombre diferente.

### Organiza meticulosamente todas las descargas
Si un paquete contiene varias decenas de archivos, no querrás que todos terminen en una sola carpeta. Humbundee organiza automáticamente todos los archivos descargados en una jerarquía de carpetas bajo la carpeta principal nombrada según la clave del paquete. Consulta la sección de configuración a continuación para más detalles.

-----

Resumen
=====

¿Qué se necesita?
-----

### Cualquier sistema tipo Unix
Con esto quiero decir que, por el momento, Humbundee probablemente no funcionará en Windows y, ciertamente, no se compilará en Windows. Podría funcionar en un entorno tipo Unix en Windows (cygwin, MSys2), pero eso no ha sido probado.

### Erlang
Instalado en una versión que soporte `maps`. Desarrollé Humbundee con Erlang 18, pero la 17 debería funcionar bien (aunque no lo verifiqué). Verifica si está instalado y en qué versión con:

    erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

### GNU make
Es un estándar en Linux y está disponible para su instalación en otras plataformas. En *BSD verifica con:

    which gmake

### git
Se necesita para clonar el repositorio en tu disco y también para obtener los scripts de compilación (se hace automáticamente). Se necesitará al menos la versión `1.6.x`.

### libyajl
Los encabezados de Libyajl deben estar instalados en tu sistema, se utilizan para analizar JSON.

### Instalación de Libyajl  

Para instalar Libyajl en tu sistema, sigue los pasos adecuados para tu sistema operativo:  

#### Ubuntu/Debian  
```bash
sudo apt update
```
``` bash
sudo apt install libyajl-dev 
```
```bash Fedora  
~sudo dnf install yajl-devel
```  

#### Arch Linux  
```bash
sudo pacman -S yajl
```  

#### macOS (usando Homebrew)  
```bash
brew install yajl
```  

#### FreeBSD  
```sh
pkg install yajl
``` 

Después de instalar, asegúrate de que los encabezados (`yajl/yajl_parse.`) estén disponibles en la ruta de inclusión de tu sistema.


### wget
También está ampliamente disponible para su instalación desde un paquete. Verifica si está instalado y disponible en la ruta con:

    wget --help

### cookie
Debes iniciar sesión en Humble Bundle para descargar archivos de tu paquete. Pero actualmente Humbundee no te permite iniciar sesión. En su lugar, espera que inicies sesión usando un navegador normal y exportes una cookie que luego pueda usar para acceder a la parte del sitio web a la que solo tú estás autorizado a acceder. La cookie debe estar en un formato compatible con `wget`. Uso [esta extensión](https://chromewebstore.google.com/detail/get-cookiestxt-locally/cclelndahbckbenkjhflpdbgdldlbecc) para exportar la cookie desde Chrome.

¿Cómo funciona?
-----

Cuando le pides que descargue un paquete, Humbundee descarga un archivo `JSON` que describe el paquete e inicia un nuevo `gen_server` de Erlang llamado `hbd_id` para procesar el archivo. Humbundee puede descargar múltiples paquetes simultáneamente. Cada `hbd_id` genera un proceso de Erlang por cada posición del paquete, que generalmente es un juego, eBook o banda sonora. Luego, cada uno de esos procesos genera un nuevo proceso de Erlang por cada archivo que se puede descargar. Por lo general, hay varios archivos por posición del paquete, por ejemplo, versiones Linux, Windows y Mac de un juego, o versiones EPUB, PDF y CBZ de un eBook. 

Así, en este momento tenemos un proceso de Erlang por cada archivo que se puede descargar. Para simplificar, concentremos la atención en lo que hace un solo proceso.

Primero, verifica si el archivo se puede descargar en absoluto. Algunos archivos, como transmisiones o aplicaciones JavaScript incrustadas, no se pueden descargar. Por lo tanto, el proceso finaliza si el archivo no se puede descargar. Segundo, verifica si el archivo ya ha sido descargado, y finaliza si es así. Tercero, verifica si el archivo debe ser excluido, es decir, si alguno de los patrones regex coincide, y finaliza si es así. 

Si todo está bien, el proceso intenta descargar el archivo torrent con `wget`. Si el archivo torrent no existe, no se puede descargar, es incorrecto o no se puede analizar, entonces el proceso descarta la información sobre el archivo torrent y el archivo se descargará como si el torrent no existiera.

Ten en cuenta que hasta este punto, todo el procesamiento se ha realizado en paralelo, incluida la descarga de archivos con `wget`. Si bien descargar muchos archivos pequeños simultáneamente no es un gran problema, manejar varias descargas grandes a la vez puede consumir muchos recursos. Para gestionar esto de manera eficiente, la descarga real es manejada por un `gen_server` separado llamado `hbd_pool`, que coordina las descargas de `wget` para asegurar una recuperación de archivos controlada y eficiente.  


Cada proceso que no ha finalizado hasta ahora añade su descarga a `hbd_pool` y espera hasta que la descarga se complete. La descarga no se inicia inmediatamente, sino que `hbd_pool` los añade a una cola y comienza cuando la cantidad de descargas activas es menor que el umbral especificado. Las descargas activas se inician con `wget`.

Todas las descargas son manejadas por `wget`, que guarda los archivos directamente en la carpeta `temp` mientras están en progreso. La carpeta `temp` también es utilizada por `hbd_pool` para almacenar versiones temporales de archivos que se están descargando. Una vez que una descarga se completa, el archivo se mueve a la carpeta `downloads`.  

Cada dos segundos, `hbd_pool` realiza tres verificaciones importantes:

 1. Lista las descargas completadas en la carpeta `downloads`. Cada descarga completada se mueve a la carpeta de destino construida a partir de las propiedades de la descarga, es decir: `<key>/<publisher>/<title> - <platform>`. 
 2. Inicia nuevas descargas desde la cola hasta el máximo permitido de descargas activas.
 3. Lista las descargas pendientes en la carpeta `temp` si el mapa con descargas activas aún no se ha vaciado. La carpeta `temp` estará vacía cuando todas las descargas de `wget` hayan terminado. Si algunos archivos permanecen en la lista de descargas activas pero no han progresado en más de 10 segundos, `hbd_pool` los considera obsoletos y reinicia la descarga usando `wget`.  


Reiteremos el último punto. Cuando la carpeta `temp` está vacía pero el mapa que contiene todas las descargas activas no lo está, significa que algunos archivos se han añadido pero no se están descargando activamente. Si esos archivos han estado en la lista durante más de 10 segundos sin progreso, se consideran obsoletos y se reintentarán usando `wget`.  

Cada descarga siempre incluye una URL HTTP/S directa, asegurando que `wget` pueda recuperar el archivo de manera confiable. Si un archivo no está progresando, eliminar el archivo temporal de la carpeta `temp` hará que `hbd_pool` lo detecte como obsoleto y reinicie la descarga usando `wget`.  

Ahora volvamos a los procesos iniciados por `hbd_id` que aún están esperando a que se completen las descargas. Por cada archivo que `hbd_pool` mueve desde la carpeta `downloads` a la carpeta de destino, también envía una respuesta de vuelta al proceso que está esperando, el cual añadió inicialmente el archivo a `hbd_pool`. Cada `hbd_id` recuerda cuántos procesos se han iniciado para descargar archivos en el paquete dado. La descarga del paquete termina cuando todos esos procesos finalizan después de recibir un OK de que el archivo se ha descargado correctamente o un error. Cuando no queda ningún proceso hijo, `hbd_id` añade un resumen al archivo de registro, luego el archivo de registro se cierra y `hbd_id` también finaliza.

Breve nota sobre el indexado
-----
Cada archivo contenido en un paquete y procesado por Humbundee se añade a un Índice. El Índice es simplemente una tabla mnesia que contiene [registros `idx`](https://github.com/amiramix/humbundee/blob/master/lib/humbundee/include/humbundee.hrl#L25). La tabla se indiza por la clave del paquete y las sumas de verificación MD5 y SHA1 del archivo. Además, almacena el tamaño del archivo y los metadatos JSON disponibles del archivo descargado. Puedes consultar las entradas en la tabla con `humbundee:read/1` y eliminarlas con `humbundee:delete/1`.

Todos los archivos descargados se añaden automáticamente al índice. Sin embargo, también es posible indexar archivos contenidos en un paquete sin descargarlos. Esto será útil si ya has descargado un paquete manualmente pero aún quieres añadirlo al índice para que Humbundee no intente descargar sus archivos nuevamente en caso de que también estén presentes en otro paquete. Para ello puedes usar `humbundee:index/1` (consulta la sección Comandos disponibles a continuación).

Esto también se puede usar con fines de prueba, es decir, para verificar que los patrones de exclusión funcionan como se espera. Para ello podrías seguir los siguientes pasos:

 1. Configura Humbundee (añade los patrones de exclusión deseados, configura carpetas, etc).
 2. Inicia Humbundee
 3. Indexa el paquete con `humbundee:index/1`
 4. Revisa los registros de descarga en busca de errores y también para verificar si los archivos deseados han sido excluidos
 5. Elimina la carpeta de descarga desde la carpeta `destination_dir`
 6. Elimina el paquete del índice con `humbundee:delete/1` y repite

Ten en cuenta que `hbd_idx` solo se preocupa por archivos duplicados y no intenta indexar todos los archivos descargados. Por lo tanto, si el mismo archivo con la misma suma de verificación MD5/SHA1 está contenido en múltiples paquetes con posiblemente diferentes nombres, títulos, editores e IDs de paquete, solo se almacenará una versión de ese archivo en el índice con un conjunto específico de información de metadatos.

-----

Todo sobre la configuración
=====

Configurar Humbundee
-----

La configuración principal y por defecto está en el archivo [humbundee/etc/sys.config.src](https://github.com/amiramix/humbundee/blob/master/etc/sys.config.src).

Cuando Humbundee se inicia, intenta adicionalmente leer el archivo `.humbundee.conf` desde el directorio principal del usuario, es decir: `~/.humbundee.conf`. Luego, para cada opción encontrada en el archivo de configuración del usuario, reemplaza el valor por defecto con el definido por el usuario. En otras palabras, las opciones del usuario tienen prioridad, pero si no están definidas se utilizan los valores por defecto.

### Ejemplo de un archivo de configuración de usuario:

    %% -*- erlang -*-
    {cookie,          <<"/home/user/Downloads/cookies.txt">>}.
    {download_dir,    <<"/home/user/archive/downloads">>}.
    {temp_dir,        <<"/home/user/archive/temp">>}.
    {destination_dir, <<"/home/user/archive/humblebundle">>}.
    {exclude_regex_list,
     [
      {name, <<"^MOBI$">>, [caseless]},
      {name, <<"^32-bit">>},
      {name, <<"32-bit$">>},
      {name, <<".rpm$">>},
      {name, <<"^720p$">>},
      {name, <<"^German$">>, [caseless]},
      {platform, <<"ebook">>}
     ]}.
    {workers, 20}.

El significado de estas opciones es el siguiente:

#### `cookie`
Ubicación del archivo de cookie

#### `download_dir`
Carpeta donde se almacenan las descargas completadas. Humbundee monitorea esta carpeta y mueve los archivos completados a una subcarpeta apropiada en `destination_dir`.

#### `temp_dir`
Carpeta donde `wget` almacena las descargas incompletas.

#### `workers`
El número máximo de descargas concurrentes que pueden estar activas al mismo tiempo está limitado por este umbral. La suma de descargas que `hbd_pool` inicia con `wget` no puede superar este límite.  

#### `destination_dir`
Una carpeta principal para paquetes completados. Todos los archivos en paquetes completados se almacenan en subcarpetas nombradas según el siguiente patrón:

    Key
      .etr, .json, .log
      *_data
        *- Publisher
          *- Title - platform

Donde:

 * `Key` es la clave del paquete, un conjunto alfanumérico de caracteres asociado con cada paquete.
 * `.etr`, `.json`, `.log` son archivos generados al descargar el paquete.
 * `_data` es la carpeta principal para todos los archivos descargados.
 * `Publisher` es generalmente la empresa detrás de los bienes digitales, juegos, cómics, etc.
 * `Title` es el título del elemento digital, un título de juego, un título de libro, etc.
 * `Platform` es una de android, linux, windows, comics, comedy, ebook, audio, etc.

#### `exclude_regex_list`

Lista de tuplas `{Match, Pattern}` o `{Match, Pattern, Options}` donde:

 * `Match` - una de `name`, `link`, `platform` y `any`.
 * `Pattern` - la variable `Regexp` aceptada por [re:compile/2](http://erlang.org/doc/man/re.html#compile-2)
 * `Options` - lista de opciones aceptadas por [re:compile/2](http://erlang.org/doc/man/re.html#compile-2) o [re:run/4](http://erlang.org/doc/man/re.html#run-3)

Humbundee intenta hacer coincidir `Pattern` contra el contenido de campos específicos en el archivo JSON según `Match` y excluye el archivo de la descarga si la coincidencia es exitosa. Los siguientes campos en el archivo JSON de Humble Bundle se comparan cuando `Match` es:

 * `name` - campos: folder (beneficiario, generalmente el editor), title (human_name, generalmente el nombre de la posición en la página del paquete), machname (una concatenación abreviada del título y la plataforma), name (texto en el botón de descarga)
 * `link` - campos: urls web y bittorrent
 * `platform` - campo: platform
 * `any` - todo lo anterior, el archivo se excluye si cualquiera de esos campos coincide

La lista `Options` puede contener opciones aceptadas por ambos, [re:compile/2](http://erlang.org/doc/man/re.html#compile-2) y [re:run/4](http://erlang.org/doc/man/re.html#run-3). Humbundee utiliza las aceptadas por [re:compile/2](http://erlang.org/doc/man/re.html#compile-2) al compilar la expresión regular y las filtra para que cuando [re:run/4](http://erlang.org/doc/man/re.html#run-3) se llame más tarde para verificar la coincidencia, solo se le pasen las opciones aceptadas por [re:run/4](http://erlang.org/doc/man/re.html#run-3).



### Configurar la carpeta `download_dir`

En la pestaña **Descargas** edita _Guardar archivos en la ubicación:_. Debe tener el mismo valor que `download_dir` configurado para Humbundee en la sección anterior.

### Configurar la carpeta `temp_dir`

En la misma pestaña **Descargas** edita _Mantener torrents incompletos en:_. Debe tener el mismo valor que `temp_dir` configurado para Humbundee en la sección anterior.



Otras opciones, especialmente las de las pestañas **Conexión** y **Velocidad**, pueden configurarse para adaptarse a la velocidad de tu enlace de red. No afectan a Humbundee de ninguna manera.

--------------------------------------

Los detalles prácticos: cómo usar Humbundee
=====

¡Empecemos!
-----

### Instalar requisitos previos


Instala Erlang, GNU make, git wget,yajl.

### Descargar la cookie

Inicia sesión en Humble Bundle y descarga la cookie en un formato compatible con `wget`. [Esta extensión](https://chrome.google.com/webstore/detail/cookiestxt/njabckikapfpffapmjgojcnbfjonfjfg) puede usarse para exportar la cookie desde Chrome.

### Configurar Humbundee

En lugar de actualizar la configuración por defecto [humbundee/etc/sys.config.src](https://github.com/amiramix/humbundee/blob/master/etc/sys.config.src) es más fácil crear un nuevo archivo `.humbundee.conf` en el directorio principal del usuario, es decir: `vi ~/.humbundee.conf`.

Consulta la sección **Todo sobre la configuración** anterior para más detalles.



### Compilar Humbundee

Nota1: usa `gmake` (GNU make) en lugar de `make` en sistemas *BSD.  
Nota2: `make dev` debe ejecutarse después de que `make get-deps` haya finalizado, `make get-deps dev` no funcionará.

    git clone https://github.com/amiramix/humbundee.git
    cd humbundee/
    make get-deps
    make dev

### Instalar Humbundee

    ./bin/init.esh

### Iniciar Humbundee

    ./bin/start.esh

### Abrir la consola de Erlang

    to_erl ../hbd/shell/

### Iniciar y gestionar descargas

Humbundee se gestiona desde la consola de Erlang llamando a las funciones expuestas en el módulo [humbundee.erl](https://github.com/amiramix/humbundee/blob/master/lib/humbundee/src/humbundee.erl). Consulta la siguiente sección para más detalles.

### Detener Humbundee (y cerrar la VM)

    q().

Esto debe ejecutarse en la consola de Erlang, por supuesto. Alternativamente, en otra consola, ejecuta en la carpeta raíz de Humbundee:

    ./bin/stop.esh


Comandos disponibles
-----

Los siguientes comandos pueden ejecutarse en la consola de Erlang para iniciar y controlar descargas. Todos los comandos aceptan cadenas o binarios.

### `humbundee:download/1`

    humbundee:download(Key) -> ok | {error, Error}

Tipos:

    Key = binary() | string()
    Error = any()

Inicia la descarga del paquete con la clave dada. La función genera un nuevo proceso para manejar la descarga y devuelve inmediatamente. Puedes iniciar la descarga de otro paquete en cualquier momento, todas las descargas se manejan en paralelo. Por favor usa `humbundee:status/1` para obtener información sobre el estado de la descarga de un paquete en particular y `humbundee:status/0` para obtener información sobre el estado de la cola de descargas `hbd_pool`.

### `humbundee:index/1`

    humbundee:index(Key) -> ok | {error, Error}

Tipos:

    Key = binary() | string()
    Error = any()

Funciona casi exactamente como `humbundee:download/1`, sin embargo, cada proceso de descarga finaliza justo antes de que el archivo se añadiría a la cola de descargas. El propósito principal de esta función es añadir un paquete al índice sin descargar realmente los archivos contenidos en el paquete. Esto es útil si ya has descargado un paquete pero aún quieres que los archivos contenidos en el paquete sean indexados para que Humbundee no intente descargar esos archivos si se distribuyen nuevamente en otro paquete. Los archivos indexados en un paquete en particular se pueden consultar usando `humbundee:read/1` y eliminarse del índice usando `humbundee:delete/1`.

### `humbundee:status/0`

    humbundee:status() -> [{downloading, Keys}, {bad, Bad}, {waiting, Waiting}, {wget, Pids}]

Devuelve el estado de la cola de descargas en `hbd_pool`.

Donde:

 * `Keys` - Lista de descargas activas: archivos que se están descargando actualmente.
 * `Bad` - Lista de archivos descargados con un error. Esos archivos no se mueven a la carpeta de destino, sino que se dejan en la carpeta `downloads` para una inspección manual posterior.
 * `Waiting` - Cantidad de descargas esperando en la cola: archivos que aún no han comenzado a descargarse.
 * `Pids` - Lista de procesos que actualmente están descargando archivos con `wget`.

### `humbundee:status/1`

    humbundee:status(Key) -> [{id, Id}, {cookie, Cookie}, {out, Out}, {count, Count},
     {stats, {OK, Ignored, Excluded, Error}}, {pids, Pids}]

Tipos:

    Key = binary() | string()

Devuelve el estado de la descarga de un paquete con la clave especificada.

Donde:

 * `Id` - La clave del paquete
 * `Cookie` - Ubicación del archivo de cookie
 * `Out` - Raíz del directorio de descarga para archivos en este paquete
 * `Count` - Cantidad de descargas esperadas para este paquete
 * `OK` - Cantidad de archivos descargados con éxito hasta ahora
 * `Ignored` - Cantidad de archivos ignorados hasta ahora
 * `Excluded` - Cantidad de archivos excluidos hasta ahora
 * `Error` - Cantidad de descargas completadas con un error hasta ahora
 * `Pids` - Procesos restantes que aún están procesando descargas o que añadieron sus descargas a `hbd_pool` y están esperando a que se completen las descargas

### `humbundee:read/1`

    humbundee:read(Id) -> [#idx{}]

Tipos:

    Id = binary() | string()

Devuelve una lista de [registros idx](https://github.com/amiramix/humbundee/blob/master/lib/humbundee/include/humbundee.hrl#L25) para los cuales el Id proporcionado coincidió con uno de los siguientes: clave del paquete, suma de verificación MD5 del archivo, suma de verificación SHA1 del archivo.

La función reconoce automáticamente si el Id proporcionado es una suma de verificación o una clave de paquete y actúa en consecuencia. Para MD5/SHA1 generalmente devolverá una lista con un elemento: el archivo para el cual coincide el MD5/SHA1. A menos que no exista tal archivo, en cuyo caso devolverá una lista vacía. Para las claves de paquete devolverá una lista con todos los elementos pertenecientes a ese paquete.

### `humbundee:delete/1`

    humbundee:delete(Id) -> [Result]

Tipos:

    Id = binary() | string()

Funciona exactamente como `humbundee:read/1`, pero en lugar de devolver los registros leídos, los elimina y devuelve una lista que contiene los resultados de la eliminación de esos registros: un resultado por cada registro eliminado.

Ejemplos
-----

    humbundee:index(<<"U6tNd3hXZUT8uCg2">>).
    humbundee:delete("U6tNd3hXZUT8uCg2").
    humbundee:download("U6tNd3hXZUT8uCg2").
    humbundee:status(<<"U6tNd3hXZUT8uCg2">>).
    humbundee:status().
    humbundee:read("U6tNd3hXZUT8uCg2").
    humbundee:read("8b6f1882f93f3c5009900e09759fbcb3e8f04deb").
    humbundee:delete(<<"5029763c40d31842637a5e8f4165a245">>).

Donde:

    U6tNd3hXZUT8uCg2 - clave del paquete
    8b6f1882f93f3c5009900e09759fbcb3e8f04deb - suma de verificación SHA1
    5029763c40d31842637a5e8f4165a245 - suma de verificación MD5

Solución de problemas
-----

El comando `./bin/init.esh` instala Humbundee en la carpeta `../hbd/`. Así, la carpeta que contiene las fuentes de Humbundee y `hbd` se convierten en hermanas. La carpeta `hbd` se llama **raíz del nodo**. Contiene archivos generados por el nodo cuando se inicia y descarga paquetes, y también contendrá los archivos descargados si no has cambiado la configuración por defecto. Una de las carpetas que será útil para la solución de problemas es `hbd/log`. Especialmente `yolf.log` contendrá la configuración inicial y cualquier error encontrado al iniciar el nodo. Los fallos también se registrarán en los registros de `lager`. Todos los mensajes impresos por la VM de Erlang durante el proceso de arranque se registrarán en los registros `erlang.log.*`.
