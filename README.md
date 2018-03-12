# Ajedrez a través de IRC

## Especificación del protocolo

**Nota:** La arquitectura de esta aplicación así como el protocolo de comunicación están inspirados en http://irc-chess.sourceforge.net/. El módulo `Chess` es autoría de Arno van Lumig: https://github.com/ArnoVanLumig/chesshs, pero fue modificado corrigiendo las funciones `stalmate` y `mate`.)

Este documento pretende describir el protocolo que rige la comunicación entre el _servidor de ajedrez_ (escrito en Haskell) y el _cliente IRC_ que actúa como bot.  
El servidor es quien implementa toda la lógica de negocio requerida por el juego de ajedrez. El cliente simplemente se encarga de: 

* Recoger las solicitudes de los jugadores en un canal IRC, digamos #foo, y enviarlas al servidor.
* Recibir las respuestas del servidor para luego mostrarlas en #foo.

Los mensajes enviados del cliente al servidor son descritos a continuación. Los mensajes del servidor al cliente son documentados después, junto con el código de colores empleado.

### Mensajes del cliente al servidor

1. 
```
SESSION <channel>
```

Mensaje enviado cada vez que se establece una conexión cliente-servidor. El canal `channel` estará asociado a esta sesión, debido a que será de donde se recojan los mensajes de los jugadores así como donde se desplieguen las respuestas del servidor. No habrá más de una sesión por canal al mismo tiempo. La sesión termina o bien después de que culmina la partida o bien después de un mensaje de `CLOSE`.

2. 
```
CLOSE
```

Cierra la conexión con el servidor.

3. 
```
REGISTER <nick>
```

Se registra el jugador `nick`. El primero que se registre será las blancas y el segundo las negras.

4. 
```
START <nick>
```

Comienza el juego. El jugador `nick` debe haberse registrado previamente a esa partida.

5. 
```
MOVE <nick> <move>
```

Realiza una jugada. El parámetro `nick` es el nick del jugador que realiza el movimiento, mientras `move` es su representación algebraica estándar (SAN).

6. 
```
DRAW <nick>
```

El jugador `nick` ofrece o acepta tablas, dependiendo del turno.

7. 
```
RESIGN <nick>
```

El jugador `nick` se retira del juego.


### Mensajes del servidor al cliente

Responden al siguiente formato:

```
<message><endmark> | <message><delimiter><history><delimiter><board><endmark>
```

donde cada uno de estos tokens tienen el siguiente significado:

```
<message>           ::=     <string>
                            {utilizado para mensajes, ej. "Blancas mueven."}

<board>             ::=     representación del tablero
                            {sigue el código de colores descrito al final de esta sección}

<delimiter>         ::=     "<:=:>"
                            {el delimitador es "<:=:>" sin las comillas}

<history>           ::=     la historia de las últimas jugadas
                            {por ejemplo, "1. e4"}

<string>            ::=     cualquier secuencia de caracteres excepto <delimiter>

<endmark>           ::=     "-###-"
                            {esta secuencia indica el fin de un mensaje}
```
