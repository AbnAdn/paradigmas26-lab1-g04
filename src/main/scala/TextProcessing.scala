import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter

object TextProcessing {

  def formatDateFromUTC(timestamp: Long): String = {
    // convierte el número en un momento en el tiempo
    val instant = Instant.ofEpochSecond(timestamp)
    // como quiero mostrar la fecha
    val formatter = DateTimeFormatter
      .ofPattern("yyyy-MM-dd HH:mm:ss")
      .withZone(ZoneId.systemDefault())
    // devuelvo un string
    formatter.format(instant)
  }

}