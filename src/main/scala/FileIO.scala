import scala.io.Source

object FileIO {

  def readFile(path: String): String = {
    // Crea la conexion con el archivo
    val source = Source.fromFile(path)
    // lo lee
    try source.getLines().mkString("\n")
    // y finalmente libera el recurso
    finally source.close()
  }

  def readSubscriptions(): List[Subscription] = {
    val json = readFile("subscriptions.json")

    // parser
    val entries = json
      .replace("[", "")
      .replace("]", "")
      .split("\\},")
      .toList

    entries.map { entry =>
      val clean = entry.replace("{", "").replace("}", "").trim

      val fields = clean.split(",").map { pair =>
        val parts = pair.split(":", 2)

        val key = parts(0).trim.replace("\"", "")
        val value = parts(1).trim.replace("\"", "")

        key -> value
      }.toMap

      Subscription(fields("name"), fields("url"))
    }
  }

  def downloadFeed(url: String): String = {
    val source = Source.fromURL(url)
    try source.mkString
    finally source.close()
  }
}