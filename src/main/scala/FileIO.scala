import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._

object FileIO {

  implicit val formats: Formats = DefaultFormats
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

  def downloadFeed(url: String): List[Post] = {
    val source = Source.fromURL(url)
    val subreddit = url.split("/")(4)
    val content = source.mkString
    source.close()

    val data = parse(content)
    val posts = (data \ "data" \ "children").children

    for (item <- posts) yield {

      val title = (item \ "data" \ "title").extract[String]
      val selftext = (item \ "data" \ "selftext" ).extractOpt[String].getOrElse("")
      val createdUtc = (item \ "data" \ "created_utc").extract[Double].toLong
      val date = TextProcessing.formatDateFromUTC(createdUtc)
      val score = (item \ "data" \ "score").extractOpt[Int].getOrElse(0)
      val url = (item \ "data" \ "url" ).extractOpt[String].getOrElse("")

      Post(subreddit, title, selftext, date, score, url)
    }  
  }
}