import scala.io.Source
import scala.util.Try
import org.json4s._
import org.json4s.jackson.JsonMethods._

object FileIO {

  implicit val formats: Formats = DefaultFormats
  //con option manejamos el error como 
  def readFile(path: String): Option[String] = {
    Try {
      // Crea la conexion con el archivo
      val source = Source.fromFile(path)
      // lo lee
      try source.getLines().mkString("\n")
      // y finalmente libera el recurso
      finally source.close()
    }.toOption
  }
    

  def readSubscriptions(): List[Subscription] = {
    val maybeJson = readFile("subscriptions.json")

    //vemos si es o no un json
    maybeJson match {
      case Some(json) =>
        // parser
        val entries = json
          .replace("[", "")
          .replace("]", "")
          .split("\\},")
          .toList
    
        //usamos flatMap para facilitar las cosas al momento de sacar los None de la lista
        entries.flatMap { entry =>
            val clean = entry.replace("{", "").replace("}", "").trim

            if (clean.isEmpty) None
            else {

              val fields = clean.split(",").map { pair =>
                val parts = pair.split(":", 2)

                val key = parts(0).trim.replace("\"", "")
                val value = parts(1).trim.replace("\"", "")

                // En caso de que el split no tenga 2 partes, protegemos el value
                val valor = if (parts.length > 1) parts(1).trim.replace("\"", "") else ""
                key -> valor
              }.toMap

              // MANEJO DE CAMPOS FALTANTES:
              // Usamos .get("clave") que devuelve un Option, en lugar de ("clave") que lanza error
              val nameOpt = fields.get("name")
              val urlOpt = fields.get("url")

              // Solo creamos la Subscription si AMBOS campos existen
              (nameOpt, urlOpt) match {
                case (Some(name), Some(url)) => Some(Subscription(name, url))
                case _ => None // Si falta alguno, devuelve None y flatMap lo ignora
              
              }
            }
        }
      case None =>
        // Si el archivo no existía o falló la lectura, simplemente devolvemos una lista vacía
        List.empty[Subscription]
            
    }
  }

  def downloadFeed(url: String): Option[List[Post]] = {
    //usamos option para manejar errores, encapsulamos el bloque Try con Option
    Try {
      val source = Source.fromURL(url)
      val subreddit = url.split("/")(4)
      val content = try source.mkString finally source.close()

      val data = parse(content)
      val posts = (data \ "data" \ "children").children

      // usamos flatMap para un mejor control sobre los posibles None
      posts.flatMap { item =>
      // encadenamos los Options obligatorios
        for {
          title      <- (item \ "data" \ "title").extractOpt[String]
          createdUtc <- (item \ "data" \ "created_utc").extractOpt[Double]
        } yield {
          // Esto solo se ejecuta si 'title' y 'createdUtc' existen
          val selftext = (item \ "data" \ "selftext").extractOpt[String].getOrElse("")
          val score    = (item \ "data" \ "score").extractOpt[Int].getOrElse(0)
          val postUrl  = (item \ "data" \ "url").extractOpt[String].getOrElse("")
          val date     = TextProcessing.formatDateFromUTC(createdUtc.toLong)

          Post(subreddit, title, selftext, date, score, postUrl)
        }
      }
    }.toOption
  }
}