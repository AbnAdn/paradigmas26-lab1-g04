import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter

object TextProcessing{
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
object Myfunctions{

    val stopwords: Set[String] = Set(
        "a", "the", "about", "above", "after", "again", "against", "all", "am", "an",
        "and", "any", "are", "aren't", "as", "at", "be", "because", "been",
        "before", "being", "below", "between", "both", "but", "by", "can't",
        "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't",
        "doing", "don't", "down", "during", "each", "few", "for", "from", "further",
        "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd",
        "he'll", "he's", "her", "here", "here's", "hers", "herself", "him",
        "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if",
        "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me",
        "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off",
        "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves",
        "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's",
        "should", "shouldn't", "so", "some", "such", "than", "that", "that's",
        "the", "their", "theirs", "them", "themselves", "then", "there", "there's",
        "these", "they", "they'd", "they'll", "re", "they've", "this", "those",
        "through", "to", "too", "under", "until", "up", "very", "was", "wasn't",
        "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what",
        "what's", "when", "when's", "where", "where's", "which", "while", "who",
        "who's", "whom", "why", "why's", "with", "won't", "would",
        "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours",
        "yourself", "yourselves"
    )

    

    //Fucion que calcula palabras mas frecuentes
    def frecuencyText(posts: List[Post]) : Map[String, Seq[(String, Int)]]={


        posts.groupBy(_.subreddit).map { case (subreddit, listaDePosts) =>
    
            // juntar titulo y texto de todos los posts de este subreddit
            val textoCompleto = listaDePosts.map(p => p.title + " " + p.selftext).mkString(" ")
            
            // separar en palabras (esto divide por cualquier cosa que no sea una letra/numero)
            val palabras = textoCompleto.split("\\W+").filter(_.nonEmpty)
            
            // filtrar mayusculas y stopwords
            val palabrasValidas = palabras.filter { word =>
                word.headOption.exists(_.isUpper) && !stopwords.contains(word.toLowerCase)
            }
            // Agrupar palabras idénticas y contar cuántas hay de cada una
            val conteo = palabrasValidas
                .groupBy(identity) // Crea un Map[String, Array[String]]
                .view.mapValues(_.size) // Cambia el Array por su longitud (Map[String, Int])
                .toMap
                
            // Ordenar de mayor a menor frecuencia
            val conteoOrdenado = conteo.toSeq.sortBy { case (palabra, cantidad) => -cantidad }
            
            // Retornamos el subreddit junto con sus palabras contadas y ordenadas
            (subreddit, conteoOrdenado)
        }
    }.toMap


    def calcularScoreTotal(posts: List[Post]): Int = {
        posts.foldLeft(0) { (acumulador, post) =>
            acumulador + post.score 
        }
    } 

    def generarReporteSuscripcion(urlFeed: String, posts: List[Post]): String = {
        
        // Suma total de scores
        val totalScore = calcularScoreTotal(posts)

        // Palabras mas frecuentes
        val frecuencias = frecuencyText(posts)
        val stringFrecuencias = frecuencias.flatMap { case (_, listaPalabras) =>
            listaPalabras.take(10).map { case (palabra, cantidad) => s"   - $palabra: $cantidad" }
        }.mkString("\n")

        // Cinco primeros posts (titulo, fecha, URL)
        val stringTop5 = posts.take(5).map { p =>
            s"   - Título: ${p.title} | Fecha: ${p.date} | URL: ${p.url}" 
        }.mkString("\n")

        // Reporte final
        s"""### Reporte para: $urlFeed
        |**Suma total de scores:** $totalScore
        |
        |**Palabras más frecuentes:**
        |$stringFrecuencias
        |
        |**Últimos 5 posts:**
        |$stringTop5
        |-----------------------------------------""".stripMargin
    }
}