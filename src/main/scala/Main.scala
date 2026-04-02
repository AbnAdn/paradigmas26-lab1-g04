object Main {
  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"

    val subscriptions: List[Subscription] = FileIO.readSubscriptions()


    //se modifica allPosts dado que downloadFeed fue modificado para manejar erroes
    val allPosts: List[(String, List[Post])] = subscriptions.map { sub =>
      println(s"Fetching posts from: ${sub.url}")
      val postsOpt = FileIO.downloadFeed(sub.url)

      val posts = postsOpt.getOrElse(List.empty[Post])
      (sub.url, posts)
    }

    val allFilteredPosts = allPosts.map { case (url, posts) =>
        val filteredPosts = posts.filter { case Post(_, title, selftext, _, _, _) =>
          title.trim.nonEmpty &&
          selftext.trim.nonEmpty
    }
    (url, filteredPosts)
    }.filter { case (_, posts) => posts.nonEmpty }
    
    //Reporte final
    val reportesFinales = allFilteredPosts.map { case (url, posts) =>
      Myfunctions.generarReporteSuscripcion(url, posts)
    }.mkString("\n\n")

    println(header)
    println("\n" + "=" * 40)
    println("INFORME DE ESTADÍSTICAS POR SUSCRIPCIÓN")
    println("=" * 40 + "\n")
    println(reportesFinales)
  }
}
