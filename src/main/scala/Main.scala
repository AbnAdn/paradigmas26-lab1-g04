object Main {
  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"

    val subscriptions: List[Subscription] = FileIO.readSubscriptions()

    val allPosts: List[(String, List[Post])] = subscriptions.map { sub =>
      println(s"Fetching posts from: $sub.url")
      val posts = FileIO.downloadFeed(sub.url)
      (sub.url, posts)
    }

    val allFilteredPosts = allPosts.map { case (url, posts) =>
        val filteredPosts = posts.filter { case Post(_, title, selftext, _) =>
          title.trim.nonEmpty &&
          selftext.trim.nonEmpty &&                     
          (title + selftext).trim.nonEmpty           
    }
    (url, filteredPosts)
    }.filter { case (_, posts) => posts.nonEmpty }

    val output = allFilteredPosts
      .map { case (url, posts) => Formatters.formatSubscription(url, posts) }
      .mkString("\n")

    println(output)
  }
}
