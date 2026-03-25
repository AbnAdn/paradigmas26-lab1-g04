
object Formatters {

  // Pure function to format posts from a subscription
  def formatPost(post: Post): String = {
    s"""
    Subreddit: ${post.subreddit}
    Title: ${post.title}
    Date: ${post.date}

    ${post.selftext}
    """
  }
  // Pure function to format posts from a subscription
  def formatSubscription(url: String, posts: List[Post]): String = {
    val header = s"\n${"=" * 80}\nPosts from: $url \n${"=" * 80}"
    val formattedPosts =  posts
    .take(10)
    .map(post => formatPost(post))
    .mkString("\n\n")
    header + "\n" + formattedPosts
  }
}
