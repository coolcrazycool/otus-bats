package me.chuwy.otusbats


trait Show[A] {
  def show(a: A): String
}

object Show {

  // 1.1 Instances (`Int`, `String`, `Boolean`)

  implicit val showInt: Show[Int] = fromJvm[Int]
  implicit val showString: Show[String] = fromJvm[String]
  implicit val showBoolean: Show[Boolean] = fromJvm[Boolean]


  // 1.2 Instances with conditional implicit

  implicit def showList[A](implicit ev: Show[A]): Show[List[A]] =
    (a: List[A]) => a.foldLeft("")((acc, el) => acc ++ ev.show(el))

  implicit def showSet[A](implicit ev: Show[A]): Show[Set[A]] =
    (a: Set[A]) => a.foldLeft("")((acc, el) => acc ++ ev.show(el))


  // 2. Summoner (apply)

  def apply[A](implicit ev: Show[A]): Show[A] = ev

  // 3. Syntax extensions

  implicit class ShowOps[A](a: A) {
    def show(implicit ev: Show[A]): String =
      ev.show(a)

    def mkString_[B](begin: String, end: String, separator: String)(implicit S: Show[B], ev: A <:< List[B]): String = {
      // with `<:<` evidence `isInstanceOf` is safe!
      val casted: List[B] = a.asInstanceOf[List[B]]
      Show.mkString_(casted, separator, begin, end)
    }

  }

  /** Transform list of `A` into `String` with custom separator, beginning and ending.
   * For example: "[a, b, c]" from `List("a", "b", "c")`
   *
   * @param separator . ',' in above example
   * @param begin     . '[' in above example
   * @param end       . ']' in above example
   */
  def mkString_[A: Show](list: List[A], begin: String, end: String, separator: String): String =
    list.headOption.map { head =>
      list.tail.foldLeft(begin ++ head.show)((acc, el) => acc ++ separator ++ el.show) ++ end
    }.getOrElse("")


  // 4. Helper constructors

  /** Just use JVM `toString` implementation, available on every object */
  def fromJvm[A]: Show[A] = (a: A) => a.toString

  /** Provide a custom function to avoid `new Show { ... }` machinery */
  def fromFunction[A](f: A => String): Show[A] = (a: A) => f(a)

}
