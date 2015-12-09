sealed trait Option[+A] {

	def mean(xs: Seq[Double]): Option[Double] = {
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)
	}

	def map[B](f: A => B): Option[B] = flatMap(a => Some(f(a)))

	def flatMap[B](f: A => Option[B]): Option[B] = this match {
		case None => None
		case Some(a) => f(a)
	}

	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(a) => a
	}

	def orElse[B >: A](that: => Option[B]): Option[B] = map(Some(_)).getOrElse(that)

	def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


