package fpinScala.errorhandling

trait Option[+A] {

    def mean(xs: Seq[Double]): Option[Double] = {
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)
    }

    // ex4.1
    def map[B](f: A => B): Option[B] = this match {
        case Some(a) => Some(f(a))
        case _ => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
        map(f).getOrElse(None)
    }

    def getOrElse[B >: A](default: => B) = this match {
        case Some(a) => a
        case _ => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        map(Some(_)).getOrElse(ob)
    }

    def filter(f: A => Boolean): Option[A] = this match {
        case Some(a) => if(f(a)) Some(a) else None
        case _ => None
    }

    // パターンマッチングを使わない記述
    // def filter(f: A => Boolean): Option[A] = {
    //  flatMap(a => if(f(a)) Some(a) else None)
    // }

    // ex4.2
    def varience(xs: Seq[Double]): Option[Double] = {
        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
    }

    def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

    val absO: Option[Double] => Option[Double] = lift(math.abs)

    // ex4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
        a.flatMap(as => b.flatMap(bs => Some(f(as, bs))))
    }

    // ex4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
        case h :: t => h.flatMap(hh => sequence(t).map(hh ::_))
        case _ => None
    }

    // foldRightとmap2による記述
    // def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    //     a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
    // }

    // def traverce(a: List[A])(f: A => Option[B]): Option[List[B]] = {
    //
    // }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {}


