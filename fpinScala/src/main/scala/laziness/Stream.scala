package fpinScala.laziness

import fpinScala.errorhandling._
import fpinScala.errorhandling.Some._
import fpinScala.errorhandling.None._

sealed trait Stream[+A] {

	def toList: List[A] = this match {
		case Cons(h, t) => h() :: t().toList
		case Empty => Nil
	}

	// ex5.2
	def drop(n: Int): Stream[A] =  {
		if (n <= 0) this
		else this match {
			case Cons(h, t) => t().drop(n-1)
			case _ => Empty
		}
	}

	// ex5.2
	def take(n: Int): Stream[A] = {
		if (n <= 0) Empty
		else this match {
			case Cons(h, t) => cons(h(), t().take(n-1))
			case _ => this
		}
	}

	// ex5.3
	def takeWhile(p: A => Boolean): Stream[A] = this match {
		case Cons(h, t) => if(p(h())) cons(h(),t().takeWhile(p)) else Empty
		case _ => this
	}

	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
		case Cons(h, t) => f(h(), t().foldRight(z)(f))
		case _ => z
	}

	def exists(p: A => Boolean): Boolean = {
		foldRight(false)((h, t) => p(h) || t)
	}

	// ex5.4
	def forAll(p: A => Boolean): Boolean = {
		foldRight(true)((h, t) => p(h) && t)
	}

	// ex5.5
	def takeWhileByFR(p: A => Boolean): Stream[A] = {
		foldRight(Empty:Stream[A])((h, t) => if(p(h)) cons(h, t) else Empty)
	}

	// ex5.6
	// def headOption(arg: Type) = {
	// }

	// ex5.7
	def map[B](p: A => B): Stream[B] = {
		foldRight(Empty:Stream[B])((h, t) => cons(p(h), t))
	}

	def filter(p: A => Boolean): Stream[A] = {
		foldRight(Empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else t)
	}

	def append[AS >: A](as: Stream[AS]): Stream[AS] = {
		foldRight(as)((h, t) => cons(h, t))
	}

	def flatMap[B](p: A => Stream[B]): Stream[B] = {
		foldRight(Empty: Stream[B])((h, t) => p(h).append(t))
	}

	// ex5.8
	def constant[A](a: A) : Stream[A] = cons(a, constant(a))

	// ex5.9
	def from(n: Int): Stream[Int] = cons(n, from(n+1))

	// ex5.10
	def fibs: Stream[Int] = {
		def fib(n: Int): Int = {
			def go(n: Int, a: Int, b: Int): Int = {
				if (n > 0) go(n-1, b, a+b)
				else a
			}
			go(n, 0, 1)
		}
		from(0).map(fib)
	}

	// ex5.11
	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
		case Some((a, s)) => cons(a, unfold(s)(f))
		case _ => Empty
	}

	// ex5.12
	def fibsByUF: Stream[Int] = {
		unfold((0, 1)){case (x, y) => Some(x, (y, (x+y)))}
	}

	def fromByUF(n: Int): Stream[Int] = {
		unfold(n)(n => Some((n, n+1)))
	}

	def constantByUF[A](a: A) : Stream[A] = {
		unfold(a)(a => Some((a, a)))
	}

	def onesByUF: Stream[Int] = {
		unfold(1)(_ => Some((1, 1)))
	}

	// ex5.13
	def mapByUF[B](p: A => B): Stream[B] = unfold(this) {
		case Cons(h, t) => Some((p(h()), t()))
		case _ => None
	}
	// 関数の変数名は一般的にf
	// Booleanを返すときのみp(predicate)

	def takeByUF(n: Int): Stream[A] = unfold(this, n) {
		case (Cons(h, t), n) => if (n > 0) Some((h(), (t(), n-1))) else None
		case _ => None
	}
	// ガードを用いた記述
	// def takeByUF(n: Int): Stream[A] = unfold(this, n) {
	// 	case (Cons(h, t), n) if (n > 0)　=>  Some((h(), (t(), n-1)))
	// 	case _ => None
	// }

	def takeWhileByUF(p: A => Boolean): Stream[A] = unfold(this) {
		case Cons(h, t) => if (p(h())) Some((h(), t())) else None
		case _ => None
	}

	def zipWithByUF[B, C](s: Stream[B])(p: (A, B) => C): Stream[C] = unfold(this, s) {
		case (Cons(h1, t1), Cons(h2, t2)) => Some((p(h1(), h2()), (t1(), t2())))
		case _ => None
	}

	def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this, s) {
		case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
		case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
		case (Cons(h1, t1), _) => Some(((Some(h1()), None), (t1(), Empty)))
		case _ => None
	}

	// ex5.14
	def startsWith[AA>:A](s: Stream[AA]): Boolean = {
		zipAll(s).forAll {
			case (Some(x), Some(y)) => x == y
			case (Some(x), _) => false
			case (_, Some(y)) => true
		}
	}


	// ex5.15
	// 空のストリームが生成されない
	// def tails: Stream[Stream[A]] = unfold(this) {
	// 	case Cons(h, t) => Some((cons(h(), t()), t()))
	// 	case _ => None
	// }
	def tails: Stream[Stream[A]] = { cons(this, unfold(this)) {
		case Cons(_, t) => Some((t(), t()))
		case _ => None
	}


	// ex5.16
	// 計算結果が線形にならない
	// def scanRight[B](z: B)(f: (A, => B) => B):Stream[B] = unfold(this) {
	// 	case Cons(h, t) => Some((cons(h(), t()).foldRight(z)(f), t()))
	// 	case _ => None
	// }.append(cons(z,  Empty))
	def scanRight[S](s: S)(f: (A, => S) => S): Stream[S] = this match {
		case Empty => Stream(s)
		case Cons(head, tail) =>
			tail().scanRight(s)(f) match {
			case xs@Cons(x, _) => Stream.cons(f(head(), x()), xs)
			case Empty => Empty
		}
	}



	def apply[A](as: A*): Stream[A] = {
		if (as.isEmpty) Empty
		else cons(as.head, apply(as.tail: _*))
	}

	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

	def empty[A]: Stream[A] = Empty

	def apply[A](as: A*): Stream[A] = {
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))
	}

	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}
}