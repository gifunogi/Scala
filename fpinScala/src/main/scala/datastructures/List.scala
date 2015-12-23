package fpinScala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/*
import fpinScala.datastructures._
import fpinScala.datastructures.List._
import fpinScala.datastructures.Nil._
import fpinScala.datastructures.Cons._
val x: List[Int] = List(1, 2, 7, 10, 4, 6)
val xd: List[Double] = List(1.1, 2.2, 7.7, 10.1, 4.4, 6.6)
*/

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	// ex3.2
	def tail[A](as: List[A]): List[A] = as match {
		case Nil => Nil
		case Cons(x, xs) => xs
	}

	// ex3.3
	def setHead[A](l: List[A], a: A): List[A] = l match {
		case Nil => Nil
		case Cons(x, xs) => Cons(a, xs)
	}

	// ex3.4
	def drop[A](l: List[A], n: Int): List[A] = l match {
		case Nil => Nil
		case Cons(x, xs) => if(n <= 0) l else drop(xs, n - 1)
	}

	// ex3.5
	def dropWhile[A](l :List[A], f: A => Boolean): List[A] = l match {
		case Nil => Nil
		case Cons(x, xs) => if(f(x)) dropWhile(xs, f) else l
	}

	// ex3.6
	def init[A](l: List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(x, Nil) => Nil
		case Cons(x, xs) => Cons(x, init(xs))
	}

	def sum2(ns: List[Int]) = {
		foldRight(ns, 0)((x, y) => x + y)
	}

	// ex3.7
	def product2(ns: List[Double]) = {
		foldRight(ns, 1.0)(_ * _)
	}

	// ex3.9
	def length[A](as: List[A]): Int = {
		foldRight(as, 0)((x, y) => y + 1)
		// foldRight(as, 0)((_, n) => n + 1)
		// 使用しない変数はアンダースコアにすると読みやすい
	}

	// ex3.11
	def suml(l: List[Int]): Int = {
		foldLeft(l, 0)(_ + _)
	}

	// ex3.11
	def productl(l: List[Double]): Double = {
		foldLeft(l, 0.0)(_ * _)
	}

	// ex3.12
	def reverse[A](l: List[A]): List[A] = {
		foldLeft(l , Nil:List[A])((x, y) => Cons(y, x))
	}

	// ex3.14
	def append[A](l: List[A], a: List[A]): List[A] = {
		foldRight(l, a)(Cons(_, _))
	}

	// ex3.15
	def flatten[A](l: List[List[A]]): List[A] = {
		foldRight(l, Nil:List[A])(append(_, _))
	}

	// ex3.16
	def inc(l: List[Int]): List[Int] = {
		foldRight(l, Nil:List[Int])((x, y)=>Cons(x + 1, y))
	}

	// ex3.17
	def doubleToString(l: List[Double]): List[String] = {
		foldRight(l, Nil:List[String])((x, y)=>Cons(x.toString, y))
	}

	// ex3.18
	def map[A,B](l: List[A])(f: A => B): List[B] = {
		foldRight(l, Nil:List[B])((x, y) => Cons(f(x), y))
	}

	// ex3.19
	def filter[A](l: List[A])(f: A => Boolean): List[A] = {
		foldRight(l, Nil: List[A])((x, y) => if(f(x)) Cons(x, y) else y)
	}

	// ex3.20
	def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
		flatten(map(l)(f))
	}

	// ex3.21
	def filterByFM[A](l: List[A])(f: A => Boolean): List[A] = {
		flatMap(l)((a: A) => if(f(a)) Cons(a,Nil) else Nil)
	}

	// ex 3.22
	def sumWith(al: List[Int], bl: List[Int]): List[Int] = (al, bl) match {
		case (Cons(a, as), Cons(b, bs)) => Cons(a+b, sumWith(as, bs))
		case _ => Nil
	}

	// ex3.23
	def zipWith[A](al: List[A], bl: List[A], f: (A, A) => A): List[A] = (al, bl) match {
		case (Cons(a, ax), Cons(b, bx)) => Cons(f(a, b), zipWith(ax, bx, f))
		case _ => Nil
	}
	// 型の拡張，カリー化が可能
	//	def zipWith[A,B,C](al: List[A], bl: List[B])(f: (A, B) => C): List[C] = (al, bl) match {
	//		case (Cons(a, ax), Cons(b, bx)) => Cons(f(a, b), zipWith(ax, bx)(f))
	//		case _ => Nil
	//	}

	// ex3.24



	def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}

	// ex3.10
	def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
	}

	def apply[A](as: A*): List[A] = {
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	// ex3.1
	def main(args: Array[String]): Unit = {
		val x = List(1, 2, 3, 4, 5) match {
			case Cons(x, Cons(2, Cons(4, _))) => x
			case Nil => 42
			case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
			case Cons(h, t) => h + sum(t)
			case _ => 101
		}
		// case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + yにマッチ
		// x = 1, y = 2 から　x + y = 3 が　返り値
	}

}
