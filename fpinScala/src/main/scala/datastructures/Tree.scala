package fpinScala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/*
import fpinScala.datastructures._
import fpinScala.datastructures.Tree._
import fpinScala.datastructures.Leaf._
import fpinScala.datastructures.Branch._
*/

object Tree {
	
}
