object MyModule {

	def abs (n: Int) : Int = {
		if (n < 0) -n
		else n
	}

	def fact (n : Int) : Int = {
		def go(n : Int, acc : Int) : Int = {
			if (n <= 1) acc
			else go(n - 1, n * acc)
		}
		go(n, 1)
	}

	private def formatResult(name : String, x : Int, f : Int => Int) = {
		val msg = "The %s of %d is %d"
		msg.format(name, x, f(x))
	}

	/*
	private def formatAbs(x : Int) : Unit = {
		println("The absolute value of %d is %d".format(x, abs(x)))
	}

	private def formatFactorial(x : Int) : Unit = {
		println("The factrial value of %d is %d".format(x, fact(x)))
	}
	*/

	def main (args: Array[String]): Unit = {
		println(formatResult("abs", -42, abs))
		println(formatResult("fact", 7, fact))
	}
}
