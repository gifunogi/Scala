package fpinScala.state

trait RNG {
	def nextInt: (Int, RNG)
}

object RNG {
	// &	: 論理ビット積
	// >>>	: 0埋め右シフト
	case class SimpleRNG(seed: Long) extends RNG {
		def nextInt: (Int, RNG) = {
			val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
			val nextRNG = SimpleRNG(newSeed)
			val n = (newSeed >>> 16).toInt
			(n, nextRNG)
		}
	}

	// ex6.1
	def nonNegativeInt(rng: RNG): (Int, RNG) = {
		val (i, nr) = rng.nextInt
		(if (i < 0) -i+1 else i, nr)
	}
}