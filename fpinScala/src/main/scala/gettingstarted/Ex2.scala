object Ex2 {

    def fib(n: Int): Int= {
        def go(n: Int, a: Int, b: Int): Int = {
            if (n <= 0) a
            else go(n - 1, b, a + b)
        }
        go(n, 0, 1)
    }

    def isSorted[A](as: Array[A], p: (A, A) => Boolean): Boolean = {
        def loop(n: Int): Boolean = {
            if(n >= as.length) true
            else if (p(as(n-1), as(n))) loop(n+1)
            else false
        }
        loop(1)
    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
        a => (b => f(a, b))
    }

    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
        (a, b) => f(a)(b)
    }

    def compose[A, B, C](f: B => C, g: A => B): A => C = {
        a => f(g(a))
    }
}