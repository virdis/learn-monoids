package com.algebra

trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) : String = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero = Nil
  }

  val intAdditionMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero = 0
  }

  val intMultiplicationMonoid = new Monoid[Int] {
    def op(v1: Int, v2: Int) = v1 * v2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(v1: Boolean, v2: Boolean) : Boolean = v1 || v2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(v1: Boolean, v2: Boolean): Boolean = v1 && v2
    val zero = true
  }


  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(v1: Option[A], v2: Option[A]) : Option[A] = v1 orElse v2
    val zero = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def endoMonoid[A] = new Monoid[ A => A ]{
    def op( f: A => A, g: A => A) = f compose g
    val zero = (a: A) => a
  }

  def trimMonoid(s: String):Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String) = (a1.trim + " "  + a2.trim).trim
    val zero = ""
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A,B](as: List[A])(m: Monoid[B])(f: A => B) : B =
    as.foldLeft( m.zero )((b, a) => m.op(b, f(a)))


  def foldMapV[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B ): B =
    if ( as.length == 0 )
      m.zero
    else if ( as.length == 1 )
      f( as(0) )
    else {
      val (l,r) = as.splitAt( as.length / 2 )
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

    def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K,V]] = 
      new Monoid[Map[K,V]] {
        def zero = Map()
        def op(a: Map[K,V], b: Map[K,V]) =
          a.map {
            case (k,v) => (k, V.op(v, b.get(k) getOrElse V.zero))
          }
      }

    def bag[A](as: IndexedSeq[A]): Map[A, Int] =
      foldMapV(as, mapMergeMonoid[A,Int](intAdditionMonoid))((a: A) => Map(a -> 1))

}

trait Foldable[F[_]] {
  import Monoid._
  
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldMap[A,B](as: F[A])(f: A => B)(m: Monoid[B]): B =
    foldRight(as)(m.zero)((a,b) => m.op(f(a), b))

  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B ):B =
    foldMap(as)(a => (b:B) => f(b,a))(dual(endoMonoid[B]))(z)

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())( _ :: _ )
}


