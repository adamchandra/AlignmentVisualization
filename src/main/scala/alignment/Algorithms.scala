package alignment

import scala.collection.mutable

object Util {

  type Array2[A] = Array[Array[A]]

  def add2DIdx[A](cells: Array2[A]): Array2[((Int, Int), A)] =
    for ((row, y) <- cells.zipWithIndex) yield {
      for ((c, x) <- row.zipWithIndex) yield {
        ((y, x), c)
      }
    }

  def posAt(y: Int, x: Int, dir: Direction.Value): (Int, Int) = dir match {
    case Direction.Left => (y, x - 1)
    case Direction.LeftTop => (y - 1, x - 1)
    case Direction.Top => (y - 1, x)
  }

//  def println(s: Any) = {}
}

import Util._

object Direction extends Enumeration {
  val Left, Top, LeftTop = Value
}

case class Result(cells: Array[Array[Option[Int]]], arrows: Array[Array[Option[Direction.Value]]]
                  , str: Array[String], path: Array[(Int, Int, Char, Char)] = Array())

trait Alignment {
  def calc: Result
}

class LCS(s1: Array[Char], s2: Array[Char]) extends Alignment {
  def calc: Result = {
    val cells = Array.tabulate[Option[Int]](s1.length + 1, s2.length + 1) {
      (i, j) =>
        if (i == 0 || j == 0) Some(0) else None
    }

    val arr: Array[Array[Option[Direction.Value]]] = Array.fill(s1.length + 1, s2.length + 1)(None)

    for (y <- 1 to s1.length; x <- 1 to s2.length) {
      println(y, x)
      val v1 = cells(y)(x - 1).get
      val v2 = cells(y - 1)(x).get
      val vv3 = cells(y - 1)(x - 1).get + (if (s1(y - 1) == s2(x - 1)) 1 else 0)
      val mx = Array((vv3, Direction.LeftTop), (v1, Direction.Left), (v2, Direction.Top)).maxBy(_._1)
      cells(y)(x) = Some(mx._1)
      arr(y)(x) = Some(mx._2)
    }
    val (str, path) = traceback(cells, arr)
    Result(cells, arr, Array(str), path)
  }

  def traceback(cells: Array[Array[Option[Int]]], arr: Array[Array[Option[Direction.Value]]]): (String, Array[(Int, Int, Char, Char)]) = {
    var (y, x) = (s1.length, s2.length)
    val res = new StringBuffer

    val poss = new mutable.ArrayBuffer[(Int, Int)]
    val poss2 = new mutable.ArrayBuffer[(Int, Int, Char, Char)]

    def prev(pos: (Int, Int), dir: Direction.Value): Int = {
      val (y, x) = posAt(pos._1, pos._2, dir)
      cells(y)(x).get
    }

    while (y > 0 && x > 0) {
      val dir = arr(y)(x).get
      println((y, x), dir)
      poss.append((y, x))
      if (prev((y, x), dir) < cells(y)(x).get) {
        res.append(s1(y - 1))
      }
      poss2.append((y, x, s1(y - 1), s2(x - 1)))
      val t = posAt(y, x, dir)
      y = t._1
      x = t._2
    }
    println(poss.mkString(","))
    println(poss2.mkString(","))
    (res.reverse.toString, poss2.toArray)
  }
}

class Smith(s1: Array[Char], s2: Array[Char]) extends Alignment {
  def calc: Result = {
    val cells = Array.tabulate(s1.length + 1, s2.length + 1) {
      (i, j) =>
        if (i == 0 || j == 0) Some(0) else None
    }

    val path: Array2[Option[Direction.Value]] = Array.fill(s1.length + 1, s2.length + 1)(None)

    for (y <- 1 to s1.length; x <- 1 to s2.length) {
      val v1 = cells(y)(x - 1).get - 2
      val v2 = cells(y - 1)(x).get - 2
      val vv3 = cells(y - 1)(x - 1).get + (if (s1(y - 1) == s2(x - 1)) 1 else -1)
      val mx = Array((vv3, Direction.LeftTop), (v1, Direction.Left), (v2, Direction.Top), (0,null)).maxBy(_._1)
      cells(y)(x) = Some(mx._1)
      path(y)(x) = if(mx._1 > 0) Some(Direction.LeftTop) else None
    }
    val (str,path2) = traceback(cells, path)
    Result(cells, path, str, path2)
  }

  def traceback(cells: Array2[Option[Int]],
                     arr: Array2[Option[Direction.Value]]): (Array[String],Array[(Int,Int,Char,Char)]) = {
    // pos: (row, col)
    var (y, x) = {
      val ci: Array2[((Int, Int), Option[Int])] = add2DIdx(cells)
      ci.flatten.maxBy(_._2)._1
    }
    val ss1 = new StringBuffer
    val ss2 = new StringBuffer
    val poss = new mutable.ArrayBuffer[(Int, Int, Char, Char)]

    while (cells(y)(x).get > 0) {
      val dir = arr(y)(x).get
      println((y, x), dir)
      poss.append((y, x, s1(y-1),s2(x-1)))

      dir match {
        case Direction.Left => {
          ss1.append('-')
          ss2.append(s2(x))
        }
        case Direction.Top => {
          ss1.append(s1(y))
          ss2.append('-')
        }
        case Direction.LeftTop => {
          ss1.append(s1(y-1))
          ss2.append(s2(x-1))
        }
      }
      val t = posAt(y, x, dir)
      y = t._1
      x = t._2
    }
    println(poss.mkString(","))
    (Array(ss1.reverse.toString, ss2.reverse.toString),poss.toArray)
  }

}

class Needleman(s1: Array[Char], s2: Array[Char]) extends Alignment {
  def calc: Result = {
    val cells: Array2[Option[Int]] = Array.tabulate[Option[Int]](s1.length + 1, s2.length + 1) {
      (i, j) =>
        if (i == 0)
          Some(-2 * j)
        else if (j == 0)
          Some(-2 * i)
        else
          None
    }
    val path: Array2[Option[Direction.Value]] = Array.fill(s1.length + 1, s2.length + 1)(None)

    for (y <- 1 to s1.length; x <- 1 to s2.length) {
      val v1 = cells(y)(x - 1).get - 2
      val v2 = cells(y - 1)(x).get - 2
      val vv3 = cells(y - 1)(x - 1).get + (if (s1(y - 1) == s2(x - 1)) 1 else -1)
      val mx = Array((vv3, Direction.LeftTop), (v1, Direction.Left), (v2, Direction.Top)).maxBy(_._1)
      cells(y)(x) = Some(mx._1)
      path(y)(x) = Some(mx._2)
    }

    val (str,path2) = tracebackNeedleman(cells, path)
    Result(cells, path, str, path2)
  }

  def tracebackNeedleman(cells: Array2[Option[Int]], arr: Array2[Option[Direction.Value]]): (Array[String], Array[(Int, Int, Char, Char)]) = {
    var (y, x) = (s1.length, s2.length)
    val ss1 = new StringBuffer
    val ss2 = new StringBuffer
    val poss = new mutable.ArrayBuffer[(Int, Int)]
    val poss2 = new mutable.ArrayBuffer[(Int, Int, Char, Char)]

    println(arr.flatten.mkString(","))
    while (y > 0 && x > 0) {
      val dir = arr(y)(x).get
      println((y, x), dir)
      poss.append((y, x))

      dir match {
        case Direction.Left => {
          ss1.append('-')
          ss2.append(s2(x - 1))
        }
        case Direction.Top => {
          ss1.append(s1(y - 1))
          ss2.append('-')
        }
        case Direction.LeftTop => {
          ss1.append(s1(y - 1))
          ss2.append(s2(x - 1))
        }
      }
      poss2.append((y, x, s1(y - 1), s2(x - 1)))

      val t = posAt(y, x, dir)
      y = t._1
      x = t._2
    }
    println(poss.mkString(","))
    (Array(ss1.reverse.toString, ss2.reverse.toString),poss2.toArray)
  }

}