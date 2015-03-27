package rotate

sealed trait Slot {
  def name: String
}
object Empty extends Slot {
  val name = "."
}
object Red extends Slot {
  val name = "R"
}
object Blue extends Slot {
  val name = "B"
}

case class Board(slots: Array[Array[Slot]]) {
  def apply(row: Integer, column: Integer) = slots(row)(column)

  override def toString = slots.foldLeft("")((acc, row) => {
    acc + row.foldLeft("")((a, s) => a + s.name) + '\n'
  })
}

object Board {
  def create(slist: List[String]): Board =
    Board(slist.map(s => {
      s.toCharArray.map(c => c match {
        case '.' => Empty
        case 'R' => Red
        case 'B' => Blue
      })
    }).toArray)
}

object Rotate extends App {

  val b = Board.create(
    "......." ::
      "......." ::
      "......." ::
      "...R..." ::
      "...RB.." ::
      "..BRB.." ::
      ".RBBR.." :: Nil)
  println(b)
  println(b(3, 3))

  val r = rotate(b)
  println(r)

  // expected
  //  .......
  //  R......
  //  BB.....
  //  BRRR...
  //  RBB....
  //  .......
  //  .......

  val g = gravity(r)
  println(g)

  // expected
  //  .......
  //  .......
  //  .......
  //  R......
  //  BB.....
  //  BRR....
  //  RBBR...

  println(winners(b, 2))
  def rotate(b: Board): Board = {
    val n = b.slots.length
    val ss = Array.ofDim[Slot](n, n)

    for (i <- 0 until n)
      for (j <- 0 until n)
        ss(i)(j) = b.slots((n - 1) - j)(i)

    Board(ss)
  }

  def gravity(b: Board): Board = {
    def sort(in: Array[Slot]): Array[Slot] =
      in.filter(s => s != Empty) ++ Array.fill[Slot](in.size)(Empty)

    val ss = rotate(b).slots.map(row => sort(row))

    rotate(rotate(rotate(Board(ss))))
  }

  def winners(b: Board, k: Integer): List[Slot] = {
    // val diagonals = duh
    val all = b.toString + "#" + rotate(b).toString

    for {
      x <- Blue :: Red :: Nil
      if (all.contains(x.name * k))
    } yield x
  }
}
