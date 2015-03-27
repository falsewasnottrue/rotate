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

  def diagonals(b: Board): Board =
    Board((for {
      i <- 0 until b.slots.length
      diagonal = (for {
        j <- 0 to i
      } yield b.slots(i-j)(j)).toArray
    } yield diagonal).toArray)

  def winners(b: Board, k: Integer): List[Slot] = {
    val all = b.toString + "#" + rotate(b).toString +
      "#" + diagonals(b).toString +
      "#" + diagonals(rotate(b)).toString +
      "#" + diagonals(rotate(rotate(b))).toString +
      "#" + diagonals(rotate(rotate(rotate(b)))).toString

    for {
      x <- Blue :: Red :: Nil
      if (all.contains(x.name * k))
    } yield x
  }
}
