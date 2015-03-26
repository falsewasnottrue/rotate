package rotate

sealed trait Slot {
  def name: String
}
object * extends Slot {
  val name = "."
}
object R extends Slot {
  val name = "R"
}
object B extends Slot {
  val name = "B"
}

case class Board(slots: Array[Array[Slot]]) {
  def rotate: Board = ???
  def gravity: Board = ???

  override def toString = slots.foldLeft("")((acc, row) => {
    acc + row.foldLeft("")((a, s) => a + s.name) + '\n'
  })
}

object Board {
  def create(slist: List[String]): Board =
    Board(slist.map(s => {
      s.toCharArray.map(c => c match {
        case '.' => *
        case 'R' => R
        case 'B' => B
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

//  val r = b.rotate
//  println(r)
//
//  val g = r.gravity
//  println(g)
}
