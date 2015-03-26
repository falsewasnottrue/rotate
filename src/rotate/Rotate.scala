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

  val r = b.rotate
  println(r)

//  val g = r.gravity
//  println(g)
}
