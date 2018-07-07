object main extends App{  //d
  //implement polymorphism of cell in excell
  val cb1 : Cell  = new CellWithFloat(272.1f , "b",1)
  val cb2 : Cell  = new CellWithFloat(22.0f  , "b",2)
  val ca1 : Cell  = new CellWithFormula (Seq(cb1.asInstanceOf[CellWithFloat],cb2.asInstanceOf[CellWithFloat]), "a",1)
  val ca2 : Cell  = new CellWithFormula (Seq(cb2.asInstanceOf[CellWithFloat],cb1.asInstanceOf[CellWithFloat]), "a",2)
  //val cellArr     = Array.ofDim[Cell](2, 2)
  val cellArr     =  Array(Array(ca1,ca2), Array(cb1, cb2))
  val tbl : Table = new Table(cellArr)
  cb2.toString
  ca1.toString
  tbl.show()


}

class Table(cells:Array[Array[Cell]]){
  def show ()={
    println("printing table")
    cells.foreach(e=>e.foreach(e=>print(e+"\n")))
  }
}
//var listSetAll1: List[L
abstract class Cell(      col    :String    , row    :Int  ){
  def onClick(): Unit
}

class CellWithFormula (formula: Seq[CellWithFloat],   col: String,   row: Int)
  extends Cell(  col, row )
{
  override def onClick (): Unit   = println(col+":"+row)
  override def toString(): String = "CellWithFunc:"+ this.col+ ":"+ this.row+" value="+formula.map(e=>e.value).sum+ " formula"+formula
}


class CellWithFloat(var value: Float ,    col: String,   row: Int)
  extends Cell(  col, row )
{
  override   def onClick(): Unit =  println(value)
  override def toString: String = "CellWithFloat:"+ this.col+ ":"+ this.row+" value="+value
}