case class User ( var name: String, var last_name: String, var salary: Double){

  override def toString: String = { String.format("%s %s: %1.2f", last_name, name, salary) }

  def GetCodedString : String = {
    val vs = Set("a", "e", "i", "o", "u")
    last_name.toLowerCase().filterNot(c => vs.contains(c.toString)).reverse
  }
}
