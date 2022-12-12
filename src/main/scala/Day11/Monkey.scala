package Day11

class Monkey (val id: Int, val op: (BigInt) => BigInt, val div: Int, val resTrue: Int, val
resFalse: Int ){
  var items: List[BigInt] = List.empty
  def test(n: BigInt): Int = {
    if (n % div == 0) {
      resTrue
    } else {
      resFalse
    }
  }

  var countItems: BigInt = 0

  override def toString: String = {
    s"Monkey ${id}: ${items} - Count: ${countItems} - Test: ${div} - Op: ${op(1)}"
  }


  def setStartingItems(l: List[BigInt]): Monkey = {
    items = l
    this
  }

  def addItem(i: BigInt): Monkey = {
    items = items.appended(i)
    this
  }
  def incrementCount : Monkey = {
    countItems = countItems + 1
    this
  }

  def pop(): Option[BigInt] = {
    if(items.isEmpty) {
      return None
    }

    val i = items.head
    items = items.drop(1)
    Some(i)
  }

}
