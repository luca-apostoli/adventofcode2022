package Day7


class OSStack(var children: List[OSStack], var value: OSElement, var index: Int) extends
  Serializable {
  override def toString: String = {
    value.toString + "\n" +
      children.foldLeft("")((c, s) => {c + s.toString})
  }
}

object OSTree {
  def mkRootNode: OSStack = new OSStack(List.empty, OSElement.apply("/", None), 0)

  def findParentIndex(tree: OSStack, pointer: Int) : (Int, Boolean) = {
    if (tree.index == pointer) {
      (tree.index, true)
    } else {
      var p = pointer
      for (c <- tree.children) {
        val r = findParentIndex(c, pointer)
        if (r._2) {
          return (tree.index, true)
        }
      }
      (p, false)
    }
  }

  def addChild(tree: OSStack, e: OSElement, pointer: Int, last: Int): (OSStack, Boolean) = {
    if (tree.index == pointer) {
      (new OSStack(tree.children.appended(new OSStack(List.empty, e, last + 1)),
          tree.value, tree.index), true)
    } else {
      var t = tree
      for (c <- tree.children.indices) {
        val r = addChild(tree.children.apply(c), e, pointer, last)
        if (r._2) {
          return (new OSStack(
            tree.children.updated(c,r._1),
            t.value,
            t.index), true)
        }
      }
      (t, false)
    }
  }

  def lookupChildIndex(tree: OSStack, e: OSElement, pointer: Int): (Int, Boolean) = {
    if (tree.index == pointer) {
      (tree.children.foldRight(0)((s, i) => {
        if (s.value.name == e.name) {
          s.index
        } else {
          i
        }
      }), true)
    } else {
      var p = pointer
      for (c <- tree.children) {
        val r = lookupChildIndex(c, e, pointer)
        if (r._2) {
          return r
        }
      }
      (p, false)
    }
  }

  def getSizes(o: OSStack, m: Map[String, Int]): Map[String, Int] = {
    var c = m
    val p = o.children.foldLeft(0) ((i, o) =>
      i +
        o.value
          .size
          .getOrElse(0))
    c = c.updated(o.value.name + o.index.toString, p)
    for (f <- o.children) {
      c = getSizes(f, c)
      c = c.updated(o.value.name + o.index.toString, p + c.getOrElse(f.value.name + f.index
        .toString, 0))
    }
    c
  }

}
