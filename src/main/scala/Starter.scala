object Solution {

  def main(args: Array[String]) {
    try {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val c = new Array[String](n)

      var root = BranchNode(Map.empty[Char, MyNode])

      for (c_i <- 0 to n) {
        if (c_i != 0) {
          c(c_i - 1) = sc.nextLine()
          root = Tree.fctn(c(c_i - 1), root, c(c_i - 1))
        } else sc.nextLine()
      }
      println("GOOD SET")
    } catch {
      case e: Exception =>
        println("BAD SET")
        println(e.getMessage())
    }
  }
}

trait MyNode

case class BranchNode(leaves: Map[Char, MyNode]) extends MyNode

case class LeafNode() extends MyNode

object Tree {

  def fctn(tail: String, bn: BranchNode, fs: String): BranchNode = {
    val key = tail.charAt(0)
    if (bn.leaves.keySet.contains(key)) {
      if (tail.length() == 1) {
        throw new Exception(fs)
      } else {
        bn.leaves(key) match {
          case _: LeafNode => throw new Exception(fs)
          case b: BranchNode =>
            val res = bn.leaves - key
            val final_res = res + (key -> fctn(tail.substring(1), b, fs))
            BranchNode(final_res)
        }
      }
    } else {
      if (tail.length() == 1) {
        val res: Map[Char, MyNode] = bn.leaves
        BranchNode(res + (key -> LeafNode()))
      } else {
        val res = bn.leaves + (key -> fctn(tail.substring(1), BranchNode(Map.empty[Char, MyNode]), fs))
        BranchNode(res)
      }
    }
  }
}