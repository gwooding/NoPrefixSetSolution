object Solution {

  def main(args: Array[String]) {
    try {
      val sc = new java.util.Scanner(System.in);
      var n = sc.nextInt();
      var c = new Array[String](n)

      var root = BranchNode2(Map.empty[Char, MyNode2])

      for (c_i <- 0 to n) {
        if (c_i != 0) {
          c(c_i - 1) = sc.nextLine()
          root = Tree2.fctn(c(c_i - 1), root, c(c_i - 1))
        } else sc.nextLine()
      }
      println("GOOD SET")
    } catch {
      case e: Exception => {
        println("BAD SET")
        println(e.getMessage())
      }
    }
  }
}

trait MyNode2

case class BranchNode2(leaves: Map[Char, MyNode2]) extends MyNode2

case class LeafNode2() extends MyNode2

object Tree2 {

  def fctn(tail: String, bn: BranchNode2, fs: String): BranchNode2 = {
    val key = tail.charAt(0)
    if (bn.leaves.keySet.contains(key)) {
      if (tail.length() == 1) {
        throw new Exception(fs);
      } else {
        bn.leaves(key) match {
          case a: LeafNode2 => throw new Exception(fs)
          case b: BranchNode2 => {
            val res = bn.leaves - key
            val final_res = res + (key -> fctn(tail.substring(1), b, fs))
            BranchNode2(final_res)
          }
        }
      }
    } else {
      if (tail.length() == 1) {
        val res: Map[Char, MyNode2] = bn.leaves
        BranchNode2(res + (key -> LeafNode2()))
      } else {
        val res = bn.leaves + (key -> fctn(tail.substring(1), BranchNode2(Map.empty[Char, MyNode2]), fs))
        BranchNode2(res)
      }
    }
  }
}