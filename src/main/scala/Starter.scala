import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks._

object Starter {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val c = new Array[String](n)

    var root = BranchNode(Map.empty[Char, MyNode])

    breakable {
      for (c_i <- 0 to n) {
        if (c_i != 0) {
          c(c_i - 1) = sc.nextLine()
          Tree.fctn(c(c_i - 1), root, c(c_i - 1)) match {
            case Success(s) => root = s
            case Failure(e) => {
              println("BAD SET")
              break
            }
          }
        } else sc.nextLine()
      }
      println("GOOD SET")
    }
  }
}

trait MyNode

case class BranchNode(leaves: Map[Char, MyNode]) extends MyNode

case class LeafNode() extends MyNode

object Tree {

  def fctn(tail: String, bn: BranchNode, fs: String): Try[BranchNode] = {
    val key = tail.charAt(0)
    if (bn.leaves.keySet.contains(key)) {
      if (tail.length() == 1) {
        Failure[BranchNode](new Exception())
      } else {
        bn.leaves(key) match {
          case _: LeafNode => Failure[BranchNode](new Exception())
          case b: BranchNode =>
            val tryRes = fctn(tail.substring(1), b, fs)
            tryRes match {
              case Success(s) => {
                val res = bn.leaves - key
                val final_res = res + (key -> s)
                Success(BranchNode(final_res))
              }
              case Failure(e) => {
                Failure(e)
              }
            }
        }
      }
    } else {
      if (tail.length() == 1) {
        val res: Map[Char, MyNode] = bn.leaves
        Success(BranchNode(res + (key -> LeafNode())))
      } else {
        val tryRes = fctn(tail.substring(1), BranchNode(Map.empty[Char, MyNode]), fs)
        tryRes match {
          case Success(s) => {
            val final_res = bn.leaves + (key -> s)
            Success(BranchNode(final_res))
          }
          case Failure(e) => {
            Failure(e)
          }
        }
      }
    }
  }
}