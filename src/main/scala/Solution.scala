import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks._

trait MyNode

case class BranchNode(leaves: Map[Char, MyNode]) extends MyNode

case class LeafNode() extends MyNode

object Solution {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val wordArray = new Array[String](n)

    var root = BranchNode(Map.empty[Char, MyNode])

    breakable {
      for (i <- 0 to n) {
        if (i != 0) {
          wordArray(i - 1) = sc.nextLine()
          Tree.addStringToTrie(wordArray(i - 1), root, wordArray(i - 1)) match {
            case Success(s) => root = s
            case Failure(e) => {
              println("BAD SET")
              println(e.getMessage)
              break
            }
          }
        } else sc.nextLine()
      }
      println("GOOD SET")
    }
  }
}

object Tree {

  def addStringToTrie(tail: String, bn: BranchNode, fullWord: String): Try[BranchNode] = {
    val key = tail.charAt(0)
    if (bn.leaves.keySet.contains(key)) {
      if (tail.length() == 1) {
        Failure(new Exception(fullWord))
      } else {
        bn.leaves(key) match {
          case _: LeafNode => Failure(new Exception(fullWord))
          case b: BranchNode =>
            val tryRes = addStringToTrie(tail.substring(1), b, fullWord)
            tryRes match {
              case Success(modifiedBranch) => {
                val mapWithoutKey = bn.leaves - key
                val mapWithNewKeyValue = mapWithoutKey + (key -> modifiedBranch)
                Success(BranchNode(mapWithNewKeyValue))
              }
              case Failure(e) => {
                Failure(e)
              }
            }
        }
      }
    } else {
      if (tail.length() == 1) {
        val map = bn.leaves
        Success(BranchNode(map + (key -> LeafNode())))
      } else {
        val tryRes = addStringToTrie(tail.substring(1), BranchNode(Map.empty[Char, MyNode]), fullWord)
        tryRes match {
          case Success(newBranch) => {
            val mapWithNewBranch = bn.leaves + (key -> newBranch)
            Success(BranchNode(mapWithNewBranch))
          }
          case Failure(e) => {
            Failure(e)
          }
        }
      }
    }
  }
}