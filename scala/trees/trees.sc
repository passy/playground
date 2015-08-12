
sealed trait Tree

case class Node[A](value: A, left: Tree, right: Tree) extends Tree
case class Empty extends Tree

def traverseDF[A](tree: Tree): Seq[A] = tree match {
  case Empty => Seq()
  case Node(v, l, r) => Seq(v) ++ traverseDF(l) ++ traverseDF(r)
}
