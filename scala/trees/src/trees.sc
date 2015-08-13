sealed trait Tree[+A]

case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
case class Empty() extends Tree[Nothing]

def traverseDF[A](tree: Tree[A]): Seq[A] = tree match {
  case Empty() => Seq()
  case Node(v, l, r) => Seq(v) ++ traverseDF(l) ++ traverseDF(r)
}

val t: Node[Char] = Node('A',
  Node('B',
    Node('C',
      Empty(),
      Node('D',
        Empty(),
        Empty()
      )
    ),
    Node('E',
      Node('F',
        Empty(),
        Empty()
      ),
      Empty()
    )
  ),
  Empty()
)

traverseDF(t)