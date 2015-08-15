import scala.collection.mutable

sealed trait Tree[+A] {
  def fmap[B](f: A => B): Tree[B]
}
case object Empty extends Tree[Nothing] {
  def fmap[B](f: Nothing => B) = Empty
}
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
  def fmap[B](f: A => B) =
    Node(f(value), left.fmap(f), right.fmap(f))

  def children(): Seq[Tree[A]] = this match {
    case Node(_, Empty, Empty) => Seq()
    case Node(_, l, Empty) => Seq(l)
    case Node(_, Empty, r) => Seq(r)
    case Node(_, l, r) => Seq(l, r)
  }
}

def traverseDF[A](tree: Tree[A]): Seq[A] = tree match {
  case Empty => Seq()
  case Node(v, l, r) => Seq(v) ++ traverseDF(l) ++ traverseDF(r)
}

def traverseBF[A](tree: Tree[A]): Seq[A] = tree match {
  case Empty => Seq()
  case node =>
    val queue = mutable.Queue[Tree[A]](node)
    val visited = mutable.Set[Tree[A]]()
    val result = mutable.ListBuffer[A]()

    while (queue.nonEmpty) {
      queue.dequeue() match {
        case Empty => ()
        case n@Node(v, _, _) =>
          n.children().filterNot { visited.contains }.foreach { queue.enqueue(_) }

          if (!visited.contains(n)) {
            visited.add(n)
            result.append(v)
          }
      }
    }
    result.toSeq
}

val t: Node[Char] = Node('A',
  Node('B',
    Node('C',
      Empty,
      Node('D',
        Empty,
        Empty
      )
    ),
    Node('E',
      Node('F',
        Empty,
        Empty
      ),
      Empty
    )
  ),
  Empty
)
traverseDF(t)
traverseBF(t)