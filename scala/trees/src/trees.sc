import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}

sealed trait Tree[+A] {
  def children(): Seq[Tree[A]]
}

case object Empty extends Tree[Nothing] {
  override def children(): Seq[Tree[Nothing]] = Seq()
}

case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def children(): Seq[Tree[A]] = this match {
    case Node(_, Empty, Empty) => Seq()
    case Node(_, l, Empty) => Seq(l)
    case Node(_, Empty, r) => Seq(r)
    case Node(_, l, r) => Seq(l, r)
  }
}

implicit object TreeFunctor extends Functor[Tree] {
  override def fmap[A, B](f: (A) => B): (Tree[A]) => Tree[B] = (tree: Tree[A]) => tree match {
    case Empty => Empty
    case Node(v, l, r) => Node(f(v), fmap(f)(l), fmap(f)(r))
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

def traverseBFPrime[A](tree: Tree[A]): Seq[A] = {
  def nval(t: Tree[A]): A = t match {
    case Node(v, _, _) => v
  }

  def go(trees: List[Tree[A]]): Seq[A] = trees match {
    case Nil => List()
    case ts =>
      ts.map(nval) ++ go(ts.flatMap(_.children()))
  }

  go(List(tree))
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
traverseBFPrime(t)
