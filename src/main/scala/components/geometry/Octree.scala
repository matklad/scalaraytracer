package components.geometry

import data.Types._
import scala.collection.immutable.VectorBuilder
import simplex.Dictionary


class Octree(shapes: Iterable[Shape], depth: Int) extends Index {

  def intersect(r: R): Intersection = {
    dumb.intersect(r) >< intersect(r, root)
  }

  private val trianglesPerNode = 10
  private val (dumb: DumbIndex, root: Tree) = {
    val (triangles: Seq[Triangle], others: Seq[Shape]) = {
      val tb = new VectorBuilder[Triangle]()
      val ob = new VectorBuilder[Shape]()
      shapes foreach {
        case t: Triangle => tb += t
        case o: Shape => ob += o
      }
      (tb.result(), ob.result())
    }

    val dumb = DumbIndex(others)
    val tree = makeTree(triangles, depth)
    (dumb, tree)
  }

  private case class Box(lo: P, hi: P) {
    val diagonal = hi - lo

    def union(other: Box) = Box(
      P.lowerBound(lo, other.lo),
      P.upperBound(hi, other.hi))

    def split8: Vector[Box] = {
      val half = diagonal * .5
      val orts = List(V.i, V.j, V.k) map (x => x * (x dot half))
      val boxes = for {
        di <- List(0, 1)
        dj <- List(0, 1)
        dk <- List(0, 1)
        dl = List(di, dj, dk)
        shift = ((orts, dl).zipped map ((v, i) => v * i)).reduce(_ + _)
        nlo = lo + shift
      } yield Box(nlo, nlo + half)
      assert(boxes.length == 8)
      boxes.toVector
    }

    def intersects(triangle: Triangle) =
      intersectsBox(triangle) && intersectsIndeed(triangle)


    private def intersectsBox(triangle: Triangle): Boolean = {
      val Box(olo, ohi) = boundingBox(triangle)
      def less(p1: P, p2: P): Boolean = {
        val d = p2 - p1
        d.x > 0 || d.y > 0 || d.z > 0
      }
      !(less(hi, olo) || less(ohi, lo))
    }

    private def intersectsIndeed(triangle: Triangle): Boolean = {
      val ab = triangle.ab
      val ac = triangle.ac
      val l = lo - triangle.a
      val h = hi - triangle.a
      //    l < p*ab + q*ac < h
      //
      //    max -t
      //    -t + p*ab  + q*ac < h
      //    -t - p*ab  - q*ac < -l
      //    -t + p            < 1
      //    -t         + q    < 1
      //    -t + p     + q    < 1

      val pre = Dictionary(10, 4,

        0, -1, 0, 0,
        h.x, 1, -ab.x, -ac.x,
        h.y, 1, -ab.y, -ac.y,
        h.z, 1, -ab.z, -ac.z,
        -l.x, 1, ac.x, ac.x,
        -l.y, 1, ac.y, ac.y,
        -l.z, 1, ac.z, ac.z,
        1, 1, -1, 0,
        1, 1, 0, -1,
        1, 1, -1, -1

      )
      val d = pre.magicPivot
      d.opt.target > -1e-6
    }


    def intersects(ray: R): Boolean = {
      val ll = (lo - ray.origin) :/ ray.direction
      val hh = (hi - ray.origin) :/ ray.direction

      val l = V.lowerBound(ll, hh)
      val h = V.upperBound(ll, hh)
      val lt = math.max(math.max(l.x, l.y), math.max(l.z, 0.0))
      val ht = math.min(math.min(h.x, h.y), h.z)
      lt <= ht
    }

  }

  private case class Tree(box: Box, node: Node)

  private abstract sealed class Node

  private case class Branch(children: Array[Tree]) extends Node

  private case class Leaf(triangles: Array[Triangle]) extends Node

  private def makeTree(triangles: Seq[Triangle], depth: Int): Tree = {
    val box = boundingBox(triangles: _*)

    Tree(box, makeNode(box, triangles, depth))
  }

  private def makeNode(box: Box, triangles: Seq[Triangle], depth: Int): Node = {
    val stop = (depth == 0) || triangles.size < trianglesPerNode
    if (stop)
      Leaf(triangles.toArray)
    else {
      val octobox = box.split8
      val children = for {
        subBox <- octobox
        boxTriangles = triangles.filter(subBox.intersects)
      } yield Tree(subBox, makeNode(subBox, boxTriangles, depth - 1))
      Branch(children.toArray)
    }
  }

  private def boundingBox(triangles: Triangle*): Box = {
    def boundOne(t: Triangle): Box = Box(
      P.lowerBound(P.lowerBound(t.a, t.b), t.c),
      P.upperBound(P.upperBound(t.a, t.b), t.c))

    triangles map boundOne reduce (_ union _)
  }

  private def intersect(r: R, tree: Tree): Intersection = {
    if (!tree.box.intersects(r))
      Intersection.zero
    else tree.node match {
      case Branch(trees) => (trees.view map (intersect(r, _))).fold(Intersection.zero)(_ >< _)
      case Leaf(triangles) =>
        (triangles.view map (Intersection(r, _))).fold(Intersection.zero)(_ >< _)

    }
  }

  private def totalTriangles = {
    def ttRec(t: Tree): Int = t.node match {
      case Branch(children) => children.map(ttRec).sum
      case Leaf(ts) => ts.size
    }
    ttRec(root)
  }


}

object Octree {
  def apply(shapes: Iterable[Shape], depth: Int): Octree = {
    new Octree(shapes, depth)
  }
}