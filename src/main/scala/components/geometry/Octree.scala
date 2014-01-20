package components.geometry

import data.Types._
import scala.collection.immutable.VectorBuilder


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

    def intersects(triangle: Triangle): Boolean = {
      val Box(olo, ohi) = boundingBox(triangle)
      def less(p1: P, p2: P): Boolean = {
        val d = p2 - p1
        d.x > 0 || d.y > 0 || d.z > 0
      }
      !(less(hi, olo) || less(ohi, lo))
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
}

object Octree {
  def apply(shapes: Iterable[Shape], depth: Int): Octree = {
    new Octree(shapes, depth)
  }
}