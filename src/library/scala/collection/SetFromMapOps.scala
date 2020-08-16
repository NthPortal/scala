/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import scala.collection.SetFromMapOps.WrappedMap

trait SetFromMapOps[A, +CC[_], +C <: SetFromMapOps[A, CC, C]]
  extends SetOps[A, CC, C]
    with WrappedMap[A]
    with Serializable {
  protected[collection] val underlying: Map[A, Unit]

  def contains(elem: A): Boolean = underlying contains elem

  def iterator: Iterator[A] = underlying.keysIterator
}

object SetFromMapOps {
  // top type to make pattern matching easier
  sealed trait WrappedMap[A] extends IterableOnce[A] {
    protected[collection] val underlying: IterableOnce[(A, Unit)]
  }

  // unknown whether mutable or immutable
  trait Unknown[A, +CC[_], +C <: SetFromMapOps[A, CC, C]]
    extends SetFromMapOps[A, CC, C] {
    def diff(that: Set[A]): C =
      toIterable
        .foldLeft(newSpecificBuilder)((b, elem) => if (that contains elem) b else b += elem)
        .result()
  }

  trait Sorted[A, +CC[X] <: SortedSet[X], +C <: SetFromMapOps[A, Set, C] with SortedSetOps[A, CC, C]]
    extends SetFromMapOps[A, Set, C]
      with SortedSetOps[A, CC, C] {
    override protected[collection] val underlying: SortedMap[A, Unit]

    def iteratorFrom(start: A): Iterator[A] = underlying.keysIteratorFrom(start)
  }
}

abstract class SetFromMapFactory[+MM[K, V] <: Map[K, V], +CC[A] <: WrappedMap[A]](mf: MapFactory[MM])
  extends IterableFactory[CC]
    with Serializable {
  protected[this] def fromMap[A](map: MM[A, Unit]): CC[A]

  def from[A](source: IterableOnce[A]): CC[A] =
    source match {
      case coll: WrappedMap[A] => fromMap(mf from coll.underlying)
      case coll                => newBuilder[A].addAll(coll).result()
    }

  def empty[A]: CC[A] = fromMap(mf.empty)
  def newBuilder[A]: mutable.Builder[A, CC[A]] = new WrappedBuilder(mf.newBuilder)

  private final class WrappedBuilder[A](b: mutable.Builder[(A, Unit), MM[A, Unit]])
    extends mutable.Builder[A, CC[A]] {
    def clear(): Unit = b.clear()
    def result(): CC[A] = fromMap(b.result())
    def addOne(elem: A): this.type = { b.addOne((elem, ())); this }

    override def addAll(xs: IterableOnce[A]): this.type =
      xs match {
        case coll: WrappedMap[A] =>
          b.addAll(coll.underlying)
          this
        case coll => super.addAll(coll)
      }

    override def sizeHint(size: Int): Unit = b.sizeHint(size)
  }
}

abstract class SortedSetFromMapFactory[+MM[K, V] <: SortedMap[K, V], +CC[A] <: WrappedMap[A]](mf: SortedMapFactory[MM])
  extends SortedIterableFactory[CC]
    with Serializable {
  protected[this] def fromMap[A: Ordering](map: MM[A, Unit]): CC[A]

  def from[A: Ordering](it: IterableOnce[A]): CC[A] =
    it match {
      case coll: WrappedMap[A] => fromMap(mf from coll.underlying)
      case coll                => newBuilder[A].addAll(coll).result()
    }

  def empty[A: Ordering]: CC[A] = fromMap(mf.empty)

  def newBuilder[A: Ordering]: mutable.Builder[A, CC[A]] = new WrapperBuilder[A](mf.newBuilder)

  private final class WrapperBuilder[A: Ordering](b: mutable.Builder[(A, Unit), MM[A, Unit]])
    extends mutable.Builder[A, CC[A]] {
    def clear(): Unit = b.clear()
    def result(): CC[A] = fromMap(b.result())
    def addOne(elem: A): this.type = { b.addOne((elem, ())); this }

    override def addAll(xs: IterableOnce[A]): this.type =
      xs match {
        case coll: WrappedMap[A] =>
          b.addAll(coll.underlying)
          this
        case coll => super.addAll(coll)
      }

    override def sizeHint(size: Int): Unit = b.sizeHint(size)
  }
}
