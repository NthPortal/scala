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

trait SetFromMapOps[
  A,
  +MM[K, V] <: MapOps[K, V, MM, _],
  +M <: MapOps[A, Unit, MM, M],
  +CC[_],
  +C <: SetFromMapOps[A, MM, M, CC, C]
]
  extends SetOps[A, CC, C]
    with WrappedMap[A]
    with Serializable {
  protected[collection] val underlying: M

  protected[this] def fromMap[B](m: MM[B, Unit]): CC[B]
  protected[this] def fromSpecificMap(m: M): C

  protected[this] def adapted[R](f: A => R): ((A, Unit)) => R = { case (elem, _) => f(elem) }

  def contains(elem: A): Boolean = underlying contains elem
  def iterator: Iterator[A] = underlying.keysIterator

  override def isEmpty: Boolean = underlying.isEmpty
  override def size: Int = underlying.size
  override def knownSize: Int = underlying.knownSize

  override def head: A = underlying.head._1
  override def headOption: Option[A] = underlying.headOption.map(_._1)
  override def last: A = underlying.last._1
  override def lastOption: Option[A] = underlying.lastOption.map(_._1)

  override def tapEach[U](f: A => U): C = fromSpecificMap(underlying.tapEach(adapted(f)))
}

object SetFromMapOps {
  // top type to make pattern matching easier
  sealed trait WrappedMap[A] extends IterableOnce[A] {
    protected[collection] val underlying: IterableOnce[(A, Unit)]
  }

  // unknown whether mutable or immutable
  trait Unknown[
    A,
    +MM[K, V] <: MapOps[K, V, MM, _],
    +M <: MapOps[A, Unit, MM, M],
    +CC[_],
    +C <: Unknown[A, MM, M, CC, C]
  ]
    extends SetFromMapOps[A, MM, M, CC, C] {
    def diff(that: Set[A]): C =
      toIterable
        .foldLeft(newSpecificBuilder)((b, elem) => if (that contains elem) b else b += elem)
        .result()
  }

  trait Unsorted[A, +MM[K, V] <: MapOps[K, V, MM, MM[K, V]], +CC[X] <: Unsorted[X, MM, CC]]
    extends SetFromMapOps[A, MM, MM[A, Unit], CC, CC[A]] {
    protected[this] final def fromSpecificMap(m: MM[A, Unit]): CC[A] = fromMap(m)
  }

  trait Sorted[
    A,
    +MM[K, V] <: Map[K, V] with SortedMapOps[K, V, MM, MM[K, V]],
    +UnsortedCC[X] <: Set[X],
    +CC[X] <: Sorted[X, MM, UnsortedCC, CC] with SortedSet[X] with SortedSetOps[X, CC, CC[X]]
  ]
    extends SetFromMapOps[A, Map, MM[A, Unit], UnsortedCC, CC[A]]
      with SortedSetOps[A, CC, CC[A]] {
    override protected[collection] val underlying: MM[A, Unit]

    @inline protected[this] final implicit def implicitOrd: Ordering[A] = ordering

    protected[this] def fromSortedMap[B: Ordering](m: MM[B, Unit]): CC[B]
    protected[this] final def fromSpecificMap(m: MM[A, Unit]): CC[A] = fromSortedMap(m)

    def iteratorFrom(start: A): Iterator[A] = underlying.keysIteratorFrom(start)

    def rangeImpl(from: Option[A], until: Option[A]): CC[A] =
      fromSortedMap(underlying.rangeImpl(from, until))

    override def head: A = underlying.firstKey
    override def firstKey: A = underlying.firstKey
    override def headOption: Option[A] = underlying.headOption.map(_._1)
    override def last: A = underlying.lastKey
    override def lastKey: A = underlying.lastKey
    override def lastOption: Option[A] = underlying.lastOption.map(_._1)
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

sealed abstract class SetFromMapCreator[MM[K, V] <: Map[K, V], +CC[A] <: Set[A]] {
  def apply[A](map: MM[A, Unit]): CC[A]
}

abstract class SetFromMapMetaFactory[MM[K, V] <: Map[K, V], +CC[A] <: Set[A]]
  extends SetFromMapCreator[MM, CC] {
  def apply(factory: MapFactory[MM]): IterableFactory[CC]
}

abstract class SortedSetFromMapMetaFactory[MM[K, V] <: SortedMap[K, V], +CC[A] <: SortedSet[A]]
  extends SetFromMapCreator[MM, CC] {
  def apply(factory: SortedMapFactory[MM]): SortedIterableFactory[CC]
}
