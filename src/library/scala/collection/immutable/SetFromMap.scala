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
package immutable

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.SetFromMapOps.WrappedMap

trait SetFromMapOps[A, +MM[K, +V] <: MapOps[K, V, MM, MM[K, V]], +CC[_], +C <: SetFromMapOps[A, MM, CC, C]]
  extends SetOps[A, CC, C]
    with WrappedMap[A]
    with Serializable {
  protected[immutable] val underlying: MM[A, Unit]

  def contains(elem: A): Boolean = underlying contains elem

  def iterator: Iterator[A] = underlying.keysIterator
}

object SetFromMapOps {
  // top type to make pattern matching easier
  sealed trait WrappedMap[A] extends IterableOnce[A] {
    protected[immutable] val underlying: IterableOnce[(A, Unit)]
  }

  trait Unsorted[A, +MM[K, +V] <: MapOps[K, V, MM, MM[K, V]], +CC[_], +C <: Unsorted[A, MM, CC, C]]
    extends SetFromMapOps[A, MM, CC, C] {
    protected[this] def fromMap(m: MM[A, Unit] @uV): C

    def incl(elem: A): C = fromMap(underlying.updated(elem, ()))

    override def concat(that: IterableOnce[A]): C = fromMap {
      that match {
        case coll: WrappedMap[A] => underlying concat coll.underlying
        case coll                => underlying concat coll.iterator.map((_, ()))
      }
    }

    def excl(elem: A): C = fromMap(underlying removed elem)

    override def removedAll(that: IterableOnce[A]): C = fromMap(underlying removedAll that)
  }
}

abstract class SetFromMapFactory[+MM[K, +V] <: Map[K, V], +CC[A] <: SetFromMapOps.WrappedMap[A]](mf: MapFactory[MM])
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

@SerialVersionUID(3L)
class SetFromMap[A](protected[immutable] val underlying: Map[A, Unit])
  extends AbstractSet[A]
    with SetFromMapOps.Unsorted[A, Map, SetFromMap, SetFromMap[A]]
    with IterableFactoryDefaults[A, SetFromMap]
    with DefaultSerializable {
  protected[this] def fromMap(m: Map[A, Unit]): SetFromMap[A] = new SetFromMap(m)

  override def iterableFactory: IterableFactory[SetFromMap] =
    new SetFromMap.WrapperFactory(underlying.mapFactory)
}

object SetFromMap {
  def apply(factory: MapFactory[Map]): IterableFactory[Set] = new WrapperFactory(factory)
  def apply(factory: MapFactory[SeqMap])(implicit d: DummyImplicit): IterableFactory[SeqSet] = SeqSetFromMap(factory)
  def apply(factory: SortedMapFactory[SortedMap]): SortedIterableFactory[SortedSet] = SortedSetFromMap(factory)

  @SerialVersionUID(3L)
  private class WrapperFactory(mf: MapFactory[Map])
    extends SetFromMapFactory[Map, SetFromMap](mf) {
    protected[this] def fromMap[A](map: Map[A, Unit]): SetFromMap[A] = new SetFromMap(map)
  }
}

@SerialVersionUID(3L)
class SeqSetFromMap[A](protected[immutable] val underlying: SeqMap[A, Unit])
  extends AbstractSet[A]
    with SeqSet[A]
    with SetFromMapOps.Unsorted[A, SeqMap, SeqSetFromMap, SeqSetFromMap[A]]
    with IterableFactoryDefaults[A, SeqSetFromMap]
    with DefaultSerializable {
  protected[this] def fromMap(m: SeqMap[A, Unit]): SeqSetFromMap[A] = new SeqSetFromMap(m)

  override def iterableFactory: IterableFactory[SeqSetFromMap] =
    new SeqSetFromMap.WrapperFactory(underlying.mapFactory)
}

object SeqSetFromMap {
  def apply(factory: MapFactory[SeqMap]): IterableFactory[SeqSet] = new WrapperFactory(factory)

  @SerialVersionUID(3L)
  private class WrapperFactory(mf: MapFactory[SeqMap])
    extends SetFromMapFactory[SeqMap, SeqSetFromMap](mf) {
    protected[this] def fromMap[A](map: SeqMap[A, Unit]): SeqSetFromMap[A] = new SeqSetFromMap(map)
  }
}

@SerialVersionUID(3L)
class SortedSetFromMap[A](protected[immutable] val underlying: SortedMap[A, Unit])(implicit val ordering: Ordering[A])
  extends AbstractSet[A]
    with SetFromMapOps[A, Map, Set, SortedSetFromMap[A]]
    with SortedSet[A]
    with SortedSetOps[A, SortedSetFromMap, SortedSetFromMap[A]]
    with IterableFactoryDefaults[A, Set]
    with SortedSetFactoryDefaults[A, SortedSetFromMap, Set]
    with DefaultSerializable {
  import SortedSetFromMap.ssfm

  override def iterableFactory: IterableFactory[Set] =
    SetFromMap(underlying.mapFactory)

  override def sortedIterableFactory: SortedIterableFactory[SortedSetFromMap] =
    new SortedSetFromMap.WrapperFactory(underlying.sortedMapFactory)

  override def incl(elem: A): SortedSetFromMap[A] = ssfm(underlying.updated(elem, ()))

  override def concat(that: IterableOnce[A]): SortedSetFromMap[A] = ssfm {
    that match {
      case coll: WrappedMap[A] => underlying concat coll.underlying
      case coll                => underlying concat coll.iterator.map((_, ()))
    }
  }

  override def excl(elem: A): SortedSetFromMap[A] = ssfm(underlying removed elem)

  override def removedAll(that: IterableOnce[A]): SortedSetFromMap[A] = ssfm(underlying removedAll that)

  def iteratorFrom(start: A): Iterator[A] = underlying.keysIteratorFrom(start)

  def rangeImpl(from: Option[A], until: Option[A]): SortedSetFromMap[A] = ssfm(underlying.rangeImpl(from, until))
}

object SortedSetFromMap {
  @inline private def ssfm[B: Ordering](map: SortedMap[B, Unit]): SortedSetFromMap[B] = new SortedSetFromMap[B](map)

  def apply(factory: SortedMapFactory[SortedMap]): SortedIterableFactory[SortedSet] = new WrapperFactory(factory)

  @SerialVersionUID(3L)
  private final class WrapperFactory(smf: SortedMapFactory[SortedMap])
    extends SortedIterableFactory[SortedSetFromMap]
      with Serializable {
    def from[A: Ordering](it: IterableOnce[A]): SortedSetFromMap[A] =
      it match {
        case coll: WrappedMap[A] => ssfm(smf from coll.underlying)
        case coll                => newBuilder[A].addAll(coll).result()
      }

    def empty[A: Ordering]: SortedSetFromMap[A] = ssfm(smf.empty)

    def newBuilder[A: Ordering]: mutable.Builder[A, SortedSetFromMap[A]] = new WrapperBuilder[A](smf.newBuilder)
  }

  private final class WrapperBuilder[A: Ordering](b: mutable.Builder[(A, Unit), SortedMap[A, Unit]])
    extends mutable.Builder[A, SortedSetFromMap[A]] {
    def clear(): Unit = b.clear()
    def result(): SortedSetFromMap[A] = ssfm(b.result())
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
