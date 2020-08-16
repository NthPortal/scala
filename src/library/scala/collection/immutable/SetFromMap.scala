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

import scala.collection.generic.DefaultSerializable
import scala.collection.SetFromMapOps.WrappedMap

trait SetFromMapOps[A, +CC[_], +C <: SetFromMapOps[A, CC, C]]
  extends SetOps[A, CC, C]
    with collection.SetFromMapOps[A, CC, C]

object SetFromMapOps {
  trait Unsorted[A, +MM[K, +V] <: collection.Map[K, V] with MapOps[K, V, MM, MM[K, V]], +CC[_], +C <: Unsorted[A, MM, CC, C]]
    extends SetFromMapOps[A, CC, C] {
    protected[collection] val underlying: MM[A, Unit]

    protected[this] def fromMap(m: MM[A, Unit]): C

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

@SerialVersionUID(3L)
class SetFromMap[A](protected[collection] val underlying: Map[A, Unit])
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

  @SerialVersionUID(3L)
  private class WrapperFactory(mf: MapFactory[Map]) extends SetFromMapFactory[Map, SetFromMap](mf) {
    protected[this] def fromMap[A](map: Map[A, Unit]): SetFromMap[A] =
      new SetFromMap(map)
  }
}

@SerialVersionUID(3L)
class SeqSetFromMap[A](protected[collection] val underlying: SeqMap[A, Unit])
  extends AbstractSet[A]
    with SeqSet[A]
    with SetFromMapOps.Unsorted[A, SeqMap, SeqSetFromMap, SeqSetFromMap[A]]
    with IterableFactoryDefaults[A, SeqSetFromMap]
    with DefaultSerializable {
  protected[this] def fromMap(m: SeqMap[A, Unit]): SeqSetFromMap[A] =
    new SeqSetFromMap(m)

  override def iterableFactory: IterableFactory[SeqSetFromMap] =
    new SeqSetFromMap.WrapperFactory(underlying.mapFactory)
}

object SeqSetFromMap {
  def apply(factory: MapFactory[SeqMap]): IterableFactory[SeqSet] = new WrapperFactory(factory)

  @SerialVersionUID(3L)
  private class WrapperFactory(mf: MapFactory[SeqMap]) extends SetFromMapFactory[SeqMap, SeqSetFromMap](mf) {
    protected[this] def fromMap[A](map: SeqMap[A, Unit]): SeqSetFromMap[A] =
      new SeqSetFromMap(map)
  }
}

@SerialVersionUID(3L)
class SortedSetFromMap[A](protected[collection] val underlying: SortedMap[A, Unit])(implicit val ordering: Ordering[A])
  extends AbstractSet[A]
    with SetFromMapOps[A, Set, SortedSetFromMap[A]]
    with collection.SetFromMapOps.Sorted[A, SortedSetFromMap, SortedSetFromMap[A]]
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

  def rangeImpl(from: Option[A], until: Option[A]): SortedSetFromMap[A] = ssfm(underlying.rangeImpl(from, until))
}

object SortedSetFromMap {
  @inline private def ssfm[B: Ordering](map: SortedMap[B, Unit]): SortedSetFromMap[B] =
    new SortedSetFromMap[B](map)

  def apply(factory: SortedMapFactory[SortedMap]): SortedIterableFactory[SortedSet] =
    new WrapperFactory(factory)

  @SerialVersionUID(3L)
  private final class WrapperFactory(mf: SortedMapFactory[SortedMap])
    extends SortedSetFromMapFactory[SortedMap, SortedSetFromMap](mf) {
    protected[this] def fromMap[A: Ordering](map: SortedMap[A, Unit]): SortedSetFromMap[A] =
      new SortedSetFromMap(map)
  }
}
