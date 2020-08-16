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
package mutable

import scala.collection.SetFromMapOps.WrappedMap
import scala.collection.generic.DefaultSerializable

trait SetFromMapOps[A, +CC[_], +C <: SetFromMapOps[A, CC, C]]
  extends SetOps[A, CC, C]
    with collection.SetFromMapOps[A, CC, C] {
  protected[collection] val underlying: Map[A, Unit]

  def clear(): Unit = underlying.clear()

  def addOne(elem: A): this.type = { underlying.update(elem, ()); this }

  override def add(elem: A): Boolean = underlying.put(elem, ()).isEmpty

  override def addAll(xs: IterableOnce[A]): this.type =
    xs match {
      case coll: WrappedMap[A] =>
        underlying.addAll(coll.underlying)
        this
      case coll => super.addAll(coll)
    }

  def subtractOne(elem: A): this.type = { underlying.subtractOne(elem); this }

  override def remove(elem: A): Boolean = underlying.remove(elem).isDefined

  override def subtractAll(xs: IterableOnce[A]): this.type = { underlying.subtractAll(xs); this }
}

@SerialVersionUID(3L)
class SetFromMap[A](protected[collection] val underlying: Map[A, Unit])
  extends AbstractSet[A]
    with SetFromMapOps[A, SetFromMap, SetFromMap[A]]
    with IterableFactoryDefaults[A, SetFromMap]
    with DefaultSerializable {
  override protected[this] def className: String = "SetFromMap"

  override def iterableFactory: IterableFactory[SetFromMap] = SetFromMap(underlying.mapFactory)
}

object SetFromMap extends SetFromMapMetaFactory[Map, Set] {
  def apply(factory: MapFactory[Map]): IterableFactory[SetFromMap] = new WrapperFactory(factory)
  def apply[A](map: Map[A, Unit]): Set[A] = new SetFromMap(map)

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
    with SetFromMapOps[A, SeqSetFromMap, SeqSetFromMap[A]]
    with IterableFactoryDefaults[A, SeqSetFromMap]
    with DefaultSerializable {
  override protected[this] def className: String = "SeqSetFromMap"

  override def iterableFactory: IterableFactory[SeqSetFromMap] = SeqSetFromMap(underlying.mapFactory)
}

object SeqSetFromMap extends SetFromMapMetaFactory[SeqMap, SeqSet] {
  def apply(factory: MapFactory[SeqMap]): IterableFactory[SeqSetFromMap] = new WrapperFactory(factory)
  def apply[A](map: SeqMap[A, Unit]): mutable.SeqSet[A] = new SeqSetFromMap(map)

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
    with SetFromMapOps.Sorted[A, SortedSetFromMap, SortedSetFromMap[A]]
    with SortedSet[A]
    with SortedSetOps[A, SortedSetFromMap, SortedSetFromMap[A]]
    with IterableFactoryDefaults[A, Set]
    with SortedSetFactoryDefaults[A, SortedSetFromMap, Set]
    with DefaultSerializable {
  override protected[this] def className: String = "SortedSetFromMap"

  override def iterableFactory: IterableFactory[Set] =
    SetFromMap(underlying.mapFactory)

  override def sortedIterableFactory: SortedIterableFactory[SortedSetFromMap] =
    new SortedSetFromMap.WrapperFactory(underlying.sortedMapFactory)

  def rangeImpl(from: Option[A], until: Option[A]): SortedSetFromMap[A] =
    new SortedSetFromMap(underlying.rangeImpl(from, until))
}

object SortedSetFromMap extends SortedSetFromMapMetaFactory[SortedMap, SortedSet] {
  def apply(factory: SortedMapFactory[SortedMap]): SortedIterableFactory[SortedSet] =
    new WrapperFactory(factory)
  def apply[A](map: SortedMap[A, Unit]): mutable.SortedSet[A] = new SortedSetFromMap(map)(map.ordering)

  @SerialVersionUID(3L)
  private final class WrapperFactory(mf: SortedMapFactory[SortedMap])
    extends SortedSetFromMapFactory[SortedMap, SortedSetFromMap](mf) {
    protected[this] def fromMap[A: Ordering](map: SortedMap[A, Unit]): SortedSetFromMap[A] =
      new SortedSetFromMap(map)
  }
}
