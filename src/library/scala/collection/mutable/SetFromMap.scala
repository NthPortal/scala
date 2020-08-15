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

import scala.collection.generic.DefaultSerializable

trait SetFromMapOps[A, +MM[K, V] <: MapOps[K, V, MM, MM[K, V]], +CC[_], +C <: SetFromMapOps[A, MM, CC, C]]
  extends SetOps[A, CC, C]
    with collection.SetFromMapOps[A, MM, CC, C] {
  def clear(): Unit = underlying.clear()

  def addOne(elem: A): this.type = { underlying.update(elem, ()); this }

  override def add(elem: A): Boolean = underlying.put(elem, ()).isEmpty

  def subtractOne(elem: A): this.type = { underlying.subtractOne(elem); this }

  override def remove(elem: A): Boolean = underlying.remove(elem).isDefined
}

@SerialVersionUID(3L)
class SetFromMap[A](protected[collection] val underlying: Map[A, Unit])
  extends AbstractSet[A]
    with SetFromMapOps[A, Map, SetFromMap, SetFromMap[A]]
    with IterableFactoryDefaults[A, SetFromMap]
    with DefaultSerializable {
  override def iterableFactory: IterableFactory[SetFromMap] = SetFromMap(underlying.mapFactory)
}

object SetFromMap {
  def apply(factory: MapFactory[Map]): IterableFactory[SetFromMap] = new WrapperFactory(factory)

  @SerialVersionUID(3L)
  private class WrapperFactory(mf: MapFactory[Map]) extends SetFromMapFactory[Map, SetFromMap](mf) {
    protected[this] def fromMap[A](map: Map[A, Unit]): SetFromMap[A] =
      new SetFromMap(map)
  }
}

@SerialVersionUID(3L)
class SeqSetFromMap[A](protected[collection] val underlying: Map[A, Unit])
  extends AbstractSet[A]
    with SeqSet[A]
    with SetFromMapOps[A, Map, SeqSetFromMap, SeqSetFromMap[A]]
    with IterableFactoryDefaults[A, SeqSetFromMap]
    with DefaultSerializable {
  override def iterableFactory: IterableFactory[SeqSetFromMap] = SeqSetFromMap(underlying.mapFactory)
}

object SeqSetFromMap {
  def apply(factory: MapFactory[Map]): IterableFactory[SeqSetFromMap] = new WrapperFactory(factory)

  @SerialVersionUID(3L)
  private class WrapperFactory(mf: MapFactory[Map]) extends SetFromMapFactory[Map, SeqSetFromMap](mf) {
    protected[this] def fromMap[A](map: Map[A, Unit]): SeqSetFromMap[A] =
      new SeqSetFromMap(map)
  }
}

@SerialVersionUID(3L)
class SortedSetFromMap[A](protected[collection] val underlying: SortedMap[A, Unit])(implicit val ordering: Ordering[A])
  extends AbstractSet[A]
    with SetFromMapOps[A, Map, Set, SortedSetFromMap[A]]
    with SortedSet[A]
    with SortedSetOps[A, SortedSetFromMap, SortedSetFromMap[A]]
    with IterableFactoryDefaults[A, Set]
    with SortedSetFactoryDefaults[A, SortedSetFromMap, Set]
    with DefaultSerializable {
  override def iterableFactory: IterableFactory[Set] =
    SetFromMap(underlying.mapFactory)

  override def sortedIterableFactory: SortedIterableFactory[SortedSetFromMap] =
    new SortedSetFromMap.WrapperFactory(underlying.sortedMapFactory)

  def iteratorFrom(start: A): Iterator[A] = underlying.keysIteratorFrom(start)

  def rangeImpl(from: Option[A], until: Option[A]): SortedSetFromMap[A] =
    new SortedSetFromMap(underlying.rangeImpl(from, until))
}

object SortedSetFromMap {
  def apply(factory: SortedMapFactory[SortedMap]): SortedIterableFactory[SortedSet] =
    new WrapperFactory(factory)

  @SerialVersionUID(3L)
  private final class WrapperFactory(mf: SortedMapFactory[SortedMap])
    extends SortedSetFromMapFactory[SortedMap, SortedSetFromMap](mf) {
    protected[this] def fromMap[A: Ordering](map: SortedMap[A, Unit]): SortedSetFromMap[A] =
      new SortedSetFromMap(map)
  }
}
