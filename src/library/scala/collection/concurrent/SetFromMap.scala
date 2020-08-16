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
package concurrent

import scala.collection.generic.DefaultSerializable

@SerialVersionUID(3L)
class SetFromMap[A](protected[collection] val underlying: Map[A, Unit],
                    override val iterableFactory: IterableFactory[SetFromMap])
  extends mutable.AbstractSet[A]
    with Set[A]
    with mutable.SetFromMapOps[A, SetFromMap, SetFromMap[A]]
    with IterableFactoryDefaults[A, SetFromMap]
    with DefaultSerializable {
  override protected[this] def className: String = "SetFromMap"
}

object SetFromMap extends SetFromMapMetaFactory[Map, Set] {
  def apply(factory: MapFactory[Map]): IterableFactory[SetFromMap] = new WrapperFactory(factory)
  def apply[A](map: Map[A, Unit]): Set[A] = {
    val factory = map.mapFactory
    val safeFactory = factory.empty match {
      case _: Map[_, _] => factory.asInstanceOf[MapFactory[Map]] // trust as concurrent
      case _            => TrieMap
    }
    new SetFromMap(map, apply(safeFactory))
  }

  @SerialVersionUID(3L)
  private class WrapperFactory(mf: MapFactory[Map]) extends SetFromMapFactory[Map, SetFromMap](mf) {
    protected[this] def fromMap[A](map: Map[A, Unit]): SetFromMap[A] = {
      // we have to pass `this` in because `concurrent.Map#mapFactory` returns
      // a `MapFactory[mutable.Map]`, not a `MapFactory[concurrent.Map]` :(
      new SetFromMap(map, this)
    }
  }
}
