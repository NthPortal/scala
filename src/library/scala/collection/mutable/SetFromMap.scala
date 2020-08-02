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

package scala.collection
package mutable

@SerialVersionUID(3L)
class SetFromMap[A](private val underlying: Map[A, Unit])
  extends AbstractSet[A]
  with SetOps[A, SetFromMap, SetFromMap[A]]
  with IterableFactoryDefaults[A, SetFromMap]
  with Serializable { // TODO: fix serialization
  import SetFromMap.sfm

  override def iterableFactory: IterableFactory[SetFromMap] = SetFromMap.fromMap(underlying.mapFactory)

  def contains(elem: A): Boolean = underlying contains elem

  def iterator: Iterator[A] = underlying.keysIterator

  def clear(): Unit = underlying.clear()

  def addOne(elem: A): this.type = { underlying.update(elem, ()); this }

  override def add(elem: A): Boolean = underlying.put(elem, ()).isEmpty

  def subtractOne(elem: A): this.type = { underlying.subtractOne(elem); this }

  override def remove(elem: A): Boolean = underlying.remove(elem).isDefined
}

object SetFromMap {
  @inline private def sfm[B](map: Map[B, Unit]): SetFromMap[B] = new SetFromMap[B](map)

  def fromMap(factory: MapFactory[Map]): IterableFactory[SetFromMap] = new WrapperFactory(factory)

  private class WrapperFactory(mf: MapFactory[Map]) extends IterableFactory[SetFromMap] {
    def from[A](source: IterableOnce[A]): SetFromMap[A] =
      source match {
        case coll: SetFromMap[A] => sfm(mf from coll.underlying)
        case coll                => newBuilder[A].addAll(coll).result()
      }

    def empty[A]: SetFromMap[A] = sfm(mf.empty[A, Unit])

    def newBuilder[A]: mutable.Builder[A, SetFromMap[A]] = new WrapperBuilder(mf.newBuilder)
  }

  private class WrapperBuilder[A](b: Builder[(A, Unit), Map[A, Unit]]) extends Builder[A, SetFromMap[A]] {
    def clear(): Unit = b.clear()

    def result(): SetFromMap[A] = sfm(b.result())

    def addOne(elem: A): this.type = { b.addOne((elem, ())); this }

    override def addAll(xs: IterableOnce[A]): this.type = {
      xs match {
        case coll: SetFromMap[A] =>
          b.addAll(coll.underlying)
          this
        case coll => super.addAll(coll)
      }
    }

    override def sizeHint(size: Int): Unit = b.sizeHint(size)
  }
}

