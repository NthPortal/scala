package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.collection.{IterableOnce, Iterator}

// junit/testOnly *LazyListLazinessTest -- -Djunit.skip.unimplemented=true
@RunWith(classOf[JUnit4])
class LazyListLazinessTest {
  private def TO_IMPLEMENT: Unit = {
    if (!sys.props.getOrElse("junit.skip.unimplemented", "").equalsIgnoreCase("true")) ???
  }

  /* laziness tests */

  /** Note: not reusable. */
  sealed abstract class LazinessChecker {
    import LazinessChecker._
    
    protected[this] final case class NamedArray(array: Array[Boolean], name: String)

    protected[this] val states = NamedArray(new Array[Boolean](count), "state")
    protected[this] val heads  = NamedArray(new Array[Boolean](count), "head")

    protected[this] def internalCheckIndex(index: Int): Unit = {
      assert(index >= 0 && index < count, "internal failure - bad index: " + index)
    }

    protected[this] def checkIndex(index: Int): Unit = {
      if (index < 0 || index >= count) throw new IndexOutOfBoundsException(index.toString)
    }
    
    private[this] def assertUnchecked(arr: NamedArray,
                                      evaluated: Boolean,
                                      index: Int,
                                      ex: ExceptionProvider): Unit = {
      if (arr.array(index) != evaluated) {
        throw ex(s"${arr.name}($index) was ${if (evaluated) "not " else ""}evaluated")
      }
    }
    
    private[this] def assertCheckedInternal(arr: NamedArray,
                                            evaluated: Boolean,
                                            index: Int,
                                            ex: ExceptionProvider): Unit = {
      internalCheckIndex(index)
      assertUnchecked(arr, evaluated, index, ex)
    }
    
    private[this] def assertChecked(arr: NamedArray,
                                    evaluated: Boolean,
                                    index: Int,
                                    ex: ExceptionProvider): Unit = {
      checkIndex(index)
      assertUnchecked(arr, evaluated, index, ex)
    }
    
    final def assertState(evaluated: Boolean, index: Int): Unit =
      assertChecked(states, evaluated, index, defaultException)
    
    final def assertHead(evaluated: Boolean, index: Int): Unit =
      assertChecked(heads, evaluated, index, defaultException)

    private[this] def assertAll(arr: NamedArray,
                                evaluated: Boolean,
                                ex: ExceptionProvider,
                                skip: Int,
                               ): Unit = {
      require(skip >= 0, "`skip` cannot be negative")
      require(skip < count, s"`skip` ($skip) >= size of lazy list ($count) - will not assert anything")
      for (i <- skip until count) assertCheckedInternal(arr, evaluated, i, ex)
    }

    /** Asserts that the evaluated status of all states matches the one specified.
      *
      * @param evaluated whether or not all states should be evaluated
      * @param ex        an exception generator (creates an `AssertionError` by default)
      */
    final def assertAllStates(evaluated: Boolean, ex: ExceptionProvider = defaultException): Unit =
      assertAll(states, evaluated, ex, 0)

    /** Asserts that the evaluated status of all heads matches the one specified.
      *
      * @param evaluated whether or not all heads should be evaluated
      * @param ex        an exception generator (creates an `AssertionError` by default)
      */
    final def assertAllHeads(evaluated: Boolean, ex: ExceptionProvider = defaultException): Unit =
      assertAll(heads, evaluated, ex, 0)

    /** Asserts that the evaluated status of all states except for the first
      * `skip` ones matches the one specified.
      *
      * @param evaluated whether or not all except the first `skip` states should be evaluated
      * @param skip      the number of states not to check
      */
    final def assertAllStatesSkipping(evaluated: Boolean, skip: Int): Unit =
      assertAll(states, evaluated, defaultException, skip)

    /** Asserts that the evaluated status of all heads except for the first
      * `skip` ones matches the one specified.
      *
      * @param evaluated whether or not all except the first `skip` heads should be evaluated
      * @param skip      the number of heads not to check
      */
    final def assertAllHeadsSkipping(evaluated: Boolean, skip: Int): Unit =
      assertAll(heads, evaluated, defaultException, skip)

    // for debugging
    final override def toString: String = {
      val sb = new java.lang.StringBuilder(getClass.getSimpleName).append("(")
      for (i <- 0 until 4) { sb.append(s"state($i): ${states.array(i)}, head($i): ${heads.array(i)}, ") }
      sb.append("...)")
      sb.toString
    }
  }

  object LazinessChecker {
    type ExceptionProvider = String => Throwable

    final val count = 16
    final val indices: Range = 0 until count

    private val defaultException: ExceptionProvider = new AssertionError(_)
  }

  /* op laziness tests */

  /** Note: not reusable. */
  final class OpLazinessChecker extends LazinessChecker {
    import LazinessChecker._

    private[this] def gen(index: Int): LazyList[Int] = {
      def elem(): Int = {
        heads.array(index) = true
        index
      }
      def state(): LazyList[Int] =
        LazyList.unfold(0) { _ => { states.array(index) = true; None } }

      internalCheckIndex(index)
      state() #::: elem() #:: LazyList.empty[Int]
    }

    private[this] def genList(): LazyList[Int] = {
      def doGen(n: Int): LazyList[Int] =
        if (n < count) gen(n) #::: doGen(n + 1)
        else LazyList.unfold(0)(_ => None)

      doGen(0)
    }

    val lazyList: LazyList[Int] = genList()
  }

  object OpLazinessChecker {
    final case class DropProfile(dropCount: Int, repeatedDrops: Boolean) {
      def apply(iteration: Int): Int =
        if (iteration <= 0) 0
        else if (repeatedDrops) dropCount * iteration
        else dropCount
    }

    val NoDrops: DropProfile = DropProfile(dropCount = 0, repeatedDrops = false)
  }

  type LazyListOp[U] = (=> LazyList[Int]) => U
  type LazyListToLazyListOp = LazyListOp[LazyList[Int]]

  // save on having to write type annotations all the time
  def lazyListOp[U](op: LazyListOp[U]): LazyListOp[U] = op

  /** Asserts that the operation does not evaluate the the initial lazy list
    * at all before methods are invoked on it.
    */
  def assertInitiallyFullyLazy[U](op: LazyListOp[U]): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    checker.assertState(evaluated = false, 0)
    checker.assertHead(evaluated = false, 0)
  }

  /** Asserts that the operation does not evaluate the initial lazy list or
    * subsequent tails at all before methods are invoked on them.
    *
    * This is a superset of the assertions performed by [[assertInitiallyFullyLazy]].
    */
  def assertRepeatedlyFullyLazy(op: LazyListToLazyListOp,
                                d: OpLazinessChecker.DropProfile = OpLazinessChecker.NoDrops): Unit = {
    val checker = new OpLazinessChecker
    val result = op(checker.lazyList)
    checker.assertState(evaluated = false, 0)
    checker.assertHead(evaluated = false, 0)
    result.head
    result.tail
    checker.assertState(evaluated = false, 1 + d(1))
    checker.assertHead(evaluated = false, 1 + d(1))
  }

  /** Asserts that the operation does not evaluate the next state of the lazy list
    * when the head is evaluated.
    */
  def assertLazyNextStateWhenHeadEvaluated(op: LazyListToLazyListOp,
                                           d: OpLazinessChecker.DropProfile = OpLazinessChecker.NoDrops): Unit = {
    val checker = new OpLazinessChecker
    val result = op(checker.lazyList)
    result.head
    checker.assertState(evaluated = false, 1 + d(1))
    result.tail.head
    checker.assertState(evaluated = false, 2 + d(2))
  }

  /** Asserts that the operation does not evaluate the head of the lazy list
    * when the next state is evaluated.
    */
  def assertLazyHeadWhenNextStateEvaluated(op: LazyListToLazyListOp,
                                           d: OpLazinessChecker.DropProfile = OpLazinessChecker.NoDrops): Unit = {
    val checker = new OpLazinessChecker
    val result = op(checker.lazyList)
    result.tail
    checker.assertHead(evaluated = false, 0 + d(1))
    result.tail.tail
    checker.assertHead(evaluated = false, 1 + d(2))
  }

  /** Asserts that the operation does not evaluate the head of the lazy list
    * when the next head is evaluated.
    */
  def assertLazyHeadWhenNextHeadEvaluated(op: LazyListToLazyListOp,
                                          d: OpLazinessChecker.DropProfile = OpLazinessChecker.NoDrops): Unit = {
    val checker = new OpLazinessChecker
    val result = op(checker.lazyList)
    result.tail.tail.head
    checker.assertHead(evaluated = false, 1 + d(2))
    result.tail.head
    checker.assertHead(evaluated = false, 0 + d(1))
  }

  /** Asserts that, though the operation may evaluate initial states and later
    * heads, it does not evaluate initial heads.
    */
  def assertLazyInitialHeads[U](op: LazyListOp[U]): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    checker.assertHead(evaluated = false, 0)
    checker.assertHead(evaluated = false, 1)
  }

  /** Asserts that, though the operation may evaluate states and initial
    * heads, it does not evaluate later heads.
    */
  def assertLazyFinalHeads[U](op: LazyListOp[U]): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    checker.assertHead(evaluated = false, LazinessChecker.count - 1)
    checker.assertHead(evaluated = false, LazinessChecker.count - 2)
  }

  /** Asserts that, though the operation may evaluate states, it does not
    * evaluate any heads.
    */
  def assertLazyAllHeads[U](op: LazyListOp[U]): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    checker.assertAllHeads(evaluated = false)
  }

  /** Asserts that the checker does not have any heads or states evaluated
    * other than the first `skip`.
    */
  def assertNotEvaluatedSkipping(checker: OpLazinessChecker, skip: Int, skipExtraState: Boolean): Unit = {
    checker.assertAllStatesSkipping(evaluated = false, skip = skip + (if (skipExtraState) 1 else 0))
    checker.assertAllHeadsSkipping(evaluated = false, skip = skip)
  }

  /** Asserts that the operation does not evaluate any heads or states
    * other than the first `skip`.
    */
  def assertLazyAllSkipping[U](op: LazyListOp[U], skip: Int, skipExtraState: Boolean = false): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    assertNotEvaluatedSkipping(checker, skip, skipExtraState)
  }

  /** Asserts that a predicate holds when a given operation is performed on
    * a lazy list that is known to be empty.
    */
  def assertKnownEmptyYields[A](op: LazyListOp[A])(predicate: A => Boolean): Unit = {
    assert(predicate(op(LazyList.empty)))
  }

  /** Asserts that operation yields a lazy list that is known to be empty
    * when performed on a lazy list that is known to be empty.
    */
  def assertKnownEmptyYieldsKnownEmpty(op: LazyListToLazyListOp): Unit =
    assertKnownEmptyYields(op)(_.knownSize == 0)

  // if this fails, all the rest will fail
  @Test
  def checker_correctness_properlyLazy(): Unit = {
    val checker = new OpLazinessChecker
    val illegalState = (s: String) => new IllegalStateException("sanity check failed: " + s)

    // check that none start evaluated
    checker.assertAllStates(evaluated = false, illegalState)
    checker.assertAllHeads(evaluated = false, illegalState)

    // check that it detects state evaluation
    checker.lazyList.isEmpty
    checker.assertState(evaluated = true, 0)
    checker.assertHead(evaluated = false, 0)
    checker.lazyList.tail.isEmpty
    checker.assertHead(evaluated = false, 0)
    checker.assertState(evaluated = true, 1)
    checker.assertHead(evaluated = false, 1)

    // check that all are evaluated after forcing
    checker.lazyList.force

    checker.assertAllStates(evaluated = true, illegalState)
    checker.assertAllHeads(evaluated = true, illegalState)

    // check unmodified checker is properly lazy
    val op = lazyListOp(x => x)
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertLazyHeadWhenNextStateEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
    assertLazyAllHeads(op)
    assertLazyAllSkipping(op, 0)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def isEmpty_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.isEmpty, 0, skipExtraState = true)
  }

  @Test
  def nonEmpty_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.nonEmpty, 0, skipExtraState = true)
  }

  @Test
  def filter_properlyLazy(): Unit = {
    val ops: Map[LazyListToLazyListOp, OpLazinessChecker.DropProfile] = Map(
      lazyListOp(_.filter(_ => true))  -> OpLazinessChecker.NoDrops,
      lazyListOp(_.filter(_ % 2 != 0)) -> OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true),
    )

    for ((op, d) <- ops) {
      assertRepeatedlyFullyLazy(op, d)
      assertLazyNextStateWhenHeadEvaluated(op, d)
      assertKnownEmptyYieldsKnownEmpty(op)
    }
  }

  @Test
  def filterNot_properlyLazy(): Unit = {
    val ops: Map[LazyListToLazyListOp, OpLazinessChecker.DropProfile] = Map(
      lazyListOp(_.filterNot(_ => false)) -> OpLazinessChecker.NoDrops,
      lazyListOp(_.filterNot(_ % 2 == 0)) -> OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true),
    )

    for ((op, d) <- ops) {
      assertRepeatedlyFullyLazy(op, d)
      assertLazyNextStateWhenHeadEvaluated(op, d)
      assertKnownEmptyYieldsKnownEmpty(op)
    }
  }

  @Test
  def withFilter_properlyLazy(): Unit = {
    assertInitiallyFullyLazy(_.withFilter(_ => true))
    assertInitiallyFullyLazy(_.withFilter(_ % 2 != 0))
  }

  @Test
  def partition_properlyLazy(): Unit = {
    val partition = lazyListOp(_.partition(_ % 2 == 0))
    val op1 = partition.andThen(_._1)
    val op2 = partition.andThen(_._2)
    val d = OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true)
    for (op <- op1 :: op2 :: Nil) {
      assertRepeatedlyFullyLazy(op, d)
      assertLazyNextStateWhenHeadEvaluated(op, d)
      assertKnownEmptyYieldsKnownEmpty(op)
    }
  }

  @Test
  def partitionWith_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def map_properlyLazy(): Unit = {
    val op = lazyListOp(_.map(_ + 1))
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertLazyHeadWhenNextStateEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def collect_properlyLazy(): Unit = {
    val op = lazyListOp(_ collect { case i if i % 2 != 0 => i })
    val d = OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyNextStateWhenHeadEvaluated(op, d)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def collectFirst_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ collectFirst { case i if i % 2 == 0 => i }, 1)
    assertLazyAllSkipping(_ collectFirst { case i if i % 2 != 0 => i }, 2)
  }

  @Test // scala/scala#6960
  def withFilter_withFilter_properlyLazy(): Unit = {
    assertInitiallyFullyLazy(_.withFilter(_ => true).withFilter(_ => true))
  }

  @Test // scala/bug#9134
  def filter_map_properlyLazy(): Unit = {
    val op = lazyListOp(_.filter(_ % 2 != 0).map(identity))
    val d = OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyNextStateWhenHeadEvaluated(op, d)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test // scala/bug#9134
  def withFilter_map_properlyLazy(): Unit = {
    val op = lazyListOp(_.withFilter(_ % 2 != 0).map(identity))
    val d = OpLazinessChecker.DropProfile(dropCount = 1, repeatedDrops = true)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyNextStateWhenHeadEvaluated(op, d)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  private def genericFlatMap_properlyLazy(flatMapOp: (LazyList[Int], Int => IterableOnce[Int]) => LazyList[Int]): Unit = {
    val op = lazyListOp(flatMapOp(_, _ :: Nil))
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertKnownEmptyYieldsKnownEmpty(op)

    // Check that calling the `flatMap` operation on instances of `LazyList` leaves them with lazy heads
    val checkers = (1 to 4) map { _ => new OpLazinessChecker }
    flatMapOp(LazyList.from(0).take(checkers.length), i => checkers(i).lazyList)
    for (checker <- checkers) checker.assertAllHeads(evaluated = false)
  }

  @Test
  def flatMap_properlyLazy(): Unit = {
    genericFlatMap_properlyLazy(_ flatMap _)
  }

  @Test
  def flatten_properlyLazy(): Unit = {
    genericFlatMap_properlyLazy(_.map(_).flatten)
  }

  @Test
  def scanLeft_properlyLazy(): Unit = {
    val op = lazyListOp(_.scanLeft(0)(_ + _))
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertLazyHeadWhenNextStateEvaluated(op)
  }

  @Test
  def scanRight_properlyLazy(): Unit = {
    val op = lazyListOp(_.scanRight(0)(_ + _))
    assertLazyHeadWhenNextStateEvaluated(op)
  }

  private def genericAppendedColl_properlyLazy(append: (=> LazyList[Int], => IterableOnce[Int]) => LazyList[Int]): Unit = {
    def check(suffix: => IterableOnce[Int]): Unit = {
      val op = lazyListOp(append(_, suffix))
      assertRepeatedlyFullyLazy(op)
      assertLazyNextStateWhenHeadEvaluated(op)
      assertLazyHeadWhenNextStateEvaluated(op)
      assertLazyHeadWhenNextHeadEvaluated(op)
    }

    for (coll <- Seq(Nil, 2 :: Nil, 1 to 10)) check(coll)

    // Check that appending a `LazyList` leaves it fully lazy
    val checker = new OpLazinessChecker
    val ll = append(LazyList.from(0).take(4), checker.lazyList)
    checker.assertAllStates(evaluated = false)
    checker.assertAllHeads(evaluated = false)
    ll.tail.tail.tail.tail // should be the appended LazyList
    checker.assertAllStates(evaluated = false)
    checker.assertAllHeads(evaluated = false)
    ll.length // evaluate states
    checker.assertAllHeads(evaluated = false)
  }

  @Test
  def lazyAppendedAll_appendedAll_properlyLazy(): Unit = {
    genericAppendedColl_properlyLazy(_ lazyAppendedAll _)
  }

  @Test
  def appendedAll_properlyLazy(): Unit = {
    genericAppendedColl_properlyLazy(_ appendedAll _)
    genericAppendedColl_properlyLazy(_ :++ _)

    val ll = LazyList.from(1)
    assertKnownEmptyYields(_ appendedAll ll)(_ eq ll)
  }

  @Test
  def concat_properlyLazy(): Unit = {
    genericAppendedColl_properlyLazy(_ concat _)
    genericAppendedColl_properlyLazy(_ ++ _)
  }

  @Test
  def appended_properlyLazy(): Unit = {
    val op = lazyListOp(_ appended -1)
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertLazyHeadWhenNextStateEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
  }

  @Test
  def prepended_properlyLazy(): Unit = {
    val op = lazyListOp { ll =>
      val prepended = ll.prepended(-1)
      prepended.head
      prepended.tail
    }
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertLazyHeadWhenNextStateEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
  }

  @Test
  def prependedAll_properlyLazy(): Unit = {
    def check(prefix: IterableOnce[Int], drop: Int): Unit = {
      val op = lazyListOp { ll =>
        var prepended = ll prependedAll prefix
        var toDrop = drop
        while (toDrop > 0) {
          prepended.head
          prepended = prepended.tail
          toDrop -= 1
        }
        prepended
      }
      assertRepeatedlyFullyLazy(op)
      assertLazyNextStateWhenHeadEvaluated(op)
      assertLazyHeadWhenNextStateEvaluated(op)
      assertLazyHeadWhenNextHeadEvaluated(op)
    }
    for (coll <- Seq(Nil, 2 :: Nil, 1 to 10)) check(coll, coll.length)

    // Check that prepending a `LazyList` leaves it fully lazy
    val checker = new OpLazinessChecker
    val ll = LazyList.from(0)
      .take(4)
      .prependedAll(checker.lazyList)
    checker.assertAllStates(evaluated = false)
    checker.assertAllHeads(evaluated = false)
    ll.length // evaluate states
    checker.assertAllHeads(evaluated = false)
  }

  @Test
  def drop_properlyLazy(): Unit = {
    val op = lazyListOp(_.drop(2))
    val d = OpLazinessChecker.DropProfile(dropCount = 2, repeatedDrops = false)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyNextStateWhenHeadEvaluated(op, d)
    assertLazyHeadWhenNextStateEvaluated(op, d)
    assertLazyHeadWhenNextHeadEvaluated(op, d)
    assertLazyInitialHeads(op.andThen(_.force))
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def dropWhile_properlyLazy(): Unit = {
    val op = lazyListOp(_.dropWhile(_ < 2))
    val d = OpLazinessChecker.DropProfile(dropCount = 2, repeatedDrops = false)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyNextStateWhenHeadEvaluated(op, d)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def dropRight_properlyLazy(): Unit = {
    val op = lazyListOp(_.dropRight(2))
    assertInitiallyFullyLazy(op)
    assertLazyHeadWhenNextStateEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
    assertLazyFinalHeads(op.andThen(_.force))
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def take_properlyLazy(): Unit = {
    val op = lazyListOp(_.take(4))
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertLazyHeadWhenNextStateEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
    assertLazyFinalHeads(op.andThen(_.force))
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def takeWhile_properlyLazy(): Unit = {
    val op = lazyListOp(_.takeWhile(_ < 4))
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertLazyFinalHeads(op.andThen(_.force))
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def takeRight_properlyLazy(): Unit = {
    val op = lazyListOp(_.takeRight(4))
    assertInitiallyFullyLazy(op)
    assertLazyHeadWhenNextStateEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
    assertLazyInitialHeads(op.andThen(_.force))
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def slice_properlyLazy(): Unit = {
    val op = lazyListOp(_.slice(2, LazinessChecker.count - 2))
    val opThenForce = op.andThen(_.force)
    val d = OpLazinessChecker.DropProfile(dropCount = 2, repeatedDrops = false)
    assertRepeatedlyFullyLazy(op, d)
    assertLazyNextStateWhenHeadEvaluated(op, d)
    assertLazyHeadWhenNextStateEvaluated(op, d)
    assertLazyHeadWhenNextHeadEvaluated(op, d)
    assertLazyInitialHeads(opThenForce)
    assertLazyFinalHeads(opThenForce)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def splitAt_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test // scala/bug#11089
  def last_properlyLazy(): Unit = {
    assertLazyInitialHeads(_.last)
  }

  @Test
  def lastOption_properlyLazy(): Unit = {
    assertLazyInitialHeads(_.lastOption)
  }

  @Test
  def length_properlyLazy(): Unit = {
    assertLazyAllHeads(_.length)
  }

  @Test
  def lengthCompare_properlyLazy(): Unit = {
    assertLazyAllHeads(_.lengthCompare(LazinessChecker.count))
    TO_IMPLEMENT
  }

  @Test
  def sizeCompare_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def reverse_properlyLazy(): Unit = {
    assertLazyAllHeads(_.reverse)
    TO_IMPLEMENT
  }

  @Test
  def reverseIterator_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def reverseMap_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def toString_properlyLazy(): Unit = {
    val op = lazyListOp(_.toString())
    assertInitiallyFullyLazy(op)
    assertLazyAllHeads(op)
  }

  @Test
  def contains_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ contains 0, 1)
    assertLazyAllSkipping(_ contains 3, 4)
  }

  @Test
  def containsSlice_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ containsSlice (0 to 2), 3)
    assertLazyAllSkipping(_ containsSlice (3 to 7), 8)

    // check laziness of slice when it is a `LazyList`
    val checker = new OpLazinessChecker
    assert(!LazyList.from(3).take(LazinessChecker.count * 2).containsSlice(checker.lazyList))
    assertNotEvaluatedSkipping(checker, 1, skipExtraState = false)
  }

  @Test
  def corresponds_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.corresponds(Iterator.empty[Int])(_ == _), 1)
    assertLazyAllSkipping(_.corresponds(LazyList.empty[Int])(_ == _), 1)

    assertLazyAllSkipping(_.corresponds(Iterator.from(1))(_ == _), 1)
    assertLazyAllSkipping(_.corresponds(LazyList.from(1))(_ == _), 1)

    assertLazyAllSkipping(_.corresponds(Iterator.from(0).take(1))(_ == _), 2)
    assertLazyAllSkipping(_.corresponds(LazyList.from(0).take(1))(_ == _), 2)

    // check laziness of corresponding `LazyList`
    def check(lazyList: LazyList[Int], withChecker: OpLazinessChecker => Unit): Unit = {
      val checker = new OpLazinessChecker
      assert(!lazyList.corresponds(checker.lazyList)(_ == _))
      withChecker(checker)
    }
    check(LazyList.from(1), assertNotEvaluatedSkipping(_, 1, skipExtraState = false))
    check(LazyList.empty, assertNotEvaluatedSkipping(_, 0, skipExtraState = true))
  }

  @Test
  def diff_properlyLazy(): Unit = {
    val op = lazyListOp(_.diff(Nil))
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def distinct_properlyLazy(): Unit = {
    val op = lazyListOp(_.distinct)
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def distinctBy_properlyLazy(): Unit = {
    val op = lazyListOp(_.distinctBy(identity))
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def startsWith_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def endsWith_properlyLazy(): Unit = {
    assertLazyInitialHeads(_.endsWith(1 to 10))
  }

  @Test
  def exists_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ exists { _ < 2 }, 1)
    assertLazyAllSkipping(_ exists { _ > 2 }, 4)
  }

  @Test
  def find_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ find { _ < 2 }, 1)
    assertLazyAllSkipping(_ find { _ > 2 }, 4)
  }

  @Test
  def forall_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ forall { _ > 2 }, 1)
    assertLazyAllSkipping(_ forall { _ < 2 }, 3)
  }

  private def genericSliding_properlyLazy(op: LazyListOp[Iterator[LazyList[Int]]]): Unit = {
    assertInitiallyFullyLazy(op)
    assertLazyInitialHeads(op andThen { _ drop 2 foreach { _.force } })
    assertLazyAllHeads(op andThen { _ foreach { _.length } })
    assertLazyAllSkipping(op andThen { _.hasNext }, 0, skipExtraState = true)
    assertLazyAllSkipping(op andThen { _.next().force }, 3)
    assertKnownEmptyYields(op)(_ eq Iterator.empty)
  }

  @Test
  def grouped_properlyLazy(): Unit = {
    genericSliding_properlyLazy(_ grouped 3)
  }

  @Test
  def sliding_properlyLazy(): Unit = {
    genericSliding_properlyLazy(_ sliding 3)
    genericSliding_properlyLazy(_.sliding(3, 2))
  }

  @Test
  def indexOf_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.indexOf(0), 1)
    assertLazyAllSkipping(_.indexOf(2), 3)
    val op = lazyListOp(_.indexOf(6, 5))
    assertLazyAllSkipping(op, 7)
    assertLazyInitialHeads(op)
  }

  @Test
  def indexOfSlice_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.indexOfSlice(0 to 5), 6)
    assertLazyAllSkipping(_.indexOfSlice(1 to 3), 4)
    val op = lazyListOp(_.indexOfSlice(6 to 9, 5))
    assertLazyAllSkipping(op, 10)
    assertLazyInitialHeads(op)

    // check laziness of slice when it is a `LazyList`
    val checker = new OpLazinessChecker
    assertEquals(-1, LazyList.from(3).take(LazinessChecker.count * 2).indexOfSlice(checker.lazyList))
    assertNotEvaluatedSkipping(checker, 1, skipExtraState = false)
  }

  @Test
  def indexWhere_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.indexWhere(_ < 2), 1)
    assertLazyAllSkipping(_.indexWhere(_ > 2), 4)
    val op  = lazyListOp(_.indexWhere(_ > 2, 4))
    assertLazyAllSkipping(op, 5)
    assertLazyInitialHeads(op)
  }

  @Test
  def lastIndexOf_properlyLazy(): Unit = {
    import LazinessChecker.count
    assertLazyInitialHeads(_.lastIndexOf(count - 1))
    assertLazyInitialHeads(_.lastIndexOf(count - 3))
    val op = lazyListOp(_.lastIndexOf(count - 7, count - 5))
    assertLazyInitialHeads(op)
    assertLazyFinalHeads(op)
  }

  @Test
  def lastIndexOfSlice_properlyLazy(): Unit = {
    import LazinessChecker.count
    assertLazyInitialHeads(_.lastIndexOfSlice((count - 6) until count))
    assertLazyInitialHeads(_.lastIndexOfSlice((count - 4) to (count - 2)))
    assertLazyInitialHeads(_.lastIndexOfSlice((count - 10) to (count - 7), count - 5))

    // check laziness of slice when it is a `LazyList`
    val checker = new OpLazinessChecker
    assertEquals(-1, LazyList.from(3).take(LazinessChecker.count * 2).lastIndexOfSlice(checker.lazyList))
    checker.assertHead(evaluated = false, 0)
    checker.assertHead(evaluated = false, 1)
  }

  @Test
  def lastIndexWhere_properlyLazy(): Unit = {
    import LazinessChecker.count
    assertLazyInitialHeads(_.lastIndexWhere(_ > (count - 3)))
    assertLazyInitialHeads(_.lastIndexWhere(_ < (count - 3)))
    val op = lazyListOp(_.lastIndexWhere(_ < (count - 7), count - 5))
    assertLazyInitialHeads(op)
    assertLazyFinalHeads(op)
  }

  @Test
  def indices_properlyLazy(): Unit = {
    assertLazyAllHeads(_.indices)
  }

  @Test
  def init_properlyLazy(): Unit = {
    val op = lazyListOp(_.init)
    assertLazyHeadWhenNextStateEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
    assertLazyFinalHeads(op.andThen(_.init.force))
  }

  @Test
  def inits_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def tails_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def intersect_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def iterator_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def knownSize_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def max_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def maxBy_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def maxOption_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def maxByOption_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def min_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def minBy_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def minOption_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def minByOption_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def padTo_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def patch_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def permutations_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def sameElements_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def search_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def segmentLength_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def span_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def to_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def transpose_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def union_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def unzip_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def unzip3_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def updated_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def view_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def zip_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def lazyZip_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def zipAll_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  @Test
  def zipWithIndex_properlyLazy(): Unit = {
    TO_IMPLEMENT
  }

  /* factory laziness tests */

  /** Note: not reusable.
    *
    * racy, but not being used in a concurrent environment
    */
  final class FactoryLazinessChecker extends LazinessChecker {
    private[this] var ll: LazyList[Int] = _

    final class Initializer private[FactoryLazinessChecker] {
      private[this] def evaluate(arr: NamedArray, index: Int): Unit = {
        checkIndex(index)
        if (arr.array(index)) throw new IllegalStateException(s"Can only evaluate ${arr.name}($index) once")
        arr.array(index) = true
      }

      /** Marks state evaluated for a given index. */
      def evaluateState(index: Int): Unit = evaluate(states, index)

      /** Marks head evaluated for a given index. */
      def evaluateHead(index: Int): Unit = evaluate(heads, index)

      /** Marks state and head evaluated for a given index. */
      def evaluateIndex(index: Int): Unit = {
        evaluateState(index)
        evaluateHead(index)
      }
    }

    def initialize(init: Initializer => LazyList[Int]): this.type = {
      if (ll ne null) throw new IllegalStateException("already initialized")
      val res = init(new Initializer)
      if (res eq null) throw new NullPointerException("null LazyList")
      ll = res
      this
    }

    def lazyList: LazyList[Int] = {
      if (ll eq null) throw new IllegalStateException("not initialized")
      ll
    }
  }

  object FactoryLazinessChecker {
    type Factory = FactoryLazinessChecker#Initializer => LazyList[Int]
  }

  def lazyListFactory(factory: FactoryLazinessChecker.Factory): FactoryLazinessChecker.Factory = factory

  def assertLazyTailWhenHeadEvaluated(factory: FactoryLazinessChecker.Factory): Unit = {
    val checker = new FactoryLazinessChecker().initialize(factory)
    checker.lazyList.head
    checker.assertState(evaluated = false, 1)
    checker.lazyList.tail.head
    checker.assertState(evaluated = false, 2)
  }

  def assertLazyHeadWhenTailEvaluated(factory: FactoryLazinessChecker.Factory): Unit = {
    val checker = new FactoryLazinessChecker().initialize(factory)
    checker.lazyList.take(LazinessChecker.count).length // evaluate all tails
    checker.assertAllHeads(evaluated = false)
  }

  def assertLazyHeadWhenNextHeadEvaluated(factory: FactoryLazinessChecker.Factory): Unit = {
    val checker = new FactoryLazinessChecker().initialize(factory)
    checker.lazyList.tail.tail.head
    checker.assertHead(evaluated = false, 1)
    checker.lazyList.tail.head
    checker.assertHead(evaluated = false, 0)
  }

  @Test
  def fromIterator_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.from(Iterator.tabulate(LazinessChecker.count) { i =>
        init.evaluateIndex(i)
        i
      })
    }
    assertLazyTailWhenHeadEvaluated(factory)
  }

  @Test
  def fromLazyList_properlyLazy(): Unit = {
    val op = lazyListOp(ll => LazyList.from(ll))
    assertRepeatedlyFullyLazy(op)
    assertLazyNextStateWhenHeadEvaluated(op)
    assertLazyHeadWhenNextStateEvaluated(op)
    assertLazyHeadWhenNextHeadEvaluated(op)
  }

  @Test
  def unfold_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.unfold(0) { i =>
        if (i >= LazinessChecker.count) None
        else {
          init.evaluateIndex(i)
          Some(i, i + 1)
        }
      }
    }
    assertLazyTailWhenHeadEvaluated(factory)
  }

  @Test
  def iterate_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.iterate(0) { i => init.evaluateIndex(i); i + 1 }
    }
    assertLazyTailWhenHeadEvaluated(factory)
  }

  @Test
  def iterateLen_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.iterate(0, 10) { i => init.evaluateIndex(i); i + 1 }
    }
    assertLazyTailWhenHeadEvaluated(factory)
  }

  @Test
  def tabulate_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.tabulate(LazinessChecker.count) { i => init.evaluateIndex(i); i }
    }
    assertLazyHeadWhenTailEvaluated(factory)
    assertLazyHeadWhenNextHeadEvaluated(factory)
  }

  @Test
  def `#:: and #::: properlyLazy`(): Unit = {
    val factory = lazyListFactory { init =>
      def gen(index: Int): LazyList[Int] = {
        def state(): LazyList[Int] =
          LazyList.unfold(0) { _ => init.evaluateState(index); None }
        def elem(): Int = {
          init.evaluateHead(index)
          index
        }
        if (index >= LazinessChecker.count) LazyList.empty
        else state() #::: elem() #:: gen(index + 1)
      }

      gen(0)
    }
    assertLazyTailWhenHeadEvaluated(factory)
    assertLazyHeadWhenTailEvaluated(factory)
    assertLazyHeadWhenNextHeadEvaluated(factory)
  }

  @Test
  def fill_properlyLazy(): Unit = {
    var counter = 0
    val lazyList = LazyList.fill(10) { counter += 1; counter }
    lazyList.length
    assertEquals(0, counter)
    assertEquals(1, lazyList(4))
    assertEquals(1, counter)
    assertEquals(2, lazyList.head)
    assertEquals(2, counter)
  }

  @Test
  def continually_properlyLazy(): Unit = {
    var counter = 0
    val lazyList = LazyList continually { counter += 1; counter }
    lazyList.lengthCompare(10) // evaluate first 10 states
    assertEquals(0, counter)
    assertEquals(1, lazyList(4))
    assertEquals(1, counter)
    assertEquals(2, lazyList.head)
    assertEquals(2, counter)
  }
}
