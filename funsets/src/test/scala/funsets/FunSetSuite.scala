package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = union(s1, s2)
    val s6 = singletonSet(6)
    def fiveEvens: Set = x => x == 2 || x == 4 || x == 6 || x == 8 || x == 10
    def fiveOdds: Set = x => x == 1 || x == 3 || x == 5 || x == 7 || x == 9
    def fivePrimes:  Set = x => x == 1 || x == 2 || x == 3 || x == 5 || x == 7
    def oneToFour: Set = x => x == 1 || x == 2 || x == 3 || x == 4
    def primesAndThousand: Set = x => x == 1 || x == 3 || x == 4 || x == 5 || x == 7 || x == 1000
    def even: Set = x => x % 2 == 0
    def odd: Set = x => x % 2 != 0
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection only contains the elements in both sets") {
    new TestSets {
      val s = intersect(s1, s4)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
      val s0 = intersect(s1,s3)
      assert(!contains(s0, 1), "Intersect: empty set 1")
      assert(!contains(s0, 2), "Intersect: empty set 2")
      assert(!contains(s0, 3), "Intersect: empty set 3")
    }
  }

  test("diff returns elements in 's' that are not in 't'") {
    new TestSets {
      val s = diff(s3, s4)
      assert(contains(s, 3), "diff 3")
      assert(!contains(s, 1), "diff 1")
      assert(!contains(s, 2), "diff 2")
      assert(!contains(s, 0), "diff should never have 0")
      val sym = diff(s4, s3)
      assert(contains(sym, 1), "not symmetric 1")
      assert(contains(sym, 2), "not symmetric 2")
      assert(!contains(sym, 3), "not symmetric 3")
      assert(!contains(sym, 0), "diff should never have 0")

      val empty = diff(s1, s1)
      assert(!contains(empty, 0), "diff of a set with itself should be empty")
      assert(!contains(empty, 1), "diff of a set with itself should be empty")
    }
  }

  test("filter does the same thing as intersect, pretty much") {
    new TestSets {
      val s = filter(s1, s4)
      assert(contains(s, 1), "filter 1")
      assert(!contains(s, 2), "filter 2")
      assert(!contains(s, 3), "filter 3")
      val s0 = filter(s1,s3)
      assert(!contains(s0, 1), "filter: empty set 1")
      assert(!contains(s0, 2), "filter: empty set 2")
      assert(!contains(s0, 3), "filter: empty set 3")
    }
  }

  test("forall tests whether a given predicate is true for all elements of the set") {
    new TestSets {
      val s1s2 = forall(s1, s2)
      assert(!s1s2, "forall(s1, s2) should be false")
      val sN = forall(s1, s1)
      assert(sN, "forall(s1, s1) should be true")
      val sX = forall(s4, s1)
      assert(!sX, "forall(s4, s1) should be false")
      assert(!forall(fivePrimes, fiveEvens), "primes are not all even")
      assert(!forall(fivePrimes, fiveOdds), "primes are not all odd")
    }
  }

  test("exists determines if there exists a bounded integer within `s` that satisfies `p`.") {
    new TestSets {
       assert(exists(fiveEvens, s6), "6 should be in fiveEvens")
       assert(!exists(fiveOdds, s6), "No even numbers in the odd set")
       assert(exists(oneToFour, s2), "2 should be in the set 1,2,3,4")
    }
  }

  test("map transforms a given set into another one by applying to each of its elements the given function") {
    new TestSets {
      val doubled = map(fiveEvens, x => x * 2)
      val thousand = map(primesAndThousand, x => x - 1)
    }
  }

  test("exists and filter") {
    new TestSets {
      val evenAndThree = union(even, s3)
      assert(exists(evenAndThree, odd), "all even numbers and 3 should contain an odd number")
      assert(contains(filter(evenAndThree, _ % 2 != 0), 3))
    }
  }
}
