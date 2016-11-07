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
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only the elements that exist in each set") {
    new TestSets {
      val s12 = union(s1, s2)
      assert(contains(intersect(s1, s12), 1), "s1 in s12")
      assert(contains(intersect(s2, s12), 2), "s2 in s12")
      assert(!contains(intersect(s3, s12), 3), "s3 not in s12")
    }
  }

  test("diff contains the elements in the first set that do not exist in the second set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      assert(contains(diff(s12, s23), 1), "1 not in s23")
      assert(!contains(diff(s12, s23), 2), "2 is in s23")
      assert(contains(diff(s23, s12), 3), "3 not in s12")
      assert(!contains(diff(s23, s12), 2), "3 not in s12")
    }
  }

  test("filter returns a subset for which the predicate holds") {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      val ss12 = filter(s123, (x: Int) => x < 3)
      assert(contains(ss12, 1))
      assert(contains(ss12, 2))
      assert(!contains(ss12, 3))

      val ss3 = filter(s123, (x: Int) => x > 2)
      assert(!contains(ss3, 1))
      assert(!contains(ss3, 2))
      assert(contains(ss3, 3))
    }
  }

  test("forall returns true if a predicate holds for all elements in a set, bounded by 1000 <= x <= 1000") {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      assert(forall(s123, (x: Int) => x > 0))
      assert(!forall(s123, (x: Int) => x > 1))

      val s999 = singletonSet(999)
      val s1000 = singletonSet(1000)
      val s1001 = singletonSet(1001)
      val sOutsideBounds = union(union(s999, s1000), s1001)
      assert(forall(sOutsideBounds, (x: Int) => x < 1001))
    }
  }

  test("exists tests whether there exists a bounded integer s that satisfies p") {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      assert(exists(s123, (x: Int) => x == 1))
      assert(exists(s123, (x: Int) => x == 2))
      assert(exists(s123, (x: Int) => x == 3))
      assert(!exists(s123, (x: Int) => x == 4))

      val s999 = singletonSet(999)
      val s1000 = singletonSet(1000)
      val s1001 = singletonSet(1001)
      val sOutsideBounds = union(union(s999, s1000), s1001)
      assert(exists(sOutsideBounds, (x: Int) => x == 999))
      assert(exists(sOutsideBounds, (x: Int) => x == 1000))
      assert(!exists(sOutsideBounds, (x: Int) => x == 1001))
    }
  }

  test("map returns a new set where each element of the input set is transformed by a function") {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      assert(contains(s123, 1))
      assert(contains(s123, 2))
      assert(contains(s123, 3))
      val transformed = map(s123, (x: Int) => x * 2)
      assert(contains(transformed, 2))
      assert(contains(transformed, 4))
      assert(contains(transformed, 6))
    }
  }


}
