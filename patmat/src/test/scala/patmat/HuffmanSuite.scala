package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    new TestTrees {
      val counts = times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
      assert(counts.length == 9, counts.length)
      counts match {
        case List(('l', x)) => assert(x == 3)
        case List((',', x)) => assert(x == 1)
        case _ =>
      }
    }
  }

  test("makeOrderedLeafList for one element") {
    assert(makeOrderedLeafList(List(('t', 2))) === List(Leaf('t', 2)))
  }

  test("makeOrderedLeafList for two elements already in order") {
    assert(makeOrderedLeafList(List(('t', 2), ('a', 3))) === List(Leaf('t', 2), Leaf('a', 3)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of a singleton") {
    val ts = List(Leaf('a', 1))
    assert(combine(ts) == ts)
  }

  test("combine of a nil") {
    assert(combine(Nil) == Nil)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("encode a 2-character list") {
    val twoList = makeCodeTree(Leaf('h', 1), Leaf('i', 1))
    assert(encode(twoList)("hi".toList) === List(1,0))
  }

  test("decode a 2-character list") {
    val twoList = makeCodeTree(Leaf('h', 1), Leaf('i', 1))
    assert (decode(twoList, List(0,1)) === "hi".toList)
  }

  test("decode the s3cr3t message") {
    assert(decodedSecret === "hello, world".toList)
  }

  test("encode some french") {
    assert(encode(frenchCode)("oui".toList) === List(1,0))
  }

  test ("encode abd") {
    new TestTrees {
      assert(encode(t2)("abd".toList) === List(1,0,1,1,0))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("ab".toList)) === "ab".toList)
    }
  }
}
