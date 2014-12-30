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
    assert(times(List('a', 'b', 'a', 'c', 'c', 'd'))   === List(('c',2), ('b',1), ('d',1), ('a',2)))

  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }



  test("Create Code tree") {
    val createCodeTreeVal = createCodeTree(string2Chars("abaac"))
    assert(createCodeTreeVal === Fork(Fork(Leaf('b',1),Leaf('c',1),List('b', 'c'),2),Leaf('a',3),List('b', 'c', 'a'),5))

  }

  test("decode") {
  val s1: List[Bit] = List(1)

  val createS1 = decode(Fork(Fork(Leaf('b',1),Leaf('c',1),List('b', 'c'),2),Leaf('a',3),List('b', 'c', 'a'),5), s1 )
  assert(createS1 === List('a'))

}


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode by code table and decode"){
    new TestTrees {
      assert(encode(t1)("ab".toList) === quickEncode(t1)("ab".toList))
    }
  }

}
