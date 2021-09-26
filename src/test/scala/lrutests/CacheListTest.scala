package lrutests

import org.scalatestplus.play.PlaySpec
import lrucache._

class CacheListTest extends PlaySpec {
  "lrucache.CacheList" must {
    "add values to back using addLast(...) method" in {
      val list = new CacheList[Int, Int]
      for (i <- Range(0, 10)) {
        list.addLast(i, i)
      }
      list.toSeq mustEqual Range(0, 10).zipWithIndex
    }

    "add values to front using addLast(...) method" in {
      val list = new CacheList[Int, Int]
      for (i <- Range(0, 10)) {
        list.addFirst(i, i)
      }
      list.toSeq mustEqual Range(0, 10).zipWithIndex.reverse
    }

    "remove values from front using removeFirst(...) method" in {
      val list = new CacheList[Int, Int]
      for (i <- Range(0, 10)) {
        list.addFirst(i, i)
      }
      for (_ <- Range(0, 5)) {
        list.removeFirst()
      }
      list.toSeq mustEqual Range(0, 5).zipWithIndex.reverse
    }

    "remove values from back using removeLast(...) method" in {
      val list = new CacheList[Int, Int]
      for (i <- Range(0, 10)) {
        list.addLast(i, i)
      }
      for (_ <- Range(0, 5)) {
        list.removeLast()
      }
      list.toSeq mustEqual Range(0, 5).zipWithIndex
    }

    "fail on remove when list is empty" in {
      val list = new CacheList[Int, Int]
      a[AssertionError] must be thrownBy {
        list.removeLast()
      }
      a[AssertionError] must be thrownBy {
        list.removeFirst()
      }
    }

    "have size N after adding N elements" in {
      val list = new CacheList[Int, Int]
      for (i <- Range(0, 10)) {
        if (i % 2 == 0) {
          list.addLast(i, i)
        } else {
          list.addFirst(i, i)
        }
      }
      list.size mustEqual 10
    }

    "must clear list when clear() called" in {
      val list = new CacheList[Int, Int]
      for (i <- Range(0, 10)) {
        if (i % 2 == 0) {
          list.addLast(i, i)
        } else {
          list.addFirst(i, i)
        }
      }
      list.clear()
      list.toSeq mustEqual Seq()
    }
  }
}
