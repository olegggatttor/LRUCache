package lrutests

import org.scalatestplus.play.PlaySpec
import lrucache._

class LRUCacheTest extends PlaySpec {

  "lrucache.LRUCache" must {
    "fail if cache size is not positive" in {
      for (size <- Range(-100, 1)) {
        a[IllegalArgumentException] must be thrownBy {
          new LRUCache[Int, Int](size, identity)
        }
      }
    }

    "store last n get requests in cache" in {
      val n = 10
      val cache = new LRUCache[Int, Int](n, identity)
      for (i <- Range(0, 10 * n)) {
        cache.get(i)
      }

      cache.dump.sorted mustEqual Range(90, 100).map { x => (x, x) }.sorted

    }

    "clear cache when clear() method is called" in {
      val cache = new LRUCache[Int, Int](10, identity)
      for (i <- Range(0, 100)) {
        cache.get(i)
      }
      cache.clear()

      cache.dump mustEqual Seq()
    }

    "return value using function given as constructor param" in {
      val cache = new LRUCache[Int, Int](10, x => x + 1)
      for (i <- Range(0, 100)) {
        cache.get(i) mustEqual i + 1
      }
    }

    "move already cached values in front of cache" in {
      val cacheSize = 5
      val cache = new LRUCache[Int, Int](cacheSize, identity)
      for (i <- Range(0, 20)) {
        val getKey = i % cacheSize
        cache.get(getKey)

        cache.dump.head mustEqual (getKey, getKey)
      }
    }

    "must store no more than n elements" in {
      val n = 5
      val cache = new LRUCache[Int, Int](n, identity)
      for (i <- Range(0, 10 * n)) {
        cache.get(i)

        cache.size <= n mustEqual true
      }
    }
  }

}
