package lrucache

import scala.collection.mutable

/**
 * LRU Cache.
 *
 * @param capacity              - cache size
 * @param calculateValue - function for computing new values
 * @tparam K - key used to retrieve values
 * @tparam V - stored values
 */
class LRUCache[K, V](val capacity: Int, val calculateValue: K => V) {
  require(capacity > 0, "Cache size must be positive.")

  private val storage = new CacheList[K, V]()
  private val cacheMap = new mutable.HashMap[K, Node[K, V]]()

  def size: Int = {
    assert(storage.size == cacheMap.size)
    storage.size
  }

  /**
   * Retrieves value from cache or gets it from storage and updates cache.
   *
   * @param key - key to find valu
   * @return - given value for key
   */
  def get(key: K): V = {
    assert(storage.size <= capacity)
    assert(cacheMap.size <= capacity)

    val cacheValue = cacheMap.get(key)

    val result = cacheValue match {
      case Some(node) =>
        storage.moveToFront(node)
        node.value
      case None =>
        val value = calculateValue(key)
        if (storage.size == capacity) {
          val lastKey = storage.removeLast()
          cacheMap.remove(lastKey)
        }
        val node = storage.addFirst(key, value)
        cacheMap.put(key, node)
        value

    }
    assert(storage.size <= capacity)
    assert(cacheMap.size <= capacity)

    result
  }

  def dump: Seq[(K, V)] = storage.toSeq

  def clear(): Unit = {
    storage.clear()
    cacheMap.clear()
  }
}
