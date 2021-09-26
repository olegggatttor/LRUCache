package lrucache

import scala.collection.mutable.ListBuffer

class CacheList[K, V] {
  private var head: Option[Node[K, V]] = None
  private var tail: Option[Node[K, V]] = None
  private var _size: Int = 0

  def peekFirst: Option[Node[K, V]] = head

  def peekLast: Option[Node[K, V]] = tail

  def size: Int = _size

  def toSeq: Seq[(K, V)] = {
    val buffer = new ListBuffer[(K, V)]
    var cur = head
    while (cur.isDefined) {
      val node = cur.get
      buffer += Tuple2(node.key, node.value)
      cur = node.next
    }
    buffer.toSeq
  }

  def addFirst(key: K, value: V): Node[K, V] = {
    val newNode = Some(new Node(None, key, value, head))
    switchHead(newNode)
    _size += 1
    assert(head == newNode)
    newNode.get
  }

  def addLast(key: K, value: V): Node[K, V] = {
    val newNode = Some(new Node(tail, key, value, None))
    tail match {
      case Some(node) => node.next = newNode
      case None => head = newNode
    }
    tail = newNode
    _size += 1
    assert(tail == newNode)
    newNode.get
  }

  def removeFirst(): K = {
    assert(head.isDefined)

    val toRemove = head.get
    toRemove.next.foreach(next =>
      next.prev = None
    )
    head = toRemove.next
    assert(!head.contains(toRemove))
    _size -= 1
    toRemove.key
  }

  def removeLast(): K = {
    assert(tail.isDefined)

    val toRemove = tail.get
    toRemove.prev.foreach(prev =>
      prev.next = None
    )
    tail = toRemove.prev
    assert(!tail.contains(toRemove))
    _size -= 1
    toRemove.key
  }

  def moveToFront(toMove: Node[K, V]): Unit = {
    val prev = toMove.prev
    val next = toMove.next
    prev.foreach(node => node.next = next)
    next.foreach(node => node.prev = prev)
    switchHead(Some(toMove))
    assert(head.contains(toMove))
  }

  def clear(): Unit = {
    head = None
    tail = None
    _size = 0
  }

  private def switchHead(newHead: Option[Node[K, V]]): Unit = {
    head match {
      case Some(node) => node.prev = newHead
      case None => tail = newHead
    }
    head = newHead
  }
}