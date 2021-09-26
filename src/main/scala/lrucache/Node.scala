package lrucache

class Node[K, V](var prev: Option[Node[K, V]],
                 val key: K,
                 val value: V ,
                 var next: Option[Node[K, V]]
                )