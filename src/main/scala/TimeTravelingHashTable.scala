import scala.annotation.tailrec
import scala.collection.mutable

object TimeTravelingHashTable {

 /* "Time Traveling Hash Table"

  Given:
    Long getTime() // monotonically unique and increasing time

  // Implement
  put(k, v)
  get(k, t) // Return the value for k as it was at time t

 */


  def main(args: Array[String]): Unit = {
    val testTable = new TimeTravelingHashTable()
     testTable.put("hello",0,"Scala")
     testTable.put("hello",5,"functional")
    testTable.put("hello",7,"programming")
    testTable.put("world",10, "is")
    testTable.put("test",12, "fun")
    testTable.put("hello",14,"world")

    println(testTable.map)
    println(testTable.get("hello", -1).getOrElse("value not found"))
    println(testTable.get("hello", 0).getOrElse("value not found"))
    println(testTable.get("hello", 1).getOrElse("value not found"))
    println(testTable.get("hello", 4).getOrElse("value not found"))
    println(testTable.get("hello", 5).getOrElse("value not found"))
    println(testTable.get("hello", 6).getOrElse("value not found"))
    println(testTable.get("hello", 10).getOrElse("value not found"))

    println(testTable.get("world", 7).getOrElse("value not found"))
    println(testTable.get("world", 10).getOrElse("value not found"))
    println(testTable.get("test", 5).getOrElse("value not found"))
    println(testTable.get("test", 15).getOrElse("value not found"))
    println(testTable.get("key", 15).getOrElse("value not found"))


  }

  class TimeTravelingHashTable {
    val map = scala.collection.mutable.HashMap.empty[String,mutable.TreeMap[Long,String]]

    def put(k: String,t:Long, v: String): Unit = {
      if(!map.contains(k))
      map.put(k,mutable.TreeMap(t -> v))
      else map.get(k).get.put(t, v)
    }

    //Tree Map is an implementation of Self balancing binary search tree where search is O(logN)
    def get(k: String,t: Long): Option[String] = {
      val treeMap = map.get(k).getOrElse(mutable.TreeMap.empty)
        if(treeMap.isEmpty) Some(k + " -> given key is not present in the table")
        else {
          val keyList = treeMap.keySet.toList
          val index = search(t,keyList)
            if(index.nonEmpty){
              treeMap.get(keyList(index.get))
            }
           else None
        }
    }

    private def search(target:Long, l:List[Long]): Option[Int] = {

      if(target < l(0)) return None
      if(target > l(l.length-1)) return Some((l.length-1))

      @tailrec
      def recursion(low:Int, high:Int):Option[Int] = (low + (high-low)/2) match{
        case _  if (high < low) => Some(high)
        case mid if l(mid) > target => recursion(low, mid-1)
        case mid if l(mid) < target => recursion(mid+1, high)
        case mid => Some(mid)
      }
      recursion(0,l.size - 1)
    }

    private def searchNearestkey(list: List[Long], target: Long): Option[Long] = {
      var left = 0
      var right = list.length-1
      if(target < list(0)) return None
      if (target > list(right)) return Some(list(right))
      while (left<=right) {
        val mid = left + (right-left)/2
        if (list(mid)==target)
          return Some(list(mid))
        else if (list(mid)>target)
          right = mid-1
        else
          left = mid+1
      }
          return Some(list(right))
    }


  }

}
