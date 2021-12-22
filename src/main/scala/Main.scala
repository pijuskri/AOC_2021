import java.util.stream.Collectors
import collection.convert.ImplicitConversions
import scala.{+:, ::}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}
implicit class MapFunctions[A, B](val map: Map[A, B]) extends AnyVal {
  def mapKeys[A1](f: A => A1): Map[A1, B] = map.map({ case (a, b) => (f(a), b) })
}

object Main extends App {
  println(day10_2())
}
def day1_1(): Int = {
  val bufferedSource = scala.io.Source.fromFile("src/main/data/input_1.txt")

  val ret = bufferedSource.getLines().toList.map(x=>Some(x.toInt).value)
  .foldLeft(0,-1)((acc: (Int, Int), x: Int) => {
    if (acc._2 == -1) (0,x)
    else (if(x > acc._2) acc._1 + 1 else acc._1, x)
  })._1

  bufferedSource.close()

  ret
}
def window_1(x:List[Int]) : List[Int] = x match {
  case x1 :: x2 :: x3 :: t => x1+x2+x3 :: window_1(x2::x3::t)
  case _ => List()
}
def day1_2(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_1.txt")
  val ret = window_1(bufferedSource.getLines().toList.map(x=>Some(x.toInt).value))
    .foldLeft(0,-1)((acc: (Int, Int), x: Int) => {
    if (acc._2 == -1) (0,x)
    else (if(x > acc._2) acc._1 + 1 else acc._1, x)
  })._1

  bufferedSource.close()

  ret
}
def day2_1(): Int = {
  val bufferedSource = scala.io.Source.fromFile("src/main/data/input_2.txt")

  val ret = bufferedSource.getLines().toList
    .map(x=>(x.split(" ")(0), Some(x.split(" ")(1).toInt).value))
    .foldLeft((0,0))((acc, x) => x._1 match {
      case "forward" => (acc._1+x._2, acc._2)
      case "up" => (acc._1, acc._2-x._2)
      case "down" => (acc._1, acc._2+x._2)
    })

  bufferedSource.close()
  ret._1 * ret._2
}
def day2_2(): Int = {
  val bufferedSource = scala.io.Source.fromFile("src/main/data/input_2.txt")

  val ret = bufferedSource.getLines().toList
    .map(x=>(x.split(" ")(0), Some(x.split(" ")(1).toInt).value))
    .foldLeft((0,0,0))((acc, x) => x._1 match {
      case "forward" => (acc._1+x._2, acc._2 + acc._3 * x._2, acc._3)
      case "up" => (acc._1, acc._2, acc._3-x._2)
      case "down" => (acc._1, acc._2, acc._3+x._2)
    })

  bufferedSource.close()
  ret._1 * ret._2
}
def day3_1(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_3.txt")

  val txt = bufferedSource.getLines().toList.flatten
  val n = 12
  val ret = List.range(0,n).map(i=>txt.drop(i).grouped(n).map(_.head).toList).map(line=>{
    val count: Map[Int, Int] = line.toList.groupBy(x=>x).map((k:Char,v:List[Char]) => (Integer.parseInt(k.toString), v.length));
    if(count.getOrElse(0, 0) > count.getOrElse(1,0)) 0 else 1
  })
  bufferedSource.close()
  Integer.parseInt(ret.mkString, 2) * Integer.parseInt(ret.map(x=> if(x==0) 1 else 0).mkString, 2)
}
@tailrec
def recursive_3(depth:Int, lines: List[String], inverse: Boolean): Int = lines match {
  case x :: Nil => Integer.parseInt(x, 2)
  case _ => {
    val count: Map[Char, Int] = lines.map(line => line.charAt(depth)).groupBy(x=>x).map((k,v) => k -> v.length)
    var filt: Char = if(count.getOrElse('0',0) > count.getOrElse('1',0)) '0' else '1'
    filt = if(inverse) (if(filt == '0') '1' else '0') else filt
    recursive_3(depth + 1, lines.filter(line => filt == line.charAt(depth)), inverse)
  }
}
def day3_2(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_3.txt")
  val txt = bufferedSource.getLines().toList
  bufferedSource.close()
  recursive_3(0, txt,false) * recursive_3(0, txt,true)
}
//convert numbers into key to row/collum, store results in lines, when number matches add to row and collum. if collum is equal to n, board wins


def recursive_4(board: Int, lines: List[String]): Map[Int,List[(Int, Int, Int)]] = lines match {
  case empty :: l1 :: l2:: l3 :: l4 :: l5 :: rest => {
    val mapped = List((l1,0),(l2,1),(l3,2),(l4,3),(l5,4)).flatMap(line =>
      line._1.split(" ").filter(x=>x != "").map(num => Integer.parseInt(num))
        .zip(0 until 5).map((n, col)=> n -> List((board, line._2, col)))

    ).toMap
    mapped ++ recursive_4(board+1, rest).map{ case (k,v) => k -> (v ++ mapped.getOrElse(k,Nil)) }
  }
  case _ => Map()
}

def day4_1(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_4.txt")
  val txt = bufferedSource.getLines().toList
  val numbers = txt.head.split(",").map(x=>Integer.parseInt(x))
  var board = recursive_4(0, txt.tail)
  val n_boards = board.values.maxBy(x=>x.maxBy(_._1)._1).maxBy(_._1)._1 + 1
  var counts = Array.ofDim[Int](n_boards, 5, 2)
  bufferedSource.close()
  var ret = -1
  breakable {
    for number <- numbers do {
      var board_id_final = -1
      board.getOrElse(number, List()).foreach(loc => {
        val bid = loc._1;
        val row = loc._2;
        val col = loc._3
        counts(bid)(row)(0) += 1
        counts(bid)(col)(1) += 1
        if(counts(bid)(row)(0) == 5 || counts(bid)(col)(1) == 5) {board_id_final = bid}
      })
      board -= number

      if (board_id_final != -1) {
        ret = number * board.map(x => (x._1, x._2.filter(y => y._1 == board_id_final))).filter(x => x._2.nonEmpty).keys.sum
        break
      }
    }
  }
  ret
}
def day4_2(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_4.txt")
  val txt = bufferedSource.getLines().toList
  val numbers = txt.head.split(",").map(x=>Integer.parseInt(x))
  var board = recursive_4(0, txt.tail)
  val n_boards = board.values.maxBy(x=>x.maxBy(_._1)._1).maxBy(_._1)._1 + 1
  val counts = Array.ofDim[Int](n_boards, 5, 2)
  bufferedSource.close()
  var ret = -1
  val boardsFilled = ArrayBuffer.range(0, n_boards);
  breakable {
    for number <- numbers do {
      var board_id_final = -1
      board.getOrElse(number, List()).foreach(loc => {
        val bid = loc._1;
        val row = loc._2;
        val col = loc._3
        counts(bid)(row)(0) += 1
        counts(bid)(col)(1) += 1
        if((counts(bid)(row)(0) == 5 || counts(bid)(col)(1) == 5) && boardsFilled.contains(bid)) {
          boardsFilled -= bid
        }
        if(boardsFilled.isEmpty && board_id_final == -1) board_id_final = bid
      })
      board -= number

      if (board_id_final != -1) {
        ret = number * board.map(x => (x._1, x._2.filter(y => y._1 == board_id_final))).filter(x => x._2.nonEmpty).keys.sum
        break
      }
    }
  }
  ret
}
def day5_helper(m:Array[Array[Int]], cord: (Int, Int, Int, Int)): Unit = {
  var c = cord;

  if(c._1 == c._3 || c._2 == c._4) {
    if(c._1>c._3) c = (c._3, c._2, c._1, c._4)
    if(c._2>c._4) c = (c._1, c._4, c._3, c._2)
    for (y <- c._2 to c._4) {
      for (x <- c._1 to c._3) {
        m(y)(x) += 1
      }
    }
  }
  else {
    //if(c._2>c._4) c = (c._1, c._4, c._3, c._2)
    if((c._1<c._3 && c._2<c._4) || (c._1>c._3 && c._2>c._4)) {
      if(c._2>c._4) c = (c._1, c._4, c._3, c._2)
      var k = if(c._1<c._3) c._1 else c._3
      for (y <- c._2 to c._4) {
        m(y)(k) += 1
        k+=1
      }
    }
    else {
      if(c._2>c._4) c = (c._1, c._4, c._3, c._2)
      var k = if(c._1>c._3) c._1 else c._3
      for (y <- c._2 to c._4) {
        m(y)(k) += 1
        k-=1;
      }
    }
  }
}

def day5_1(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_5.txt")
  val m = Array.ofDim[Int](1000, 1000)
  val txt = bufferedSource.getLines().toList.map(line => {
    val nums = line.split(" ").filter(x => x != "->").map(x => x.split(",").map(num => Integer.parseInt(num)))
    (nums(0)(0), nums(0)(1), nums(1)(0), nums(1)(1))
  }).filter(x=> x._1==x._3 || x._2==x._4)
  txt.foreach(cord => {
    day5_helper(m, cord)
  })
  bufferedSource.close()
  m.flatten.foldLeft(0)((acc, v) => if(v>=2) acc+1 else acc)
}
def day5_2(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_5.txt")
  val m = Array.ofDim[Int](1000, 1000)
  val txt = bufferedSource.getLines().toList.map(line => {
    val nums = line.split(" ").filter(x => x != "->").map(x => x.split(",").map(num => Integer.parseInt(num)))
    (nums(0)(0), nums(0)(1), nums(1)(0), nums(1)(1))
  })
  txt.foreach(cord => {
    day5_helper(m, cord)
  })
  bufferedSource.close()
  m.flatten.foldLeft(0)((acc, v) => if(v>=2) acc+1 else acc)
}

def day6_1(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_6.txt")
  val days = 80
  var m: Array[Int] = bufferedSource.getLines().mkString.split(",").map(x=>Integer.parseInt(x)) //.to(ArrayBuffer)
  bufferedSource.close()
  for( a <- 0 until days){
    val buf = ArrayBuffer[Int]()
    val mapped = m.map(time=>{
      if(time == 0) {
        buf += 8
        6
      } else time-1
    })
    m = mapped ++ buf.toArray
    //println(m.mkString)
  }

  m.length
}

/*
def day6_recursive(fish: Int, time: Int): Int = {
  if(time>fish) day6_recursive(8, time-fish-1) + day6_recursive(6, time-fish-1)
  else 1
}
*/
/*
def day6_recur(times: List[Int], days: Int): List[Int] = {
  if(days>0) {
    val new_times = times.flatMap(x => {
      if (x == 0) List(6, 8)
      else List(x - 1)
    })
    new_times.length :: day6_recur(new_times, days-1)
  }
  else List()
}
*/
def day6_recur(times: List[Int], days: Int): List[Int] = {
  if(days>0) {
    val new_times = times.flatMap(x => {
      if (x == 0) List(6, 8)
      else List(x - 1)
    })
    new_times.length :: day6_recur(new_times, days-1)
  }
  else List()
}
def day6_2(): Long = {
  val days = 256;
  var m: Array[Long] = Array.ofDim(9)

  val bufferedSource = Source.fromFile("src/main/data/input_6.txt")
  bufferedSource.getLines().mkString.split(",").map(x=>Integer.parseInt(x)).foreach(x=>m(x)+=1)
  bufferedSource.close()
  for( i <- 0 until days){
    val new_fish = m(0)
    m(7)+=new_fish
    m = m.takeRight(8) ++ Array(new_fish)
  }
  //val a=bufferedSource.getLines().mkString.split(",").map(x=>Integer.parseInt(x)).map(x=>counts(days-x)).sum\
  m.sum
}
/*
def day6_2(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_6.txt")
  val days = 18
  val m: Array[Int] = bufferedSource.getLines().mkString.split(",").map(x=>Integer.parseInt(x)) //.to(ArrayBuffer)
  bufferedSource.close()
  day6_recursive(m(0), days)
  //m.map(x=>day6_recursive(x, days)).sum
}
*/
def day7_1(): Long = {
  val bufferedSource = Source.fromFile("src/main/data/input_7.txt")
  val txt = bufferedSource.getLines().mkString.split(",").toList.map(x=>Integer.parseInt(x))
  bufferedSource.close()
  val n = txt.max
  val groups: Map[Int, Int] = txt.groupBy(x=>x).map((k,v)=>k->v.length)
  var min: Long = 99999999999
  for( pos <- 0 until n){
    val cur = groups.map(x=>(x._1-pos).abs * x._2).sum
    if(cur < min) min = cur
  }
  //println(groups)
  min
}
def day7_2(): Long = {
  val bufferedSource = Source.fromFile("src/main/data/input_7.txt")
  val txt = bufferedSource.getLines().mkString.split(",").toList.map(x=>Integer.parseInt(x))
  val n = txt.max
  val groups: Map[Int, Int] = txt.groupBy(x=>x).map((k,v)=>k->v.length)
  var min: Long = 99999999999
  for( pos <- 0 until n){
    val cur = groups.map(x=>{
      val a = (x._1-pos).abs
      a*(a+1) / 2 * x._2
    }).sum
    if(cur < min) min = cur
  }
  //println(groups)
  min
}
def day7_helper(groups: Map[Int, Int], pos: Int): Long = {
  groups.map(x=>(x._1-pos).abs * x._2).sum
}
/*
display -> # of segments
1 -> 2
4 -> 4
7 -> 3
8 -> 7
 */
def day8_1(): Long = {
  val bufferedSource = Source.fromFile("src/main/data/input_8.txt")

  val txt = bufferedSource.getLines().map(line=>line.split("""\|""")(1).split(" ").tail).toList.flatten
  val count = txt.map(x=>x.length).groupBy(x=>x).map(x=>x._1 -> x._2.length)
  bufferedSource.close()
  count(2) + count(4) + count(3) + count(7)
}
def day8_helper(txt: List[Array[String]]): Int = {
  val display = txt.head.toList
  val decode = txt(1).tail.toList
  val meaning = scala.collection.mutable.Map[Int, String]()
  val segments = scala.collection.mutable.Map[Char, Char]()
  //println(bufferedSource.getLines().toList.head)
  val lengths = display.groupBy(x=>x.length)
  meaning += (1 -> lengths(2).head)
  meaning += (4 -> lengths(4).head)
  meaning += (7 -> lengths(3).head)
  meaning += (8 -> lengths(7).head)
  segments += 'a' -> meaning(7).toSet.diff(meaning(1).toSet).head // get meaning of a
  meaning += 6 -> display.filter(x=>x.length == 6 && (!x.contains(meaning(1)(0)) || !x.contains(meaning(1)(1))) ).head//find 6
  meaning += 0 -> display.filter(x=>x.length == 6 && x != meaning(6) && meaning(4).toSet.diff(x.toSet).toList.length==1).head//find 0
  meaning += 9 -> display.filter(x=>x.length == 6 && x != meaning(6) && x != meaning(0)).head//find 9
  segments += 'e' -> meaning(8).toSet.diff(meaning(9).toSet).head
  //meaning += 3 -> display.filter(x=>x.length == 5 && x.contains(meaning(1)(0)) && x.contains(meaning(1)(1))).head//find 3
  segments += 'c' -> meaning(8).toSet.diff(meaning(6).toSet).head
  segments += 'f' -> meaning(1).toSet.diff(segments('c').toString.toSet).head
  meaning += 5 -> display.filter(x=>x.length == 5 && x.contains(segments('f')) && !x.contains(segments('c'))).head//find 5
  meaning += 2 -> display.filter(x=>x.length == 5 && x.contains(segments('c')) && !x.contains(segments('f'))).head//find 2
  meaning += 3 -> display.filter(x=>x.length == 5 && x.contains(segments('c')) && x.contains(segments('f'))).head//find 3

  //println(meaning)
  //println(segments)
  val flipped = meaning.map(_.swap)
  val decoded = decode.map(x=> {
    val set = x.toSet
    flipped.find(entry=>entry._1.toSet == set).getOrElse(("",0))._2
  })
  //println(decoded)

  decoded(0) * 1000 + decoded(1) * 100 + decoded(2) * 10 + decoded(3)
}
def day8_2(): Long = {
  val bufferedSource = Source.fromFile("src/main/data/input_8.txt")

  val txt: List[List[Array[String]]] = bufferedSource.getLines().toList.map(
    line=>line.split("""\|""").map(side=>side.split(" ")).toList
  )

  bufferedSource.close()
  //day8_helper(txt(1))
  txt.map(line => day8_helper(line)).sum
}
def day9_1(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_9.txt")
  val txt = bufferedSource.getLines().toList.map(x=>x.toList.map(y=>Integer.parseInt(""+y)))
  val lowest_list = scala.collection.mutable.ListBuffer[Int]()
  val size = (txt.length, txt.head.length)
  for(i <- 0 until size._1){
    for(j <- 0 until size._2){
      var lowest = true
      val cur = txt(i)(j)
      if(i>0 && txt(i-1)(j) <= cur) lowest = false
      if(j>0 && txt(i)(j-1) <= cur) lowest = false
      if(i+1<size._1 && txt(i+1)(j) <= cur) lowest = false
      if(j+1<size._2 && txt(i)(j+1) <= cur) lowest = false
      if(lowest) lowest_list += cur
    }
  }
  bufferedSource.close()
  lowest_list.map(x=>x+1).sum
}
def day9_helper(m: List[List[Int]], initial: (Int, Int)): Int = {
  val basin = ListBuffer[(Int, Int)]()
  val size = (m.length, m.head.length)
  val q = mutable.Queue[(Int,Int)](initial)
  basin += initial
  while(q.nonEmpty){
    breakable {
      val pos = q.dequeue()
      val i = pos._1
      val j = pos._2
      val cur = m(i)(j)
      if(cur == 9) break
      if(i>0 && !basin.contains((i-1,j)) && m(i-1)(j) < 9) {basin += ((i-1,j)); q.enqueue((i-1,j))}
      if(j>0 && !basin.contains((i,j-1)) && m(i)(j-1) < 9) {basin += ((i,j-1)); q.enqueue((i,j-1))}
      if(i+1<size._1 && !basin.contains((i+1,j)) && m(i+1)(j) < 9) {basin += ((i+1,j)); q.enqueue((i+1,j))}
      if(j+1<size._2 && !basin.contains((i,j+1)) && m(i)(j+1) < 9) {basin += ((i,j+1)); q.enqueue((i,j+1))}
    }
  }

  basin.length
}
def day9_2(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_9.txt")
  val txt = bufferedSource.getLines().toList.map(x=>x.toList.map(y=>Integer.parseInt(""+y)))
  val lowest_list = ListBuffer[(Int, Int)]()
  val size = (txt.length, txt.head.length)
  for(i <- 0 until size._1){
    for(j <- 0 until size._2){
      var lowest = true
      val cur = txt(i)(j)
      if(i>0 && txt(i-1)(j) <= cur) lowest = false
      if(j>0 && txt(i)(j-1) <= cur) lowest = false
      if(i+1<size._1 && txt(i+1)(j) <= cur) lowest = false
      if(j+1<size._2 && txt(i)(j+1) <= cur) lowest = false
      if(lowest) lowest_list += ((i, j))
    }
  }
  bufferedSource.close()
  lowest_list.map(x => day9_helper(txt, x)).sortBy(x=>x).reverse.take(3).product
}
def day10_opening_chars: List[Char] = List('(', '[', '{', '<')
def day10_closing_chars: List[Char] = List(')', ']', '}', '>')
def day10_matching_chars: Map[Char, Char] = Map(('(', ')'), ('[', ']'), ('{', '}'), ('<', '>'))
def day10_error_points: Map[Char, Int] = Map((')', 3), (']', 57), ('}', 1197), ('>', 25137))
def day10_completion_points: Map[Char, Int] = Map(('(', 1), ('[', 2), ('{', 3), ('<', 4))
def day10_1_helper(to_close: mutable.Stack[Char], txt: List[Char]): Int = txt match {
  case Nil => 0
  case cur :: t => {
    if(day10_opening_chars.contains(cur)) {
      to_close.push(cur)
      day10_1_helper(to_close, t)
    }
    else {
      if(to_close.isEmpty) 0
      else {
        val to_match = to_close.pop()
        if (day10_matching_chars(to_match) == cur) day10_1_helper(to_close, t)
        else day10_error_points(cur) + day10_1_helper(to_close, t)
      }
    }
  }
}
@tailrec
def day10_2_helper(to_close: mutable.Stack[Char], txt: List[Char]): Long = txt match {
  case Nil => {
    to_close.foldLeft(0L)((acc: Long, v: Char)=> acc * 5 + day10_completion_points(v))
  }
  case cur :: t => {
    if(day10_opening_chars.contains(cur)) {
      to_close.push(cur)
      day10_2_helper(to_close, t)
    }
    else {
      if(to_close.isEmpty) 0
      else {
        val to_match = to_close.pop()
        if (day10_matching_chars(to_match) == cur) day10_2_helper(to_close, t)
        else 0
      }
    }
  }
}
def day10_1(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_10.txt")
  val txt = bufferedSource.getLines().toList

  bufferedSource.close()
  txt.map(line => day10_1_helper(mutable.Stack[Char](), line.toList)).sum
}
def day10_2(): Long = {
  val bufferedSource = Source.fromFile("src/main/data/input_10.txt")
  val txt = bufferedSource.getLines().toList

  bufferedSource.close()
  val ret = txt.map(line => day10_2_helper(mutable.Stack[Char](), line.toList)).filter(x=>x>0).sortBy(x=>x)
  println(ret)
  ret((ret.length)/2)
}
def day11_1(): Int = {
  val bufferedSource = Source.fromFile("src/main/data/input_11.txt")
  val txt = bufferedSource.getLines().toList

  bufferedSource.close()
  0
}

