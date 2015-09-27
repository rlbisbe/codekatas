object KarateChop {

  def chop (target: Int, searchBox: Array[Int]) : Int = {
    val size = searchBox.size
    if (size == 0) {
      return -1
    }

    if (size == 1){
      if(searchBox(0) == target){
        return 0
      } else {
        return -1
      }
    }

    val pointer = searchBox.size / 2
    val searchResult = searchBox(pointer)
    if(searchResult == target){
      return pointer
    }

    if (searchResult < target){
      val partialResult = chop(target, searchBox.slice(pointer, size))
      if(partialResult == -1)
        return -1

      return pointer + partialResult
    }

    if (searchResult > target){
      return chop(target, searchBox.slice(0, pointer));
    }

    return -1
  }

  def assert(expected: Int, actual: Int){
    if(expected == actual){
      println("OK");
    }
    else {
      println("Error, expected: " + expected + " actual: "+ actual);
    }
  }

  def main(args: Array[String]){
    test1()
    test2()
    test3()
    test4()
    test5()
    test6()
    test7()
    test8()
    test9()
    test10()
  }

  def test1(): Unit ={
    val target = 2
    val searchBox = Array.emptyIntArray
    assert(-1, chop(target, searchBox))
  }

  def test2(): Unit ={
    val target = 1
    val searchBox = Array(1,2,3,4,5)
    assert(0, chop(target, searchBox))
  }

  def test3(): Unit ={
    val target = 3
    val searchBox = Array(1,2,3,4,5)
    assert(2, chop(target, searchBox))
  }

  def test4(): Unit ={
    val target = 3
    val searchBox = Array(1,2,3)
    assert(2, chop(target, searchBox))
  }

  def test5(): Unit ={
    val target = 1
    val searchBox = Array(1)
    assert(0, chop(target, searchBox))
  }

  def test6(): Unit ={
    val target = 5
    val searchBox = Array(1,2,3,4,5)
    assert(4, chop(target, searchBox))
  }

  def test7(): Unit ={
    val target = 4
    val searchBox = Array(1,2,3,4,5)
    assert(3, chop(target, searchBox))
  }

  def test8(): Unit ={
    val target = 2
    val searchBox = Array(1,2,3,4,5)
    assert(1, chop(target, searchBox))
  }

  def test9(): Unit ={
    val target = 0
    val searchBox = Array(1,2,3,4,5)
    assert(-1, chop(target, searchBox))
  }

  def test10(): Unit ={
    val target = 7
    val searchBox = Array(1,2,3,4,5)
    assert(-1, chop(target, searchBox))
  }
}