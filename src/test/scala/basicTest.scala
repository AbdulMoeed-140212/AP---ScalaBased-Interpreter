import org.scalatest.FunSuite

class basicTest extends FunSuite {

  test("testIsBool") {
    assert(basic.isBool("tt") === true)
    assert(basic.isBool("ff") === true)
    assert(basic.isBool("affdsf") === false)
    assert(basic.isBool("1452ff") === false)
    assert(basic.isBool("ttlein") === false)
  }

  test("testIsInt") {
    assert(basic.isInt("052") == true)
    assert(basic.isInt("-052") == true)
    assert(basic.isInt("52328349") == true)
    assert(basic.isInt("999999999") == true)
    assert(basic.isInt("2147483647") == true)
    assert(basic.isInt("-2147483648") == true)
    assert(basic.isInt("2147483648") == false)
    assert(basic.isInt("-2147483649") == false)
    assert(basic.isInt("a052") == false)
  }
  test("testOperaterOperations"){
    assert(basic.operatersOperations("8","+",(List("9"),List.empty))._1 === "17")
    assert(basic.operatersOperations("8","/",(List("9"),List.empty))._1 === "-1")
    assert(basic.operatersOperations("4","*",(List("10"),List.empty))._1 === "40")
    assert(basic.operatersOperations("4","<",(List("10"),List.empty))._1 === "tt")
    assert(basic.operatersOperations("4",">",(List("10"),List.empty))._1 === "ff")
    assert(basic.operatersOperations("4","==",(List("10"),List.empty))._1 === "ff")
    assert(basic.operatersOperations("4","><",(List("10"),List.empty))._1 === "tt")
    assert(basic.operatersOperations("4","==",(List("4"),List.empty))._1 === "tt")
    assert(basic.operatersOperations("4","><",(List("4"),List.empty))._1 === "ff")
  }
}
