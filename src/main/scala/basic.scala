import java.io.File
import scala.io.Source
import scala.util.Try // for try catch  Exception handeling

object basic {
  //====== ============================ Token Function ==================================
  var operatorsList = List("+" , "/" , "*" ,"==" , "><" , "<" , ">" , "<=" , ">=" , "and" , "or" , "^" ,"-" , "not" , ";" , ":" , "=")
  // class for variables
  class Variables(n: String, v: String, t: String , const : Boolean = false) {
    var isConstant = const  // boolean check for constant variables
    var name: String = {  // name of variables
      if (isVarName(n)) // check valid variable name
        n
      else {
        println("variable name rule undefied >> "  + n);
        sys.exit(5)
      }
    }
    var type_v: String = if (isType(t)) t else {    // data type of variale // check valid variable type (int ,bool , alpha)
      println("Type not recoginized in variable declaration >> " + t);
      sys.exit(6)
    }
    var value: String = if(v != "") {  // set values
      type_v match {
      case "bool" => {if (v == "tt" || v == "ff") v else {println("typemismatch");sys.exit(7)}}
      case "alpha" => {if ((v(0) == '\"' && v(0) == v.last) || (v(0) == '\'' && v(0) == v.last)) v.substring(1).dropRight(1)
                      else {println("typemismatch");sys.exit(8)}};
      case "int" => {if (isInt(v)) v else {println("typemismatch " + v);sys.exit(9)}}};
    }else {
      type_v match{ // default values
        case "bool" => "ff"    // false for boolean
        case "int"  => "0"     // 0 for intergers
        case "alpha"=> ""      // empty string for alpha
      }
    }
    // return true if type and name of variable is same
    def matchNameAndType(nV:Variables): Boolean = ((this.name == nV.name) && (this.type_v == nV.type_v))
    // print variables content
    def printit() = println(name + " : " + type_v + " = " + value)
  }
  // task raw code and pass it to recursive function
  def lex(text: String) = {
    getToken("", text, List.empty[String])
  }
  // identify token strings values and other words in a list recursively
  def getToken(acc: String, text: String, L: List[String]): List[String] = {
    if (text.length <= 0) { // end function if string length is 0
      return L::: List(acc )::: List("ENDOFFILE")
    }
    else {  // check each character
      if( text(0) == '\"'){ // for string
        var  str = getString("", text.substring(1)) // make a token for string till next ' " ' character
        return getToken("", str._2, L:::List("\""+str._1) )
      }
      if (text(0) == ' ' || text(0) == '\r') { // for carriage return and white spaces
        return getToken("", text.substring(1), if (acc == "") L else L ::: List(acc))
      }
      if(text(0) == ';'){   // for semicolon
        return getToken("", text.substring(1), if (acc == "") L ::: List(text(0).toString) else L ::: List(acc , text(0).toString))
      }
      if((text(0) == '=' || text(0) == '<' || text(0) == '>' )&& text(1) == '='){ // for operaters with two characters( == <= >= )
        return getToken("", text.substring(2), if (acc == "") L ::: List(text(0).toString+text(1).toString) else L ::: List(acc , text(0).toString+text(1).toString))
      }
      if(isOperater(acc) || isOperater(text(0).toString)){ // for rest of operaters (+ ,- , * etc) in List defined above
        return getToken("", text.substring(1), if (acc == "") L ::: List(text(0).toString) else L ::: List(acc , text(0).toString))
      }
      if(text(0).toString.matches("[ -~]")){ // for any pintable character
        val word = acc + text(0)
        return getToken(acc + text(0), text.substring(1), L)
      }
      // else Exit
      println("Unknown Character forund >> " , text(0))
      sys.exit(10)
    }
  }
  //==========================================================================================================================
  // return a token containing value of type ALPHA (Used in get Token)
  def getString(acc: String, text: String): Tuple2[String,String] = {
    if (text.length <= 0){
      println("End of string not definned")
      sys.exit(11)
    }
    if(text(0) == '\"')
      return (acc+"\"",text.tail)
    else
      getString(acc + text(0), text.tail)
  }
  /*
    checking functions
  */
   // check datatype
  def isType(t: String): Boolean = if (t == "bool" || t == "int" || t == "alpha") true else {
    println("Type not recoginized " + t);
    sys.exit(6)
  }
  // integer
  def isInt(text: String) = Try {
    text.toInt
  }.isSuccess
  // check booloean
  def isBool(text: String) = text == "tt" || text == "ff"
  // check variable name First alpha and rest is Alpha numeric and * # $_
  def isVarName(text: String) = text.matches("^[a-zA-Z][\\w\\d*#$_]*")
  // check newline
  def isNewLine(text: String) = text == "__NEWLINE__"
  // check semicolen
  def isSemicolen(text: String) = text == ";"
  // check operater in operator list Usend in getTokens ^
  def isOperater(p:String) :Boolean = {
    for(op <- operatorsList ){
      if(op == p)
        return true
    }
    return false
  }
  // check variable already exists (match names only)
  def findVariable(vname:String, vList:List[Variables]) :Boolean= {for(v <- vList ){ if(v.name == vname)return true};return false}
  //============================= EXPR Function Start ==========================================
  /*
    return a Value depending upon list of token given till ; or new line
    expression is calculated from right to left
    eg 4+8*9-1 = 4+8*8 = 4+64 = 68
  */
  def expr(acc:String, txtnVar :Tuple2[List[String] , List[Variables]]):Tuple3[String , List[String] , List[Variables]]={
    // for test purpose*****
    //println("in EXp")
    // *********************
    if(txtnVar._1.length >0) { // if there is token(s) in token List
      if (isNewLine(txtnVar._1(0)) || isSemicolen(txtnVar._1(0))) { // If end of Statment
        return Tuple3(acc, txtnVar._1.drop(1), txtnVar._2)
      }
      if (isOperater(txtnVar._1(0))){// check if operater
        var result = operatersOperations(rVal = acc, OP = txtnVar._1(0),lVal = (txtnVar._1.drop(1),txtnVar._2))
        return (result._1, result._2._1 ,result._2._2)
      }
      if(findVariable(txtnVar._1(0), txtnVar._2)){ // check if variable
        for(v <- txtnVar._2) {
          if (v.name == txtnVar._1(0)) {
            return (expr(acc+v.value, (txtnVar._1.drop(1), txtnVar._2)))
          }
        }
      }
      expr(acc + txtnVar._1(0), Tuple2(txtnVar._1.drop(1), txtnVar._2))// recurrsion
    }
    else{// when token ends return value of Last calculation
      return (acc , txtnVar._1 , txtnVar._2)
    }
  }
  //============================= EXPR Function End ==========================================
  //============================= BinOP Function Start =======================================
  /*
    calculate on base of operater!
   */
  def operatersOperations(rVal: String, OP : String, lVal: (List[String], List[Variables])):Tuple2[String ,Tuple2[List[String],List[Variables]]] = {
    if(OP ==  "-" || OP == "not"){ // for unarray operations
      var left = expr("", lVal)// calculate expression
      var result = invertOpfunction(left._1 ); // invert results
      return (result, (left._2, left._3)); // return with changes
    }
    else if(isInt(rVal)){// for Integers
      var right:Int = rVal.toInt  // convert to integer
      var left = expr("", lVal)   // calculate Expression 2 + (x+6) <- x+6
      if(isInt(left._1))
      {
        if (OP == "+") { // addition
          var result = right + left._1.toInt;
          return (result.toString, (left._2, left._3));
        }
        else if (OP == "/") { // subtraction
          var result = right - left._1.toInt;
          return (result.toString, (left._2, left._3));
        }
        else if (OP == "*") { // multiplication
          var result = right * left._1.toInt;
          return (result.toString, (left._2, left._3));
        }
        else if (OP == "==") { // equal to
          var result = right == left._1.toInt;
          return (if (result) "tt" else "ff", (left._2, left._3));
        }
        else if (OP == "><") { // not equal to
          var result = right != left._1.toInt;
          return (if (result) "tt" else "ff", (left._2, left._3));
        }
        else if (OP == "<") { // less than
          var result = right < left._1.toInt;
          return (if (result) "tt" else "ff", (left._2, left._3));
        }
        else if (OP == ">") { //  greater than
          var result = right > left._1.toInt;
          return (if (result) "tt" else "ff", (left._2, left._3));
        }
        else if (OP == "<=") { // less than equal to
          var result = right <= left._1.toInt;
          return (if (result) "tt" else "ff", (left._2, left._3));
        }
        else if (OP == ">=") { // greater than equla to
          var result = right >= left._1.toInt;
          return (if (result) "tt" else "ff", (left._2, left._3));
        }
        else { // undeifined characters
          println("Invalid operator " + OP); sys.exit(152)
        }
      }else{println("left val not int >> " +left._1 );sys.exit(11)} //unidentified integer
    }
    else if(isBool(rVal)){// first value bool
      var left = expr("", lVal)
      if(!isBool(left._1)){println("Second Oprand not bool >> " + left._1); sys.exit(12)} // second value bool
      if (OP == "and" || OP == "or" || OP == "^") { // calculate
        var result = logicalOpfunction(rVal, OP,left._1 );
        return (result, (left._2, left._3));
      }
    }
    println("Invalid operation " +rVal+""+ OP  + lVal._1(0)); sys.exit(13)

  }
  //============================= BinOP Function End =======================================
  //============================= Logical Function Start =======================================
  def logicalOpfunction(rVal: String, OP : String ,lVal:String):String = { //calculate boolean opeartions
    if(!isBool(rVal)|| !isBool(lVal)){println("Error in Value Bool expected >> "  + rVal +" " + lVal );sys.exit(72);}
    if(OP == "and"){return if((rVal == "tt")&&(lVal == "tt"))"tt" else "ff"}
    else if(OP == "or"){return if((rVal == "tt")||(lVal == "tt"))"tt" else "ff"}
    else if(OP == "^"){return if(rVal== lVal)"tt" else "ff"}
    else {println("Invalid operator for Boolean Operations" + OP) ; sys.exit(151)}
  }
  //============================= Logical Function Ends =======================================
  def invertOpfunction(lVal:String):String = {
    if(isInt(lVal)){
      var num = lVal.toInt
      return (-1 * num).toString
    }
    else if(isBool(lVal)){
      if(lVal == "tt"){return "ff"}
      else{return  "tt"}
    }
    println("Error in Inverting Operator "+lVal)
    sys.exit(80);
  }
  def decVariable(isConst:Boolean ,txtnVar :Tuple2[List[String] , List[Variables]]): Tuple2[List[String] , List[Variables]] ={
   // println("in declare variable")
    if(isVarName(txtnVar._1(0))) {
      if(findVariable(txtnVar._1(0), txtnVar._2)){ println("variable already exists");sys.exit()}
      var name = txtnVar._1(0)
      if (txtnVar._1(1)== ":")
      {
        if (isType(txtnVar._1(2))) {
          var type_v = txtnVar._1(2)
          if (txtnVar._1(3) == "=") {

            // call expression
            var result = expr("" , Tuple2(txtnVar._1.drop(4) , txtnVar._2 ))
             return (result._2,txtnVar._2 ::: List(new Variables(n = name , v = result._1 , t = type_v , isConst)) )
          }
          return (txtnVar._1.drop(3),txtnVar._2 ::: List(new Variables(n = name , v = "" , t = type_v , isConst)) )
        }
      }
    }
    println("Syntax Error in variable declaration" + txtnVar._1(0))
    sys.exit(7)
  }
  def printfunc(txtnVar :Tuple2[List[String] , List[Variables]]): Tuple2[List[String] , List[Variables]] ={
    var result = expr("", txtnVar)
    println(result._1.stripPrefix("\"").stripSuffix("\""))
    return  Tuple2(result._2,result._3)
  }
  def assignmentfunction(varName:String ,txtnVar :Tuple2[List[String] , List[Variables]]): Tuple2[List[String] , List[Variables]]={
    var result = expr("" , txtnVar)
    var type_v:String = if(isInt(result._1))"int"
                        else if(isBool(result._1)) "bool"
                        else if(result._1(0) == '"' || result._1.last == '\"')"alpha"
                        else {println("Error Invalid type returned from Expresstion "+result._1);sys.exit(200)}
    var nVar = new Variables(n = varName ,t = type_v , v = result._1 )
    var varInList = indexOfVariable(varName , txtnVar._2)
    if(txtnVar._2(varInList).isConstant){println("Cannot not assign value to a constant variable >> " + varName);sys.exit(45)}
    if(!txtnVar._2(varInList).matchNameAndType(nVar)){
      println("Variable of type "+ txtnVar._2(varInList).type_v + " but given type " + nVar.type_v)
      sys.exit(201)
    }


    var splitedList = txtnVar._2.splitAt(varInList)
    var ListWithNewVariable = List(nVar):::splitedList._2.tail
    var finalList = splitedList._1:::ListWithNewVariable
    return  (result._2,finalList)

  }
  // return index of matching variable in given list
  def indexOfVariable(name:String ,lst: List[Variables]):Int = {
    for(i <- 0 until lst.length){
      if(lst(i).name == name){
        return i
      }
    }
    -1
  }
  //================================= if Statement Function =================================
  /*
  get Block  of statements for if and while
   */
  def getBlock(of:String ,block:List[String],code: List[String]):Tuple2[List[String],List[String]]={
    of match{
      case "if" =>{
        if(code(0) == "then"){
          return (block ,code)
        }else if(isNewLine(code(0))){
          println("Error THEN expected at >> " + code(0))
          sys.exit(265)
        }
      }
      case "then" => {
        if(code(0) == "else" || isNewLine(code(0))){
          return (block ,code)
        }
      }
      case "else" =>{
        if(isNewLine(code(0))){
          return (block , code)
        }
      }
      case "while" =>{
        if(code(0) == "do"){
          return (block ,code)
        }
      }
      case "do" =>{
        if(isNewLine(code(0))){
          return (block ,code)
        }
      }
      case _ => {
        println("Wrong Block Type >> " + of)
        sys.exit(260)
      }
    }
    getBlock(of , block:::List(code(0)),code.tail)

  }
  /*
    Skip Block of statements for if else
   */
  def skipBlock(acc :String ,code :List[String]):List[String] = {
    if (acc == "then" && code(0) == "else") {
      return code
    } else if (isNewLine(code(0))) {
      return code.tail
    }
    skipBlock(acc, code.tail)
  }
  /*
    Execution for a IF statement
   */
  def ifStatementFinction(txtnvar:Tuple2[List[String] , List[Variables]]):Tuple2[List[String] , List[Variables]]={
    var ifCondition = getBlock("if",List.empty,txtnvar._1) // get condition
    var condition = expr("",(ifCondition._1 , txtnvar._2)) // calclulate condtion
    if(condition._1 == "tt"){ // if true
      if(ifCondition._2(0) == "then"){ // execute then BLock
        var thenBlock = getBlock(ifCondition._2(0) , List.empty ,ifCondition._2.tail ) // get then block
        var afterExecution = interpret(thenBlock._1 , condition._3) // execute then block
        if(thenBlock._2(0) == "else"){ // skip else block
          return (skipBlock("" , thenBlock._2) , afterExecution._2)
        }
        return (thenBlock._2 , afterExecution._2) // return to normal flow
      }
      println("Then Expected " + condition._1)
      sys.exit(270)
    } // if false
    else if(condition._1 == "ff"){
      var thenskip = skipBlock("then" ,ifCondition._2.tail )
      if(thenskip(0) == "else") {
        var elseBlock = getBlock("else" , List.empty, thenskip.tail)
        var afterExecution = interpret(elseBlock._1 , condition._3)
        return (elseBlock._2 , afterExecution._2)
      }
      println("Else not  at >>"+ thenskip(0))
      sys.exit(262)
    }
    println("IF condition not returning bool "+ condition._1)
    sys.exit(250)
  }
  //================================ while Statement Function ===============================
  def ittLoop(condition:List[String] , block:List[String] , varList: List[Variables]): List[Variables] ={
    var result = expr("",(condition,varList))
    if( result._1 == "ff"){
      return varList
    }else if(result._1 == "tt"){
      var executedBlock = interpret(block,varList)
      return ittLoop(condition , block ,executedBlock._2 )
    }
    println("Something happened at while Loop >> " + result._1)
    sys.exit(300)
  }
  def whileStatement(txtnvar:Tuple2[List[String] , List[Variables]]):Tuple2[List[String] , List[Variables]]={
    var whileCondition = getBlock("while",List.empty,txtnvar._1) // take condition from code
    if(whileCondition._2(0) == "do") {
      var doBlock = getBlock("do", List.empty, whileCondition._2.tail) // take while block from code
      var whileExecuted = ittLoop(whileCondition._1 ,doBlock._1 , txtnvar._2)
      return (doBlock._2 , whileExecuted)
    }
    println("Do not found after while Condition >> " +whileCondition._2(0))
    sys.exit(305)
  }
  //================================== Interprete Function ==================================
  def interpret(txtnVar :Tuple2[List[String] , List[Variables]]): Tuple2[List[String] , List[Variables]] ={
      if(txtnVar._1.length <=0){ // if end of code end interpretation
        //println("End of file")
        return txtnVar
      }else{// else check for statement type
        var word = txtnVar._1(0)
        if(word != ""){// empty
          if(word == "ENDOFFILE"){
            return txtnVar
          }
          if(word == "var"){// variable declarations
            return interpret(decVariable(false,(txtnVar._1.drop(1) , txtnVar._2)))
          }
          if(word == "const"){// constant declarations
            return interpret(decVariable(true , (txtnVar._1.drop(1) , txtnVar._2)))
          }
          if(word == "print"){// print statement
            return interpret(printfunc(Tuple2(txtnVar._1.drop(1) , txtnVar._2)))
          }
          if(word == "if"){// if statment
            return interpret(ifStatementFinction(Tuple2(txtnVar._1.drop(1) , txtnVar._2)))
          }
          if(word == "while"){// while statement
            return interpret(whileStatement(Tuple2(txtnVar._1.drop(1) , txtnVar._2)))
          }
          if(isVarName(word)){// assignment operation
            if(txtnVar._1(1) == "="){
              var result = assignmentfunction(word , (txtnVar._1.drop(2),txtnVar._2))
              return interpret(result)
            }
            //return interpret(expr("",))
          }
          if(isNewLine(word) || isSemicolen((word))){ // new line
            return interpret(Tuple2(txtnVar._1.drop(1) , txtnVar._2))
          }
        }else {
          return interpret(Tuple2(txtnVar._1.drop(1) , txtnVar._2))//
        }
        println("Interpreter Crashed at >>" + word + "<<")
        sys.exit(100)
      }
  }
  // read code file
  def fileread(fileName : String):String ={
    val file = new File(fileName)
    if(fileName.contains(".app")){
      if(file.exists()) {
        return Source.fromFile(fileName).mkString
      }
    }
    println("File not Supported or Cannot find file \"" + fileName + "\" ")
    sys.exit(1) // file not found

  }

  def main(args:Array[String]): Unit ={
    /*if(args.length <1){
      println("No file name specified ! give file name of format : filename.app")
      sys.exit(3)
    }*/
    print("Enter file name >>> ")
    var in = readLine()
    var read = fileread(in)
    var text = read.replaceAll("\n" , " __NEWLINE__ ").replace("\r", " ")
    var listOfWords = lex(text)
    //listOfWords.foreach(println)
    println("Output- >>")
    var answer = interpret(Tuple2(listOfWords,List.empty))
//    for(i <-0 until answre._2.length){
//      answre._2(i).printit()
  }
}
