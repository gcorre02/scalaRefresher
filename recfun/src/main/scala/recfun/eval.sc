def pascal(c: Int, r: Int): Int = {val length = r +1;if(c == 0 || c == r) 1;else{val aboveLeft = pascal(c-1,r-1);    val aboveRight = pascal(c,r-1);    aboveLeft + aboveRight;}}

println("Pascal's Triangle")
for (row <- 0 to 10) {
  for (col <- 0 to row)
    print(pascal(col, row) + " ")
println()}

def balance(chars: List[Char]): Boolean = {val openCounter = 0;val closerCounter = 0;def innerFunc(oc:Int, cc:Int, tails: List[Char]):Boolean = {if(tails.isEmpty && oc == cc) return true;else if(tails.isEmpty && oc != cc)return false;var localOc = oc;var localcc = cc;if(tails.head == ('(')) localOc += 1;else if(tails.head == (')')) localcc += 1;if(localcc > localOc) return false;else innerFunc(localOc,localcc,tails.tail);};if(chars.isEmpty) return true;else return innerFunc(openCounter, closerCounter, chars)}

val a = "(if (zero? x) max (/ 1 x))";val b = "I told him (that it’s not (yet) done). (But he wasn’t listening)";val c = ":-)";val d = "())("

balance("".toList)
balance(a.toList)
balance(b.toList)
balance(c.toList)
balance(d.toList)
balance("some string without parens".toList)
balance("(((".toList)
balance("(()".toList)

def countChange(money: Int, coins: List[Int]): Int = {
  if(money == 0 || coins.isEmpty) 0
  else 5
}

countChange(5,List())
countChange(4,List(1,2)) //3
countChange(300,List(5,10,20,50,100,200,500)) //1022
countChange(301,List(5,10,20,50,100,200,500)) //0
countChange(300,List(500,5,50,100,20,200,10)) //1022


