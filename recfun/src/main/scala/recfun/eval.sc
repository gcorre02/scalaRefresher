def pascal(c: Int, r: Int): Int = {val length = r +1;if(c == 0 || c == r) 1;else{val aboveLeft = pascal(c-1,r-1);    val aboveRight = pascal(c,r-1);    aboveLeft + aboveRight;}}

println("Pascal's Triangle")
for (row <- 0 to 10) {
  for (col <- 0 to row)
    print(pascal(col, row) + " ")
println()}

def balance(chars: List[Char]): Boolean = {
  val openCounter = 0;
  val closerCounter = 0;

  true
}

val a = "(if (zero? x) max (/ 1 x))";val b = "I told him (that it’s not (yet) done). (But he wasn’t listening)";val c = ":-)";val d = "())("

balance(a.toList)
balance(b.toList)
balance(c.toList)
balance(d.toList)
