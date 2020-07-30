class Rational(x: Int, y: Int) {
 
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  
  val gcdvalue = if(gcd(x, y) < 0) -gcd(x, y) else gcd(x, y)
  
  val numer = x / gcdvalue
  
  val denom = y / gcdvalue
  
  
  def neg = new Rational(-this.numer,this.denom)
   
  def +(r:Rational) = new Rational( this.numer * r.denom + r.numer * this.denom, denom * r.denom)
  
  def -(r:Rational) = this+r.neg
  
  override def toString = this.numer + "/" + this.denom
 
}

class Account(id:String,n: String, b: Double) {
  
  val nic:String=id
  
  val acnumber: String = n
  
  var balance: Double = b
  
  def withdraw(a:Double) =
    this.balance=this.balance-a

  def deposit(a:Double) =
    this.balance=this.balance+a
  
  def transfer(a:Account,b:Double)= {
    this.withdraw(b)
    a.deposit(b)
  }
  
  override def toString = "["+nic+":"+acnumber +":"+ balance+"]"
  
}


object FunctionsAndData {

  def main(args: Array[String])={
  	  
  val x = new Rational(3,4)
  
  val y = new Rational(5,8)
  
  val z = new Rational(2,7)
  
  
  println(x.neg)  //Question1
  
  println(x-y-z)  //Question2
  
  val a1 = new Account("971991313V", "76961664", 1000)
  
  val a2 = new Account("945445779V", "76964574", 2000)
  
  val a3 = new Account("975424545V", "76965434", -1000)
  
  val a4 = new Account("976545321V", "76456498", 2000)
  
  
  a1.transfer(a2, 3500.00) //Question3
  
  println("Account1 Bal: " + a1.balance)
  
  println("Account2 Bal: " + a2.balance)
  
  //Q4 all
  var bank:List[Account]=List(a1,a2,a3,a4)
  
  val overdraft=(b:List[Account])=> b.filter(_.balance < 0)
  
  val interest=(b:List[Account])=>b.map(x=> if(x.balance >=0) x.balance * 1.05 else x.balance * 1.1)

  val balance=(b:List[Double]) => b.reduce((x,y) => x+y )

  
  println(overdraft(bank))
  
  var bank2 = interest(bank)
  
  println(balance(bank2))
  
}
  
}