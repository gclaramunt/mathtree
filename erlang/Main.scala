/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mathtree

object Main {

 object Op extends Enumeration {
    type Op = Value
    val Add, Subtract, Multiply, Divide = Value
  }
  import Op._

  trait Term
  case class Number(value: Int) extends Term {
    val kind = 1
  }
  case class Operator(value: Op) extends Term {
    val kind = 2
  }

  trait Expression
  case class SimpleExpr(value: Int) extends Expression {
    val kind = 1
  }
  case class ComplexExpr(op: Operator, op1: Expression, op2: Expression) extends Expression {
    val kind = 2
  }
  
  def main(args: Array[String]): Unit = {
    var total:Long=0;
    val iters=100000
    var result = 0
    for(i <- 1 to iters) {
      val start_time = System.nanoTime();
      val terms = make_terms(args);
      val expr = make_expr(terms, List());
      result = calculate(expr)
      val end_time = System.nanoTime();
      total=total+(end_time - start_time)/1000;
      //println("Scala\ttime = " + (end_time - start_time)/1000 + "\tresult = " + result);
    }
    println("result="+result);
    println ("avg ="+ (total/iters));
  }

  
  def make_terms(args: Seq[String]): List[Term] = 
    if (args.isEmpty) {
      Nil
    } else {
      val arg = args.head;
      val next = arg match {
        case "+" => new Operator(Add)
        case "-" => new Operator(Subtract)
        case "x" => new Operator(Multiply)
        case "/" => new Operator(Divide)
        case n => new Number(arg.toInt)
      }
    next :: make_terms(args.tail)
   }

  def make_expr(terms: List[Term], stack: List[Expression]): Expression = {
    val head::rest=terms
    head match {
      case x: Operator => {
          val st0::st1::srest=stack
          rest match {
            case Nil => new ComplexExpr(x, st1, st0)
            case _ =>  make_expr(rest, new ComplexExpr(x, st1, st0) :: srest)
          }
        }
      case Number(value) => make_expr(terms.tail, new SimpleExpr(value) :: stack)
    }
  }

  def calculate(expr: Expression): Int = {
    expr match {
      case SimpleExpr(value) => value;
      case ComplexExpr(op,op1,op2) =>
      op.value match {
        case Add =>  calculate(op1) + calculate(op2)
        case Subtract => calculate(op1) - calculate(op2)
        case Multiply => calculate(op1) * calculate(op2)
        case Divide =>   calculate(op1) / calculate(op2)
      }
    }
  }
}
