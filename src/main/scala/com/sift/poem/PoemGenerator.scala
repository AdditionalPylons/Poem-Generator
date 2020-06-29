package com.sift.poem

import scala.util.matching.Regex
import scala.util.Random
import scala.util.parsing.combinator._


object Main extends PoemGenerator with App {
  println(generatePoem("POEM"))
}

trait PoemGenerator extends RegexParsers {

  val grammarFile: String = "/home/bill/Documents/Code/Scala Code/PoemGenerator/src/main/scala/com/sift/poem/Grammar Rules.txt"

  //Mapped grammar rules line by line of form "KEY: VALUE"
  val grammarRules: Map[String, String] = LoanPattern.using(io.Source.fromFile(grammarFile)) { source => {
    (for {
      line <- source.getLines
      Array(key, value) = line.split(":")
    }yield key -> value).toMap
  }
  }

  val knownKeywords = Map("LINEBREAK" -> "\n", "END" -> "", "越线" -> "\n", "结束" -> "")

  //Override parser def of white space without "\n" because line breaks are meaningful in grammar
  override val whiteSpace: Regex = "[ \t\r\f]+".r

  //Character used to separate options when one should be chosen randomly
  val separator: String = "|"

  //Parser combinators
  val term: Regex = "\\w+".r

  def definition: Parser[Any] = term ~ ":" ~> rule <~ "\n"

  def rule: Parser[Any] = rep1(multiChoice | keyword | reference) ^^ (x => x.mkString(" "))

  def keyword: Parser[Any] = "$" ~> term ^^ (x => Keyword(x))

  def reference: Parser[Any] = "<" ~> term <~ ">" ^^ (x => Reference(x))

  def multiChoice: Parser[Any] = rep1sep(choice, separator) ^^ (x => MultiChoice(x))

  def choice: Parser[Any] = keyword | reference | term

  //Case classes used for tokenizing; superclass "Token" provided for documentation
  abstract class Token
  case class MultiChoice(choices: List[Any]) extends Token {
    override def toString: String = Random.shuffle(choices).head.toString
  }
  case class Reference(ref: String) extends Token {
    //References are looked up by recursively calling parse on their values in grammarRules after being tokenized
    override def toString = parse(rule,grammarRules(ref)).toString.split(": ")(1)
  }
  case class Keyword (command: String) extends Token{
    override def toString = if (knownKeywords.contains(command)) knownKeywords(command)
    else throw new Exception("Undefined keyword found. Add keyword to knownKeywords and retry.")
  }


  def generatePoem(startSymbol: String) : String =
    //split on ": " removes parser success code for cleaner output
    parseAll(rule,grammarRules(startSymbol)).toString.split(": ")(1)

}

object LoanPattern {
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}
