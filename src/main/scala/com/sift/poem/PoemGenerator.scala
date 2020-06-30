package com.sift.poem

import scala.util.matching.Regex
import scala.util.Random
import scala.util.parsing.combinator._


object Main extends PoemGenerator with App {
  print(generatePoem("POEM"))
}

trait PoemGenerator {

  object GrammarImporter {

    //Files in relative path main/resources, default file is "Grammar Rules.txt",
    //test files are of form "Test Grammar x.txt" where x = an Int
    private val grammarFile: String = "Grammar Rules.txt"

    //validLine is arg to matches method in grammarRules. Must be a String, not a Regex
    private val validLine: String = ".+:.+"

    //Mapped grammar rules line by line of form "KEY: VALUE" after filtering source for valid lines
    val grammarRules: Map[String, String] = LoanPattern.using(io.Source.fromResource(grammarFile)) { source => {
      (for {
        line <- source.getLines.filter(_.matches(validLine))
        Array(key, value) = line.trim.split(":")
      } yield key -> value).toMap
    }
    }
  }


  object PoemParser extends RegexParsers {

    private val knownKeywords = Map(
      "LINEBREAK" -> "\n",
      "END" -> "",
      "越线" -> "\n",
      "结束" -> ""
    )

    //Character used to separate options when one should be chosen randomly
    private val separator: String = "|"

    //A term can be anything except characters reserved for parsing
    private val term: Regex = new Regex("[^:$<>" + separator +"]+")

    //Parser combinators tokenize grammar instead of changing grammar in place
    def rule: Parser[Any] = rep1(multiChoice | keyword | reference) ^^ (x => x.mkString(" "))

    def keyword: Parser[Any] = "$" ~> term ^^ (x => Keyword(x))

    def reference: Parser[Any] = "<" ~> term <~ ">" ^^ (x => Reference(x))

    def multiChoice: Parser[Any] = rep1sep(choice, separator) ^^ (x => MultiChoice(x))

    def choice: Parser[Any] = keyword | reference | term

    //Case classes used for tokenizing; superclass "Token" provided for documentation, can be used for pattern matching
    abstract class Token

    case class MultiChoice(choices: List[Any]) extends Token {
      override def toString: String = Random.shuffle(choices).head.toString
    }

    case class Reference(ref: String) extends Token {
      //References are looked up by recursively calling parse on their values in grammarRules after being tokenized
      override def toString: String = parse(rule, GrammarImporter.grammarRules(ref)).toString.split(": ")(1)
    }

    case class Keyword(command: String) extends Token {
      override def toString: String = if (knownKeywords.contains(command)) knownKeywords(command)
      else throw new Exception("Undefined keyword found. Add keyword to knownKeywords and retry.")
    }
  }

  def generatePoem(startSymbol: String) : String = {
    //split on ":" removes parser success code for cleaner output
    PoemParser.parseAll(PoemParser.rule, GrammarImporter.grammarRules(startSymbol)).toString.split(":")(1)
  }

}

object LoanPattern {
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}
