
import org.scalatest._
import comb.{Match, Success, Failure}

import org.scalatest.prop.PropertyChecks
import org.scalatest.matchers.Matcher

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

class CombSpec 
	extends PropSpec 
	with PropertyChecks
	with Matchers {

	property("`empty` doesn't change the input") {
		forAll("input") { 
			(input: String) => 
				Match(comb.empty)(input) should be (true) 
		}
	}

	property("`char` should be true only if the string begins with the given char") {
		forAll("c", "input") { 
			(c: Char, input: String) => 
				val output = Match(comb.char(c))(input)

				if(input.length > 0)
					output should be (input.charAt(0) == (c))
				else
					output should be (false)
		}
	}

	property("`range` should be true only if the string begins with the char in range") {
		forAll("a", "b", "input") {
			(a: Char, b: Char, input: String) =>
				val output = Match(comb.range(a, b))(input)

				if(input.length > 0) 
					output should be ((a to b) contains input.charAt(0))
				else
					output should be (false)
		}
	}

	// write tests for `either` and `repeat` here
	
	// write tests for compositions here	

	property("`empty` should be the identity combinator") {
		def checkIdentities(p: comb.Parser)(input: String) = {
			val ε = comb.empty
			val output = Match(p)(input)
			Match(p ∙ ε)(input) should be (output)
			Match(ε ∙ p)(input) should be (output) 
		}

		forAll("input") {
			(input: String) =>
				checkIdentities(comb.empty)(input)
		}

		forAll("c", "input") {
			(c: Char, input: String) =>
				checkIdentities(comb.char(c))(input)
		}

		forAll("a", "b", "input") {
			(a: Char, b: Char, input: String) =>
				checkIdentities(comb.range(a, b))(input)
		}		
	}

}