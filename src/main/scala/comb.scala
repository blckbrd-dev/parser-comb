
package object comb {
	/* we could have used many things here:
		- the first valid thing would be to use (Bool, T)
		- the second would be to use the builtin Option[T]
		- the third would be to use scala.util.Try[T]
		  which has Success and Failure, BUT Failure has to
		  throw an exception; we needed a simpler version

		I think this is nicer than (Bool, T) and less heavy
		than any of the other options, that's why...
	*/
	sealed trait Score[T] 
	case class Success[T](value: T) extends Score[T]
	case class Failure[T](value: T) extends Score[T]

	// this is the same as `String => (Bool, String)`, technically
	type Checker = String => Score[String]

	abstract class Parser extends Checker {
		/* composition is usually read as
		    f ∘ g -- f after g
		   the order isn't **really** important,
		   but we'll try to be upholding math things
		*/
		def ∘(p: Parser): Parser =
			(input: String) => p(input) match {
				/* if `p` succeeded parsing, then
				 	continue with me!
				*/
				case Success(rest) => this(rest)

				// if `p` didn't, then it already failed
				case fail => fail
			}

		// inline composition (reversed from above)
		// this is how regex does it: f ∙ g -- f before g
		def ∙(p: Parser) = p ∘ this
	}

	object Match {
		def apply(p: Parser)(s: String) = p(s) match {
			case Success(_) => true
			case _ => false
		}
	}

	// `empty` combinator should just return true for whatever case
	def empty: Parser = 
		(whatever: String) => Success(whatever)

	/* `char` combinator returns true only if the head of the input
		is the same as the argument `c`
	*/
	def char(c: Char): Parser =
		(input: String) =>
			input.headOption match {
				case Some(x) if x == c => 
					Success(input.tail)
				case _ => 
					Failure(input)
			}

	/* `range` combinator returns true if the head of the input is
		in the range between characters `a` and `b`, given as
		arguments; we use Scala's `(a to b)` range definition
		here: this works for numbers and characters alike!

		example:
			comb.range('a', 'z') == [a-z]
	*/
	def range(a: Char, b: Char): Parser = ???

	/* `either` should return true if any of the parsers in the `ps`
		sequence return true

		example:
			comb.either(comb.char('a'), comb.char('b')) == a | b
			comb.either(comb.char('A'), comb.range('a', 'z')) == A | [a-z]
	*/
	def either(ps: Parser*): Parser = ???

	/* `repeat` should return true if the input contains any number of
		repeated substrings found by `p`

		example:
			comb.repeat(comb.char('a')) == a*
	*/
	def repeat(p: Parser): Parser = ???

}