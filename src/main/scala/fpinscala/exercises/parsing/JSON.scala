package fpinscala.exercises.parsing

import java.util.regex.Pattern

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import P.*

    def token(s: String) = string(s).attempt <* whitespace

    def foo = string("").map(identity)

    def jString = whitespace.map(x => JString(x))
    def quoted: Parser[String] = string("\"").combo(thru("\"").map(_.dropRight(1)))

    def whitespace: Parser[String] = regex("\\s*".r)

    def token2[A](p: Parser[A]): Parser[A] = p.attempt <* whitespace

    def escapedQuoted: Parser[String] = token2(quoted)

    def thru(s: String): Parser[String] = regex((".*?" + Pattern.quote(s)).r)

    def lit: Parser[JSON] = (
      token("null").as(JNull) |
        double.map(JNumber(_)) |
        jString |
        token("true").as(JBool(true)) |
        token("false").as(JBool(false))
      )


    def doubleString: Parser[String] =
      token2(regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r))

    /** Floating point literals, converted to a `Double`. */
    def double: Parser[Double] =
      doubleString.map(_.toDouble)

    lit

