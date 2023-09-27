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

    def quoted: Parser[String] = string("\"").combo(thru("\"").map(_.dropRight(1)))

    def whitespace: Parser[String] = regex("\\s*".r)

    def token2[A](p: Parser[A]): Parser[A] = p.attempt <* whitespace

    def escapedQuoted: Parser[String] = token2(quoted)

    def thru(s: String): Parser[String] = regex((".*?" + Pattern.quote(s)).r).attempt

    def lit: Parser[JSON] =
      token("null").as(JNull) |
        double.map(JNumber(_)) |
        escapedQuoted.map(x => JString(x)) |
        token("true").as(JBool(true)) |
        token("false").as(JBool(false))

    def doubleString: Parser[String] =
      token2(regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r))

    def double: Parser[Double] =
      doubleString.map(_.toDouble)


    def value: Parser[JSON] = lit | obj | array
    def kv: Parser[(String, JSON)] = (escapedQuoted <* whitespace)  ** (token(":") *> value)

    def obj: Parser[JSON] = token("{").combo(kv.sep(token(",")).map(kvs => JObject(kvs.toMap))) <* token("}")
    def array: Parser[JSON] = token("[").combo(value.sep(token(",")).map(values => JArray(values.toIndexedSeq))) <* token("]")

    whitespace *> (obj | array)

