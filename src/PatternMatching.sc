/**
1. Create extractor object for email address string.
apply method should take 2 attributes: localPart and domain;
and return string in format: "localPart@domain".
unapply method should take string argument and return localPart and domain
as Some[(String, String)] or None if cannot parse string as email address.

2. Create extractor object for domain name string.
unapplySeq should return array of strings that represents domain parts.
The first element of array should be top-level domain.
For example for "extractor.scala-bootcamp.com" result should be: Some(Array("com","scala-bootcamp","extractor"))

3. Create function to filter domain strings that not contains "in.ua" part from the domains list.

4. Create function to print all email addresses that has "gmail.com" domain from the emails array.
 */

object Email {
  def apply(localPart: String, domain: String): String = {
    s"$localPart@$domain"
  }

  def unapply(email: String): Option[(String, String)] = {
    email.split("@") match {
      case Array(localPart, domain) => Some((localPart, domain))
      case _ => None
    }
  }
}

val email1 = Email("new","gmail.com")
Email.unapply(email1)

object DomainSeq {
  def apply(x: Array[String]): String = {
    x.mkString(".")
  }

  def unapplySeq(domainKey: String): Option[Array[String]] = {
    domainKey.split("[.]") match {
      case Array(part1, part2, domain) => Some(Array(domain, part2, part1))
      case _ => None
    }
  }

  def filterDomainSeq(domainKey: String): Boolean = {
    if (!domainKey.contains("in.ua")) true
    else false
  }

  def printGmail(list: Array[String]): Unit = {
    list.filter( name => name.contains("gmail.com")).foreach(println)
  }
}

val domain = DomainSeq(Array("extractor", "scala-bootcamp", "com"))
DomainSeq.unapplySeq(domain)

DomainSeq.filterDomainSeq("scala-bootcamp.in.ua")
DomainSeq.printGmail(Array("scala-bootcamp.gmail.com", "scala-bootcamp.in.ua", "test.gmail.com"))