package ziochan

import java.net.{URI as JURI}
import zio.IO
import java.net.MalformedURLException
import java.net.URL
import zio.ZIO
import java.net.URISyntaxException
import scala.reflect.ClassTag
import scala.annotation.targetName
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.net.URLDecoder
import zio.Chunk
import zio.NonEmptyChunk

final class URI private (val toJava: JURI) extends Ordered[URI] derives CanEqual:

  import URI.*

  override def equals(other: Any): Boolean = other.asInstanceOf[Matchable] match
    case uri: URI => toJava.equals(uri.toJava)
    case _        => false

  override inline def hashCode: Int = toJava.hashCode

  override inline def toString: String = toJava.toString

  override inline def compare(that: URI): Int = toJava.compareTo(that.toJava)

  inline def scheme: Option[String] = toJava.getScheme.nullOpt

  inline def isAbsolute: Boolean = toJava.isAbsolute

  inline def isOpaque: Boolean = toJava.isOpaque

  inline def schemeSpecificPart: String = toJava.getSchemeSpecificPart.nn

  inline def rawSchemeSpecificPart: String = toJava.getRawSchemeSpecificPart.nn

  inline def fragment: Option[String] = toJava.getFragment.nullOpt

  inline def rawFragment: Option[String] = toJava.getRawFragment.nullOpt

  inline def toURL: IO[MalformedURLException, URL] = ZIO.effect(toJava.toURL.nn).refineToOrDie[MalformedURLException]

  inline def parseServerAuthority: Either[URISyntaxException, URI] =
    in(fromJava(toJava.parseServerAuthority.nn)).catching[URISyntaxException]

  inline def normalizePath: URI = fromJava(toJava.normalize.nn)

  inline def resolve(uri: URI): URI = fromJava(toJava.resolve(uri.toJava).nn)

  inline def relativize(other: URI): URI = fromJava(toJava.relativize(other.toJava).nn)

object URI:

  private val unsetPortJava = -1

  opaque type Host = String

  extension (h: Host)
    def hostnameString: String = h
    def components: Vector[String] = h.split('.').toVector
    def +:(prefix: String): Host = prefix + "." + h
    def :+(suffix: String): Host = h + "." + suffix

  object Host:
    def fromString(s: String): Host = s
    def apply(first: String, components: String*): Host = (first +: components).mkString(".")

  opaque type Query = String

  extension (q: Query)
    def queryString: String = q
    def asParameters: Either[IllegalArgumentException, Chunk[(String, String)]] = foreachEither(q.split('&')) { s =>
      s.split('=')
        .toSeq
        .match
          case Seq(k, v) =>
            in(URLDecoder.decode(k, StandardCharsets.UTF_8).nn -> URLDecoder.decode(v, StandardCharsets.UTF_8).nn)
              .catching[IllegalArgumentException]
          case _ => Left(new IllegalArgumentException(s"Expected key=value, got: '$s'"))
    }
    def addParameter(name: String, value: String): Query =
      q + "&" + URLEncoder.encode(name, StandardCharsets.UTF_8) + "=" + URLEncoder.encode(name, StandardCharsets.UTF_8)
    def +(param: (String, String)): Query = addParameter(param._1, param._2)

  object Query:
    def fromString(s: String): Query = s
    def apply(params: (String, String)*): Query = params
      .map((k, v) =>
        URLEncoder.encode(k, StandardCharsets.UTF_8).nn + "=" + URLEncoder.encode(v, StandardCharsets.UTF_8).nn
      )
      .mkString("&")

  opaque type Path = NonEmptyChunk[String]

  extension (p: Path)
    def pathString: String = p.mkString("/")
    def /(s: String): Path = p :+ s
    def component(index: Int): String = p(index)
    def components: Chunk[String] = p
    def lastComponent: String = p.last

  object Path:
    def fromString(s: String): Option[Path] = s
      .split('/')
      .toList
      .match
        case Nil           => None
        case xs @ (_ :: _) => Some(NonEmptyChunk.fromCons(xs))
    def apply(first: String, rest: String*): Path = NonEmptyChunk(first, rest*)

  final case class Authority(userInfo: Option[String], host: Host, port: Option[Int]) derives CanEqual

  def fromJava(javaUri: JURI): URI = URI(javaUri)

  def parse(s: String): Either[URISyntaxException, URI] = in(fromJava(JURI(s))).catching[URISyntaxException]

  def hierarchical(
    scheme: Option[String],
    userInfo: Option[String] = None,
    host: Option[Host] = None,
    port: Option[Int] = None,
    path: Option[Path] = None,
    query: Option[Query] = None,
    fragment: Option[String] = None
  ): URI = fromJava(
    JURI(
      scheme.orNull,
      userInfo.orNull,
      host.map(_.hostnameString).orNull,
      port.getOrElse(unsetPortJava),
      path.map(_.pathString).orNull,
      query.map(_.queryString).orNull,
      fragment.orNull
    )
  )

  def absolute(
    scheme: String,
    userInfo: Option[String] = None,
    host: Option[Host] = None,
    port: Option[Int] = None,
    path: Option[Path] = None,
    query: Option[Query] = None,
    fragment: Option[String] = None
  ) = hierarchical(Some(scheme), userInfo, host, port, path, query, fragment)

  def relative(
    userInfo: Option[String] = None,
    host: Option[Host] = None,
    port: Option[Int] = None,
    path: Option[Path] = None,
    query: Option[Query] = None,
    fragment: Option[String] = None
  ): URI = hierarchical(None, userInfo, host, port, path, query, fragment)

  def opaque(scheme: Option[String], schemeSpecificPart: Option[String], fragment: Option[String]): URI = fromJava(
    JURI(scheme.orNull, schemeSpecificPart.orNull, fragment.orNull)
  )
