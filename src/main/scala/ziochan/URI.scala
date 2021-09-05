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

opaque type URI = JURI

given CanEqual[URI, URI] = CanEqual.derived

extension (u: JURI)
  def scheme: Option[String] = u.getScheme.nullOpt

  def isAbsolute: Boolean = u.isAbsolute

  def isOpaque: Boolean = u.isOpaque

  def schemeSpecificPart: String = u.getSchemeSpecificPart.nn

  def rawSchemeSpecificPart: String = u.getRawSchemeSpecificPart.nn

  def fragment: Option[String] = u.getFragment.nullOpt

  def rawFragment: Option[String] = u.getRawFragment.nullOpt

  def parseServerAuthority: Either[URISyntaxException, URI] =
    in(u.parseServerAuthority.nn).catching[URISyntaxException]

  def normalizePath: URI = u.normalize.nn

  def resolve(uri: URI): URI = u.resolve(uri).nn

  def relativize(other: URI): URI = u.relativize(other).nn

  def toURL: IO[MalformedURLException, URL] = ZIO.effect(u.toURL.nn).refineToOrDie[MalformedURLException]

  def authority: Option[String] = u.getAuthority.nullOpt

  def rawAuthority: Option[String] = u.getRawAuthority.nullOpt

  def userInfo: Option[String] = u.getUserInfo.nullOpt

  def rawUserInfo: Option[String] = u.getRawUserInfo.nullOpt

  def host: Option[URI.Host] = u.getHost.nullOpt.map(URI.Host.fromString)

  def port: Option[Int] = u.getPort.match
    case URI.unsetPortJava => None
    case other             => Some(other)

  def path: Option[URI.Path] = u.getPath.nullOpt.flatMap(URI.Path.fromString)

  def rawPath: Option[String] = u.getRawPath.nullOpt

  def query: Option[URI.Query] = u.getQuery.nullOpt.map(URI.Query.fromString)

  def rawQuery: Option[String] = u.getRawQuery.nullOpt

  def asciiString: String = u.toASCIIString.nn

  def copyOpaque(
    scheme: Option[String] = None,
    schemeSpecificPart: Option[String] = None,
    fragment: Option[String] = None
  ): URI = URI.opaque(
    scheme = scheme.orElse(u.scheme),
    schemeSpecificPart = Some(schemeSpecificPart.getOrElse(u.schemeSpecificPart)),
    fragment = fragment.orElse(u.fragment)
  )

  def copy(
    scheme: Option[String] = None,
    userInfo: Option[String] = None,
    host: Option[URI.Host] = None,
    port: Option[Int] = None,
    path: Option[URI.Path] = None,
    query: Option[URI.Query] = None,
    fragment: Option[String] = None
  ): URI = URI.hierarchical(
    scheme = scheme.orElse(u.scheme),
    userInfo = userInfo.orElse(u.userInfo),
    host = host.orElse(u.host),
    port = port.orElse(u.port),
    path = path.orElse(u.path),
    query = query.orElse(u.query),
    fragment = fragment.orElse(u.fragment)
  )

  def withScheme(scheme: String): URI = copy(scheme = Some(scheme))

  def withHost(host: URI.Host): URI = copy(host = Some(host))

  def withHostString(host: String): URI = withHost(URI.Host.fromString(host))

  def withPath(path: URI.Path): URI = copy(path = Some(path))

  def updatePath(f: URI.Path => URI.Path): URI = withPath(f(path.getOrElse(URI.Path("."))))

end extension

object URI:

  private[ziochan] val unsetPortJava = -1

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

  def fromJava(javaUri: JURI): URI = javaUri

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
