package d4s

final class Rec[K <: String, +V](
  private val map: Map[String, Any]
)
object Rec {
  def apply[V](s: String)(v: V): Rec[s.type, V]                   = new Rec[s.type, V](Map(s -> v))
  def apply[K <: String with Singleton, V](kv: (K, V)): Rec[K, V] = new Rec[K, V](Map(kv))

  implicit class RecOps[R <: Rec[_, _]](private val self: R) extends AnyVal {
    def get[V](k: String)(implicit ev: R <:< Rec[k.type, V]): V           = self.map(k: k.type).asInstanceOf[V]
    def selectDynamic[V](k: String)(implicit ev: R <:< Rec[k.type, V]): V = get(k: k.type)
    def ++[R1 <: Rec[_, _]](that: R1): R with R1                          = new Rec(self.map ++ that.map).asInstanceOf[R with R1]
  }
}

object App extends App {
//  Rec("x" ->> 1)
}
