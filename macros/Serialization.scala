package utexas.aorta.common

import scala.language.experimental.macros
import scala.reflect.macros.Context

// TODO gotta replicate stuff here... ultimately
// 1) share serialization code in common project
// 2) just have serialization stuff here <----
class MagicWriter(fn: String) {
  def done() {}
  def int(x: Int) {}
  def double(x: Double) {}
  def string(x: String) {}
  def bool(x: Boolean) {}
}

trait Mappable[T] {
  def magic_save(t: T, w: MagicWriter)
}

object Mappable {
  implicit def materializeMappable[T]: Mappable[T] = macro materializeMappableImpl[T]

  def materializeMappableImpl[T: c.WeakTypeTag](c: Context): c.Expr[Mappable[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val fields = tpe.declarations.collectFirst({
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }).get.paramss.head

    val per_field: List[Apply] = fields.map(field => {
      val name = field.name
      val decoded = name.decoded
      val returnType = tpe.declaration(name).typeSignature

      q"w.string(t.$name.toString)"
    })

    val result = c.Expr[Mappable[T]] { q"""
      new Mappable[$tpe] {
        def magic_save(t: $tpe, w: MagicWriter) {
          ..$per_field
          val result = "foo"
          println(result)
        }
      }
    """ }
    println(result)
    result
  }
}
