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

    val per_field = fields.map(field => {
      val name = field.name
      // All of the fields are methods () => Something
      val field_type = tpe.declaration(name).asMethod.returnType

      if (field_type =:= typeOf[String]) {
        q"w.string(t.$name)"
      } else if (field_type =:= typeOf[Double]) {
        q"w.double(t.$name)"
      } else if (field_type =:= typeOf[Int]) {
        q"w.int(t.$name)"
      } else if (field_type <:< typeOf[Array[_]]) {
        val type_param = field_type.asInstanceOf[TypeRef].args.head
        q"w.int(t.$name.size)"
        /*q"""
          w.int(t.$name.size)
          for (obj <- t.$name) {
            $type_param.do_magic(obj, w)
          }
        """*/
      } else {
        // TODO Enumerations are hard to detect
        println(s"$name is $field_type")
        q"w.string(t.$name.toString)"
      }
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
