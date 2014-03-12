package utexas.aorta.common

import scala.language.experimental.macros
import scala.reflect.macros.Context

import java.io.{ObjectOutputStream, FileOutputStream, PrintWriter, File}

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

class BinaryMagicWriter(fn: String) extends MagicWriter(fn) {
  private val out = new ObjectOutputStream(new FileOutputStream(fn))
  override def done() {
    out.close()
  }
  override def int(x: Int) {
    out.writeInt(x)
  }
  override def double(x: Double) {
    out.writeDouble(x)
  }
  override def string(x: String) {
    out.writeUTF(x)
  }
  override def bool(x: Boolean) {
    out.writeBoolean(x)
  }
}

trait MagicSerializable[T] {
  def magic_save(t: T, w: MagicWriter)
}

object MagicSerializable {
  def materialize[T]: MagicSerializable[T] = macro materialize_impl[T]

  def materialize_impl[T: c.WeakTypeTag](c: Context): c.Expr[MagicSerializable[T]] = {
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
      } else if (field_type =:= typeOf[Boolean]) {
        q"w.bool(t.$name)"
      } else if (field_type <:< typeOf[Array[_]] || field_type <:< typeOf[List[_]]) {
        val type_param = field_type.asInstanceOf[TypeRef].args.head
        // TODO For some reason, quasiquoting type_param directly blows up
        val inner = type_param.toString match {
          case "utexas.aorta.sim.make.MkAgent" => q"MkAgent.do_magic_save(obj, w)"
          case "utexas.aorta.sim.make.MkIntersection" => q"MkIntersection.do_magic_save(obj, w)"
          case "utexas.aorta.common.RoadID" => q"w.int(obj.int)"
        }
        q"""
          w.int(t.$name.size)
          for (obj <- t.$name) {
            $inner
          }
        """
      } else if (field_type.toString.endsWith(".Value")) {
        // TODO hacky way to detect enumerations
        q"w.int(t.$name.id)"
      } else if (field_type.toString.endsWith("ID")) {
        // TODO hacky way to detect AnyVals...
        q"w.int(t.$name.int)"
      // TODO hacky way of getting around quasiquoting
      } else if (field_type.toString == "utexas.aorta.sim.make.SystemWalletConfig") {
        q"SystemWalletConfig.do_magic_save(t.$name, w)"
      } else if (field_type.toString == "utexas.aorta.sim.make.MkRoute") {
        q"MkRoute.do_magic_save(t.$name, w)"
      } else if (field_type.toString == "utexas.aorta.sim.make.MkWallet") {
        q"MkWallet.do_magic_save(t.$name, w)"
      } else {
        c.abort(c.enclosingPosition, s"Dunno how to serialize $field_type")
      }
    })

    val result = c.Expr[MagicSerializable[T]](q"""
      new MagicSerializable[$tpe] {
        def magic_save(t: $tpe, w: MagicWriter) {
          ..$per_field
        }
      }
    """)
    println(s"Generated serialization magic for $tpe")
    //println(result)
    result
  }
}
