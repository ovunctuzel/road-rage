package utexas.aorta.common

import scala.language.experimental.macros
import scala.reflect.macros.Context

import java.io.{ObjectOutputStream, ObjectInputStream, FileOutputStream, FileInputStream,
                PrintWriter, File}

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

abstract class MagicReader(fn: String) {
  def int: Int
  def double: Double
  def string: String
  def bool: Boolean
}

class BinaryMagicReader(fn: String) extends MagicReader(fn) {
  private val in = new ObjectInputStream(new FileInputStream(fn))
  override def int = in.readInt
  override def double = in.readDouble
  override def string = in.readUTF
  override def bool = in.readBoolean
}

trait MagicSerializable[T] {
  def magic_save(t: T, w: MagicWriter)
  def magic_load(r: MagicReader): T
}

object MagicSerializable {
  // TODO this is called several times. lazy val?
  def materialize[T]: MagicSerializable[T] = macro materialize_impl[T]

  def materialize_impl[T: c.WeakTypeTag](c: Context): c.Expr[MagicSerializable[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val companion = tpe.typeSymbol.companionSymbol

    val fields = tpe.declarations.collectFirst({
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }).get.paramss.head

    val both_per_field = fields.map(field => {
      val name = field.name
      // All of the fields are methods () => Something
      val field_type = tpe.declaration(name).asMethod.returnType

      if (field_type =:= typeOf[String]) {
        (q"w.string(t.$name)", q"r.string")
      } else if (field_type =:= typeOf[Double]) {
        (q"w.double(t.$name)", q"r.double")
      } else if (field_type =:= typeOf[Int]) {
        (q"w.int(t.$name)", q"r.int")
      } else if (field_type =:= typeOf[Boolean]) {
        (q"w.bool(t.$name)", q"r.bool")
      } else if (field_type <:< typeOf[Array[_]]) {
        val type_param = field_type.asInstanceOf[TypeRef].args.head.typeSymbol.companionSymbol
        (q"""
          w.int(t.$name.size)
          for (obj <- t.$name) {
            $type_param.do_magic_save(obj, w)
          }
        """, q"Range(0, r.int).map(_ => $type_param.do_magic_load(r)).toArray")
      } else if (field_type.toString.endsWith(".Value")) {
        // TODO hacky way to detect/handle enumerations
        val read = field_type.toString match {
          case "utexas.aorta.sim.make.IntersectionType.Value" => q"IntersectionType(r.int)"
          case "utexas.aorta.sim.make.RouterType.Value" => q"RouterType(r.int)"
          case "utexas.aorta.sim.make.OrderingType.Value" => q"OrderingType(r.int)"
          case "utexas.aorta.sim.make.WalletType.Value" => q"WalletType(r.int)"
          case "utexas.aorta.sim.make.CongestionType.Value" => q"CongestionType(r.int)"
          case "utexas.aorta.sim.make.ReroutePolicyType.Value" => q"ReroutePolicyType(r.int)"
          case "utexas.aorta.map.Direction.Value" => q"Direction(r.int)"
        }
        (q"w.int(t.$name.id)", read)
      } else {
        val type_param = field_type.typeSymbol.companionSymbol
        (q"$type_param.do_magic_save(t.$name, w)", q"$type_param.do_magic_load(r)")
      }
    })
    val write_per_field = both_per_field.map(_._1)
    val read_per_field = both_per_field.map(_._2)

    val result = c.Expr[MagicSerializable[T]](q"""
      new MagicSerializable[$tpe] {
        def magic_save(t: $tpe, w: MagicWriter) {
          ..$write_per_field
        }
        def magic_load(r: MagicReader): $tpe = new $tpe(..$read_per_field)
      }
    """)
    println(s"Generated serialization magic for $tpe")
    //println(result)
    result
  }
}
