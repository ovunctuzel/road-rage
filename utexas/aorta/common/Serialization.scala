// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common

import java.io.{FileWriter, Serializable}
import scala.io.Source

import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream, PrintWriter, BufferedReader, FileReader, File}

// TODO make stats, maps, scenarios -- everything -- use these.
abstract class StateWriter(fn: String) {
  def done()
  def int(x: Int)
  def double(x: Double)
  def string(x: String)
  def bool(x: Boolean)
  def long(x: Long)
  def obj(x: Any)   // TODO remove.

  // TODO a macro to do lots of these in one line?
}

class BinaryStateWriter(fn: String) extends StateWriter(fn) {
  private val out = new ObjectOutputStream(new FileOutputStream(fn))
  def done() {
    out.close()
  }

  def int(x: Int) {
    out.writeInt(x)
  }
  def double(x: Double) {
    out.writeDouble(x)
  }
  def string(x: String) {
    out.writeUTF(x)
  }
  def bool(x: Boolean) {
    out.writeBoolean(x)
  }
  def long(x: Long) {
    out.writeLong(x)
  }
  def obj(x: Any) {
    out.writeObject(x)
  }
}

class StringStateWriter(fn: String) extends StateWriter(fn) {
  private val out = new PrintWriter(fn)
  def done() {
    out.close()
  }

  def int(x: Int) {
    out.println(x)
  }
  def double(x: Double) {
    out.println(x)
  }
  def string(x: String) {
    out.println(x)
  }
  def bool(x: Boolean) {
    out.println(x)
  }
  def long(x: Long) {
    out.println(x)
  }
  def obj(x: Any) {
    out.println(x)
  }
}

abstract class StateReader(fn: String) {
  def int: Int
  def double: Double
  def string: String
  def bool: Boolean
  def long: Long
  def obj: Any
}

class BinaryStateReader(fn: String) extends StateReader(fn) {
  private val in = new ObjectInputStream(new FileInputStream(fn))
  def int = in.readInt
  def double = in.readDouble
  def string = in.readUTF
  def bool = in.readBoolean
  def long = in.readLong
  def obj = in.readObject
}

class StringStateReader(fn: String) extends StateReader(fn) {
  private val in = new BufferedReader(new FileReader(fn))
  def int = in.readLine.toInt
  def double = in.readLine.toDouble
  def string = in.readLine
  def bool = in.readLine.toBoolean
  def long = in.readLine.toLong
  def obj: Any = {
    in.readLine
    return null
  }
}
