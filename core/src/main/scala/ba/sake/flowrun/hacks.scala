package org.getshaka.nativeconverter


import scala.deriving.Mirror

// TODO delete when new version released
trait NativeConverter[T]

object NativeConverter:
  inline given derived[T](using m: Mirror.Of[T]): NativeConverter[T] = ???