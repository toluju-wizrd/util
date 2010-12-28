package org.wizrd.util.guice

import com.google.inject.AbstractModule
import com.google.inject.Provider
import com.google.inject.TypeLiteral
import com.google.inject.binder.AnnotatedBindingBuilder
import com.google.inject.binder.AnnotatedConstantBindingBuilder
import com.google.inject.binder.LinkedBindingBuilder
import com.google.inject.binder.ScopedBindingBuilder
import com.google.inject.util.Types
import java.lang.annotation.Annotation

/**
 * Taken from http://github.com/sptz45/sse-guice, modified for my purposes.
 * License: Apache 2.0
 */

abstract class ScalaModule extends AbstractModule {
  protected def bind[T](implicit m:Manifest[T]):AnnotatedBindingBuilder[T] = {
    m.typeArguments match {
      case Nil => bind(m.erasure.asInstanceOf[Class[T]])
      case _   => bind(Helpers.typeLiteral(m))
    }
  }

  implicit def annotatedBindingBuilderWrapper[T](b:AnnotatedBindingBuilder[T]) =
    new RichAnnotatedBindingBuilder[T](b)

  implicit def linkedBindingBuilderWrapper[T](b:LinkedBindingBuilder[T]) =
    new RichLinkedBindingBuilder[T](b)

  implicit def annotatedConstantBindingBuilderWrapper(b:AnnotatedConstantBindingBuilder) =
    new RichAnnotatedConstantBindingBuilder(b)

  implicit def scopedBindingBuilderWrapper(b:ScopedBindingBuilder) =
    new RichScopedBindingBuilder(b)
}

class RichAnnotatedBindingBuilder[T](override val builder:AnnotatedBindingBuilder[T])
      extends RichLinkedBindingBuilder[T](builder) {
  def annotatedWithClass[A<:Annotation](implicit a:Manifest[A]) =
    builder.annotatedWith(a.erasure.asInstanceOf[Class[A]])
}

class RichAnnotatedConstantBindingBuilder(val builder:AnnotatedConstantBindingBuilder) {
  def annotatedWithClass[A<:Annotation](implicit a:Manifest[A]) =
    builder.annotatedWith(a.erasure.asInstanceOf[Class[A]])
}

class RichLinkedBindingBuilder[T](val builder:LinkedBindingBuilder[T]) {
  def toClass[I<:T](implicit i:Manifest[I]) = {
    if (i.typeArguments.isEmpty)
      builder.to(i.erasure.asInstanceOf[Class[I]])
    else
      builder.to(Helpers.typeLiteral(i))
  }

  def toProviderClass[P<:Provider[_<:T]](implicit p:Manifest[P]) =
    builder.toProvider(p.erasure.asInstanceOf[Class[P]])
}

class RichScopedBindingBuilder(val builder:ScopedBindingBuilder) {
  def inScopeClass[A<:Annotation](implicit ann:Manifest[A]) =
    builder.in(ann.erasure.asInstanceOf[Class[A]])
}

object Helpers {
  def typeLiteral[A](implicit a:Manifest[A]):TypeLiteral[A] = {
    val targs = a.typeArguments.map(_.erasure)
    TypeLiteral.get(Types.newParameterizedType(a.erasure, targs:_*)).asInstanceOf[TypeLiteral[A]]
  }
}
