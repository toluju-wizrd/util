package org.wizrd.util.guice

import com.google.inject.{Provider, Guice, Module}
import org.specs._

class ScalaGuiceSpecs extends SpecificationWithJUnit {
  def makeInstance(module:Module):Foo = {
    Guice.createInjector(module).getInstance(classOf[Foo])
  }

  "binding to class" should {
    "yield instance returning 'hello world'" in {
      val instance = makeInstance(new ScalaModule {
        override protected def configure = {
          bind[Foo].toClass[Bar]
        }
      })

      instance.hello() mustEqual "Hello World"
    }
  }

  "binding to instance" should {
    "yield instance returning 'hello world'" in {
      val instance = makeInstance(new ScalaModule {
        override protected def configure = {
          bind[Foo].toInstance(new Bar())
        }
      })

      instance.hello() mustEqual "Hello World"
    }
  }

  "binding to class without manifest" should {
    "yield instance returning 'hello world'" in {
      val instance = makeInstance(new ScalaModule {
        override protected def configure = {
          bind(classOf[Foo]).to(classOf[Bar])
        }
      })

      instance.hello() mustEqual "Hello World"
    }
  }

  "binding to provider" should {
    "yield instance returning 'hello world'" in {
      val instance = makeInstance(new ScalaModule {
        override protected def configure = {
          bind[Foo].toProviderClass[FooProvider]
        }
      })

      instance.hello() mustEqual "Hello World"
    }
  }
}

trait Foo {
  def hello():String
}

class Bar extends Foo {
  override def hello() = "Hello World"
}

class FooProvider extends Provider[Foo] {
  override def get() = new Bar()
}
