package org.wizrd.util

import scala.collection.mutable.HashMap
import org.specs._

class CommandLineSpecs extends SpecificationWithJUnit {
  def dummyMain(args:Array[String]):(String, Seq[String], Map[String,Seq[String]]) = CommandLine.parse(args) {
    case Command("cmd") => (args, flags) => ("cmd", args, flags)
    case Command("cmd2") => (args, flags) => ("cmd2", args, flags)
    case NoCommand => (args, flags) => ("nocmd", args, flags)
  }

  "command line arguments '--foo bar'" should {
    val (command, args, flags) = dummyMain("--foo bar" split " ")

    "have command be 'nocmd'" in { command mustEqual("nocmd") }
    "have no arguments" in { args must beEmpty }
    "have one flag" in { flags must haveSize(1) }
    "have a flag 'foo' with value 'bar'" in { 
      flags must haveKey("foo") 
      flags("foo") must contain("bar") 
    }
  }

  "command line arguments 'cmd --foo bar'" should {
    val (command, args, flags) = dummyMain("cmd --foo bar" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have no arguments" in { args must beEmpty }
    "have one flag" in { flags must haveSize(1) }
    "have a flag 'foo' with value 'bar'" in { 
      flags must haveKey("foo")
      flags("foo") must contain("bar")
    }
  }

  "command line arguments 'cmd test --foo bar'" should {
    val (command, args, flags) = dummyMain("cmd test --foo bar" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have one argument" in { args must haveSize(1) }
    "have an argument with value 'test'" in { args must contain("test") }
    "have one flag" in { flags must haveSize(1) }
    "have a flag 'foo' with value 'bar'" in { 
      flags must haveKey("foo")
      flags("foo") must contain("bar")
    }
  }

  "command line arguments '--foo bar baz'" should {
    val (command, args, flags) = dummyMain("--foo bar baz" split " ")

    "have command be 'nocmd'" in { command mustEqual("nocmd") }
    "have no arguments" in { args must beEmpty }
    "have one flag" in { flags must haveSize(1) }
    "have a flag 'foo' with values 'bar' and 'baz'" in { 
      flags must haveKey("foo")
      flags("foo") must containAll(Seq("bar", "baz"))
    }
  }

  "command line arguments '--foo'" should {
    val (command, args, flags) = dummyMain("--foo" split " ")

    "have command be 'nocmd'" in { command mustEqual("nocmd") }
    "have no arguments" in { args must beEmpty }
    "have one flag" in { flags must haveSize(1) }
    "have a flag 'foo' with no values" in { flags("foo") must beEmpty }
  }

  "command line arguments 'cmd'" should {
    val (command, args, flags) = dummyMain("cmd" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have no arguments" in { args must beEmpty }
    "have no flags" in { flags must beEmpty }
  }

  "command line arguments 'cmd2'" should {
    val (command, args, flags) = dummyMain("cmd2" split " ")

    "have command be 'cmd2'" in { command mustEqual("cmd2") }
    "have no arguments" in { args must beEmpty }
    "have no flags" in { flags must beEmpty }
  }

  "command line arguments 'cmd2 cmd'" should {
    val (command, args, flags) = dummyMain("cmd2 cmd" split " ")

    "have command be 'cmd2'" in { command mustEqual("cmd2") }
    "have one argument" in { args must haveSize(1) }
    "have an argument with value 'cmd'" in { args must contain("cmd") }
    "have no flags" in { flags must beEmpty }
  }

  "command line arguments 'cmd cmd2'" should {
    val (command, args, flags) = dummyMain("cmd cmd2" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have one argument" in { args must haveSize(1) }
    "have an argument with value 'cmd2'" in { args must contain("cmd2") }
    "have no flags" in { flags must beEmpty }
  }

  "command line arguments '--foo bar cmd'" should {
    val (command, args, flags) = dummyMain("--foo bar cmd" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have no arguments" in { args must beEmpty }
    "have one flag" in { flags must haveSize(1) }
    "have a flag 'foo' with value 'bar'" in { 
      flags must haveKey("foo")
      flags("foo") must contain("bar")
    }
  }

  "command line arguments '--foo bar cmd test'" should {
    val (command, args, flags) = dummyMain("--foo bar cmd test" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have one argument" in { args must haveSize(1) }
    "have an argument with value 'test'" in { args must contain("test") }
    "have one flag" in { flags must haveSize(1) }
    "have a flag 'foo' with value 'bar'" in { 
      flags must haveKey("foo")
      flags("foo") must contain("bar")
    }
  }

  "command line arguments ''" should {
    val (command, args, flags) = dummyMain(Array.empty[String])

    "have command be 'nocmd'" in { command mustEqual("nocmd") }
    "have no arguments" in { args must beEmpty }
    "have no flags" in { flags must beEmpty }
  }

  "command line arguments 'cmd --foo'" should {
    val (command, args, flags) = dummyMain("cmd --foo" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have no arguments" in { args must beEmpty }
    "have one flag" in { flags must haveSize(1) }
    "have a flag 'foo' with no values" in { 
      flags must haveKey("foo")
      flags("foo") must beEmpty 
    }
  }

  "command line arguments '--bar cmd'" should {
    val (command, args, flags) = dummyMain("--bar cmd" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have no arguments" in { args must beEmpty }
    "have one flag" in { flags must haveSize(1) }
    "have a flag 'bar' with no values" in { flags("bar") must beEmpty }
  }

  "command line arguments '--bar cmd --foo'" should {
    val (command, args, flags) = dummyMain("--bar cmd --foo" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have no arguments" in { args must beEmpty }
    "have two flags" in { flags must haveSize(2) }
    "have a flag 'foo' with no values" in { flags("foo") must beEmpty }
    "have a flag 'bar' with no values" in { flags("bar") must beEmpty }
  }

  "command line arguments '--bar baz cmd --foo faz'" should {
    val (command, args, flags) = dummyMain("--bar baz cmd --foo faz" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have no arguments" in { args must beEmpty }
    "have two flags" in { flags must haveSize(2) }
    "have a flag 'foo' with value 'faz'" in { 
      flags must haveKey("foo")
      flags("foo") must contain("faz")
    }
    "have a flag 'bar' with value 'baz'" in { 
      flags must haveKey("bar")
      flags("bar") must contain("baz")
    }
  }

  "command line arguments '--bar baz cmd --foo faz foz'" should {
    val (command, args, flags) = dummyMain("--bar baz cmd --foo faz foz" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have no arguments" in { args must beEmpty }
    "have two flags" in { flags must haveSize(2) }
    "have a flag 'foo' with values 'faz' and 'foz'" in { 
      flags must haveKey("foo")
      flags("foo") must containAll(Seq("faz", "foz")) 
    }
    "have a flag 'bar' with value 'baz'" in { 
      flags must haveKey("bar")
      flags("bar") must contain("baz")
    }
  }

  "command line arguments '--bar baz cmd test1 test2 --foo faz foz'" should {
    val (command, args, flags) = dummyMain("--bar baz cmd test1 test2 --foo faz foz" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have two arguments" in { args must haveSize(2) }
    "have an argument 'test1'" in { args must contain("test1") }
    "have an argument 'test2'" in { args must contain("test2") }
    "have two flags" in { flags must haveSize(2) }
    "have a flag 'foo' with values 'faz' and 'foz'" in { 
      flags must haveKey("foo")
      flags("foo") must containAll(Seq("faz", "foz"))
    }
    "have a flag 'bar' with value 'baz'" in { 
      flags must haveKey("bar")
      flags("bar") must contain("baz")
    }
  }

  "command line arguments '--bar cmd test1 test2 --foo faz foz'" should {
    val (command, args, flags) = dummyMain("--bar cmd test1 test2 --foo faz foz" split " ")

    "have command be 'cmd'" in { command mustEqual("cmd") }
    "have two arguments" in { args must haveSize(2) }
    "have an argument 'test1'" in { args must contain("test1") }
    "have an argument 'test2'" in { args must contain("test2") }
    "have two flags" in { flags must haveSize(2) }
    "have a flag 'foo' with values 'faz' and 'foz'" in { 
      flags must haveKey("foo")
      flags("foo") must contain("foz") 
    }
    "have a flag 'bar' with no values" in { flags("bar") must beEmpty }
  }
}
