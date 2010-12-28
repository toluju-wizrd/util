package org.wizrd.util

import scala.collection.mutable.HashMap

object CommandLine {
  private val delim = "--"

  type CmdHandler[A] = (Seq[String],Map[String,Seq[String]]) => A

  def parse[A](args:Array[String])(fun:PartialFunction[CommandBase,CmdHandler[A]]):A = {
    val buffer = args.toBuffer
    val flags = new HashMap[String,Seq[String]]

    while (!buffer.isEmpty) {
      val fi = buffer.indexWhere(_.startsWith(delim))

      if (fi >= 0) {
        val vsi = buffer.indexWhere(a => a.startsWith(delim) || fun.isDefinedAt(Command(a)), fi + 1)
        flags.put(buffer(fi).substring(delim.length), if (vsi >= 0) buffer.slice(fi + 1, vsi) else buffer.drop(fi + 1))
        buffer.remove(fi, if (vsi >= 0) vsi - fi else buffer.length - fi)
      }
      else {
        val cmd = Command(buffer.head)

        if (!fun.isDefinedAt(cmd)) {
          throw new IllegalArgumentException("Unknown command: " + cmd.name)
        }

        // toMap call here is in theory unnecessary, but the type system
        // complains without it
        return fun.apply(cmd)(buffer.tail.toSeq, flags.toMap)
      }
    }

    if (fun.isDefinedAt(NoCommand)) {
      return fun.apply(NoCommand)(buffer.toSeq, flags.toMap)
    }
    else {
      throw new IllegalArgumentException("Missing command")
    }
  }
}

trait CommandBase
case class Command(name:String) extends CommandBase
case object NoCommand extends CommandBase

/*

Example Usage:

object CommandLineTest {
  def main(args:Array[String]) = CommandLine.parse(args) {
    case Command("cmd") => (args, flags) =>
      println("Command 'cmd' called, with args=%s and flags=%s" format (args, flags))
    case Command("cmd2") => (args, flags) =>
      println("Command 'cmd2' called, with args=%s and flags=%s" format (args, flags))
    case NoCommand => (args, flags) =>
      println("Default command handler, with args=%s and flags=%s" format (args, flags))
  }
}

Output for above usage:

> CommandLineTest --foo bar
Default command handler, with args=ArrayBuffer() and flags=Map(foo -> ArrayBuffer(bar))
> CommandLineTest cmd --foo bar
Command 'cmd' called, with args=ArrayBuffer() and flags=Map(foo -> ArrayBuffer(bar))
> CommandLineTest cmd test --foo bar
Command 'cmd' called, with args=ArrayBuffer(test) and flags=Map(foo -> ArrayBuffer(bar))
> CommandLineTest --foo bar baz
Default command handler, with args=ArrayBuffer() and flags=Map(foo -> ArrayBuffer(bar, baz))
> CommandLineTest --foo
Default command handler, with args=ArrayBuffer() and flags=Map(foo -> ArrayBuffer())
> CommandLineTest cmd
Command 'cmd' called, with args=ArrayBuffer() and flags=Map()
> CommandLineTest cmd2
Command 'cmd2' called, with args=ArrayBuffer() and flags=Map()
> CommandLineTest cmd2 cmd
Command 'cmd2' called, with args=ArrayBuffer(cmd) and flags=Map()
> CommandLineTest cmd cmd2
Command 'cmd' called, with args=ArrayBuffer(cmd2) and flags=Map()
> CommandLineTest cmd test --foo bar
Command 'cmd' called, with args=ArrayBuffer(test) and flags=Map(foo -> ArrayBuffer(bar))
> CommandLineTest --foo bar cmd
Command 'cmd' called, with args=ArrayBuffer() and flags=Map(foo -> ArrayBuffer(bar))
> CommandLineTest --foo bar cmd test
Command 'cmd' called, with args=ArrayBuffer(test) and flags=Map(foo -> ArrayBuffer(bar))
> CommandLineTest
Default command handler, with args=ArrayBuffer() and flags=Map()
> CommandLineTest cmd --foo
Command 'cmd' called, with args=ArrayBuffer() and flags=Map(foo -> ArrayBuffer())
> CommandLineTest --bar cmd
Command 'cmd' called, with args=ArrayBuffer() and flags=Map(bar -> ArrayBuffer())
> CommandLineTest --bar cmd --foo
Command 'cmd' called, with args=ArrayBuffer() and flags=Map(foo -> ArrayBuffer(), bar -> ArrayBuffer())
> CommandLineTest --bar baz cmd --foo faz
Command 'cmd' called, with args=ArrayBuffer() and flags=Map(foo -> ArrayBuffer(faz), bar -> ArrayBuffer(baz))
> CommandLineTest --bar baz cmd --foo faz foz
Command 'cmd' called, with args=ArrayBuffer() and flags=Map(foo -> ArrayBuffer(faz, foz), bar -> ArrayBuffer(baz))
> CommandLineTest --bar baz cmd test1 test2 --foo faz foz
Command 'cmd' called, with args=ArrayBuffer(test1, test2) and flags=Map(foo -> ArrayBuffer(faz, foz), bar -> ArrayBuffer(baz))
> CommandLineTest --bar cmd test1 test2 --foo faz foz
Command 'cmd' called, with args=ArrayBuffer(test1, test2) and flags=Map(foo -> ArrayBuffer(faz, foz), bar -> ArrayBuffer())

*/
