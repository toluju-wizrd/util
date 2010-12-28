package org.wizrd.util.io

import scala.collection.Seq
import java.io.File

sealed class PathType private (val name:String) {
  override def toString = name
}

object PathType {
  val File = new PathType("File")
  val Directory = new PathType("Directory")
  val Link = new PathType("Link")
  val Unknown = new PathType("Unknown")

  def withName(name:String):Option[PathType] = name match {
    case "File" => Some(File)
    case "Directory" => Some(Directory)
    case "Link" => Some(Link)
    case "Unknown" => Some(Unknown)
    case _ => None
  }
}

trait Path[A,This<:Path[A,This]] extends Seq[This] { self:This =>
  val pathType:PathType
  val fs:FileSystem[This]
  val name:String
  val path:String

  def children:Seq[This] = fs.listChildren(this)
  def parent:Option[This] = fs.parent(this)
  def exists:Boolean = fs.exists(this)
  def delete():Boolean = fs.delete(this)
  def deleteRecursive():Boolean = fs.deleteRecursive(this)

  override def apply(index:Int) = children.apply(index)
  override def iterator = children.iterator
  override def length = children.length
  override def toString = path
}

class FilePath(_file:File)(implicit override val fs:FileSystem[FilePath]) extends Path[File,FilePath] {
  val file = _file.getAbsoluteFile
  override val pathType = 
    if (file.isFile) PathType.File 
    else if (file.isDirectory) PathType.Directory 
    else PathType.Unknown

  override val name = file.getName
  override val path = file.getPath
}
