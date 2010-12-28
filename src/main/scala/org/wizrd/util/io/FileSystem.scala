package org.wizrd.util.io

import scala.collection.Seq
import java.io.File

trait FileSystem[A<:Path[_,A]] {
  def listChildren(path:A):Seq[A]
  def parent(path:A):Option[A]
  def exists(path:A):Boolean
  def delete(path:A):Boolean

  def deleteRecursive(path:A):Boolean = {
    if (exists(path)) {
      val r = listChildren(path).foldLeft(true)((l,r) => l && deleteRecursive(r))
      if (r) delete(path) else false
    }
    else {
      false
    }
  }
}

class SystemFileSystem extends FileSystem[FilePath] {
  private def makePath(file:File) = new FilePath(file)(this)

  override def listChildren(path:FilePath) = 
    Option(path.file.listFiles).toSeq.flatMap(_.toSeq.map(makePath))

  override def parent(path:FilePath) = 
    Option(path.file.getParentFile).map(makePath)

  override def exists(path:FilePath) = path.file.exists

  override def delete(path:FilePath) = path.file.delete()
}
