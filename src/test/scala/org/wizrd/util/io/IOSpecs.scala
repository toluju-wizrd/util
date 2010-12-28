package org.wizrd.util.io

import java.io.File
import java.io.IOException
import org.specs._

class IOSpecs extends SpecificationWithJUnit {
  // use system file system for tests
  implicit val fs = new SystemFileSystem

  "creating path from existing directory with contents" should {
    // common directory and path for these tests
    // TODO: Is there a better way to do this?
    var testDir:File = null
    var path:FilePath = null
    
    doBefore {
      // use java's built-in temp file mechanism (ugly)
      testDir = File.createTempFile("temp", "")
      if (!testDir.delete()) {
        throw new IOException("Could not delete temporary file")
      }
      if (!testDir.mkdir()) {
        throw new IOException("Could not create temporary directory")
      }

      // create some child files in this directory
      File.createTempFile("test1", ".txt", testDir)
      File.createTempFile("test2", ".bin", testDir)

      // initialize path
      path = new FilePath(testDir)
    }

    doAfter {
      def doDelete(file:File) {
        if (file.listFiles != null) {
          file.listFiles.foreach(doDelete)
        }
        file.delete()
      }

      if (testDir != null) {
        doDelete(testDir)
      }
    }

    "have name starting with 'temp'" in {
      path.name must startWith("temp")
    }

    "have path starting with testing directory's path" in {
      path.path must startWith(testDir.getAbsolutePath)
    }

    "have two children" in {
      path.children must haveSize(2)
    }

    "have expected child files" in {
      path.children must exist { c =>
        (c.name startsWith "test1") && (c.name endsWith ".txt") ||
        (c.name startsWith "test2") && (c.name endsWith ".bin")
      }
    }

    "have expected parent" in {
      path.parent must beSome(new FilePath(testDir.getParentFile))
    }

    "have type 'Directory'" in {
      path.pathType mustBe(PathType.Directory)
    }

    "fail non-recursive delete" in {
      path.delete() mustBe(false)
    }

    "succeed recursive delete" in {
      path.deleteRecursive() mustBe(true)
    }
  }
}
