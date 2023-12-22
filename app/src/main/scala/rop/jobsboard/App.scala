package rop.jobsboard

import org.scalajs.dom.document

import scala.scalajs.js.annotation.*

// this class will be compiled to a regular javascript and it will be referred to from javascript as module called "JobsBoardFE"
@JSExportTopLevel("JobsBoardFE")
class App {

  @JSExport // To expose this method in JS
  def doSomething(containerId: String) =
    document.getElementById(containerId).innerHTML = "This is the jobs board"
}
