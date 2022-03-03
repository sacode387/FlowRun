package dev.sacode.flowrun
package edit

import java.util.UUID
import scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all.{name => _, *}
import reactify.*
import dev.sacode.flowrun.parse.*
import dev.sacode.flowrun.toastify.*
import dev.sacode.flowrun.ast.*, Statement.*

/** Editor for selected statement or function signature. */
final class StatementEditor(
    programModel: ProgramModel,
    flowrunChannel: Channel[FlowRun.Event],
    flowRunElements: FlowRunElements
) {

  def edit(stmtId: String): Unit = {

    val stmt = programModel.findStatement(stmtId)

    val stmtElem = div(cls := "flowrun-stmt-inputs")

    stmt match {
      case _: Begin =>
        if currFun.isMain then Toastify(ToastifyOptions("Begin is not editable.", Color.yellow)).showToast()
        else {
          val nameInputElem = newNameInput(10, currFun.name, "myFun") { newName =>
            programModel.updateFunction(stmtId, name = Some(newName))
          }
          val typeInputElem = newTypeInput(Expression.Type.values.toSeq, currFun.tpe) { newType =>
            programModel.updateFunction(stmtId, tpe = Some(newType))
          }

          val paramsListElem = div().render
          val addParamElem = flowRunElements.addParamButton
          addParamElem.onclick = _ => {
            val newParam =
              Function.Parameter(UUID.randomUUID().toString, s"p${getParams().length + 1}", Expression.Type.Integer)
            val newParams = getParams() ++ List(newParam)
            val paramNameInput = getParamNameInput(stmtId, newParam)
            val paramTpeInput = getParamTpeInput(stmtId, newParam)
            val paramDeleteBtn = getParamDeleteBtn(stmtId, newParam)

            paramsListElem.appendChild(
              div(id := newParam.id, cls := "flowrun-param-inputs")(
                paramNameInput,
                paramTpeInput,
                paramDeleteBtn
              ).render
            )
            paramNameInput.focus()
            programModel.updateFunction(stmtId, parameters = Some(newParams))
          }

          getParams().foreach { param =>
            val paramNameInput = getParamNameInput(stmtId, param)
            val paramTpeInput = getParamTpeInput(stmtId, param)
            val paramDeleteBtn = getParamDeleteBtn(stmtId, param)
            paramsListElem.appendChild(
              div(id := param.id, cls := "flowrun-param-inputs")(paramNameInput, paramTpeInput, paramDeleteBtn).render
            )
          }

          flowRunElements.stmtOutput.innerText = ""
          flowRunElements.stmtOutput.appendChild(
            frag(
              stmtElem(
                nameInputElem,
                typeInputElem
              ),
              div(br, small("Parameters:")),
              paramsListElem,
              div(addParamElem)
            ).render
          )
          nameInputElem.focus()
        }

      case statement: Return =>
        if currFun.isMain then Toastify(ToastifyOptions("End is not editable.", Color.yellow)).showToast()
        else if currFun.tpe == Expression.Type.Void then
          Toastify(ToastifyOptions("Void function does not return any value.", Color.yellow)).showToast()
        else
          val exprInputElem = newExprInput(statement.id, 10, statement.maybeValue.getOrElse(""), "x + 1")(
            newExprText => {
              val updatedStmt = statement.copy(maybeValue = Some(newExprText))
              programModel.updateStmt(updatedStmt)
            },
            (e, newExprText) => {
              if newExprText.isEmpty then
                val updatedStmt = statement.copy(maybeValue = None)
                programModel.updateStmt(updatedStmt)
              else flowrunChannel := FlowRun.Event.SyntaxError(e.getMessage)
            }
          )
          flowRunElements.stmtOutput.innerText = ""
          flowRunElements.stmtOutput.appendChild(exprInputElem)
          exprInputElem.focus()

      case statement: Declare =>
        val nameInputElem = newNameInput(10, statement.name, "x") { newName =>
          val updatedStmt = statement.copy(name = newName)
          programModel.updateStmt(updatedStmt)
        }
        val typeInputElem = newTypeInput(Expression.Type.VarTypes.toSeq, statement.tpe) { newType =>
          val updatedStmt = statement.copy(tpe = newType)
          programModel.updateStmt(updatedStmt)
        }
        val exprInputElem = newExprInput(statement.id, 10, statement.initValue.getOrElse(""), "123")(
          newExprText => {
            val updatedStmt = statement.copy(initValue = Some(newExprText))
            programModel.updateStmt(updatedStmt)
          },
          (e, newExprText) => {
            if newExprText.isEmpty then
              val updatedStmt = statement.copy(initValue = None)
              programModel.updateStmt(updatedStmt)
            else flowrunChannel := FlowRun.Event.SyntaxError(e.getMessage)
          }
        )

        flowRunElements.stmtOutput.innerText = ""
        flowRunElements.stmtOutput.appendChild(
          stmtElem(
            nameInputElem,
            typeInputElem,
            span(" = "),
            exprInputElem
          ).render
        )
        nameInputElem.focus()

      case statement: Input =>
        val nameInputElem = newNameInput(10, statement.name, "x") { newName =>
          val updatedStmt = statement.copy(name = newName)
          programModel.updateStmt(updatedStmt)
        }
        flowRunElements.stmtOutput.innerText = ""
        flowRunElements.stmtOutput.appendChild(nameInputElem)
        nameInputElem.focus()

      case statement: Output =>
        val exprInputElem = newExprInput(statement.id, 30, statement.value, "\"Hello!\"")(newExprText => {
          val updatedStmt = statement.copy(value = newExprText)
          programModel.updateStmt(updatedStmt)
        })
        flowRunElements.stmtOutput.innerText = ""
        flowRunElements.stmtOutput.appendChild(exprInputElem)
        exprInputElem.focus()

      case statement: Assign =>
        val nameInputElem = newNameInput(10, statement.name, "x") { newName =>
          val updatedStmt = statement.copy(name = newName)
          programModel.updateStmt(updatedStmt)
        }
        val exprInputElem = newExprInput(statement.id, 10, statement.value, "x + 1")(newExprText => {
          val updatedStmt = statement.copy(value = newExprText)
          programModel.updateStmt(updatedStmt)
        })
        flowRunElements.stmtOutput.innerText = ""
        flowRunElements.stmtOutput.appendChild(
          stmtElem(
            nameInputElem,
            span(" = "),
            exprInputElem
          ).render
        )
        exprInputElem.focus()

      case statement: Call =>
        val exprInputElem = newExprInput(statement.id, 10, statement.value, "fun1()")(newExprText => {
          val updatedStmt = statement.copy(value = newExprText)
          programModel.updateStmt(updatedStmt)
        })
        flowRunElements.stmtOutput.innerText = ""
        flowRunElements.stmtOutput.appendChild(exprInputElem)
        exprInputElem.focus()

      case statement: If =>
        val exprInputElem = newExprInput(statement.id, 10, statement.condition, "true")(newExprText => {
          val updatedStmt = statement.copy(condition = newExprText)
          programModel.updateStmt(updatedStmt)
        })
        flowRunElements.stmtOutput.innerText = ""
        flowRunElements.stmtOutput.appendChild(exprInputElem)
        exprInputElem.focus()

      case statement: While =>
        val exprInputElem = newExprInput(statement.id, 10, statement.condition, "true")(newExprText => {
          val updatedStmt = statement.copy(condition = newExprText)
          programModel.updateStmt(updatedStmt)
        })
        flowRunElements.stmtOutput.innerText = ""
        flowRunElements.stmtOutput.appendChild(exprInputElem)
        exprInputElem.focus()

      case statement: DoWhile =>
        val exprInputElem = newExprInput(statement.id, 10, statement.condition, "true")(newExprText => {
          val updatedStmt = statement.copy(condition = newExprText)
          programModel.updateStmt(updatedStmt)
        })
        flowRunElements.stmtOutput.innerText = ""
        flowRunElements.stmtOutput.appendChild(exprInputElem)
        exprInputElem.focus()

      case statement: ForLoop =>
        val nameInputElem = newNameInput(3, statement.varName, "x") { newName =>
          val updatedStmt = statement.copy(varName = newName)
          programModel.updateStmt(updatedStmt)
        }
        val startInputElem = newExprInput(statement.id, 3, statement.start, "0")(newExprText => {
          val updatedStmt = statement.copy(start = newExprText)
          programModel.updateStmt(updatedStmt)
        })
        val toInputElem = newExprInput(statement.id, 3, statement.end, "10")(newExprText => {
          val updatedStmt = statement.copy(end = newExprText)
          programModel.updateStmt(updatedStmt)
        })
        val byInputElem = newExprInput(statement.id, 3, statement.incr, "1")(newExprText => {
          val updatedStmt = statement.copy(incr = newExprText)
          programModel.updateStmt(updatedStmt)
        })
        flowRunElements.stmtOutput.innerText = ""
        flowRunElements.stmtOutput.appendChild(
          stmtElem(
            nameInputElem,
            span(" = "),
            startInputElem,
            span(" to "),
            toInputElem,
            span(" by "),
            byInputElem
          ).render
        )
        nameInputElem.focus()

      case _ => ()
    }
  }

  private def currFun = programModel.currentFunction

  private def getParams(): List[Function.Parameter] =
    programModel.currentFunction.parameters

  private def newNameInput(size: Int, value: String, placeHolder: String)(onSuccess: String => Unit): dom.html.Input = {
    val newInput = flowRunElements.newInputText(size)
    newInput.value = value
    newInput.placeholder = placeHolder
    newInput.oninput = { (_: dom.Event) =>
      val newName = newInput.value.trim
      val errorMsg: Option[String] = NameUtils.validateIdentifier(newName)
      errorMsg match
        case None =>
          onSuccess(newName)
        case Some(msg) =>
          flowrunChannel := FlowRun.Event.SyntaxError(msg)
    }
    newInput
  }

  private def newExprInput(nodeId: String, size: Int, value: String, placeHolder: String)(
      onSuccess: String => Unit
  ): dom.html.Input =
    newExprInput(nodeId, size, value, placeHolder)(
      onSuccess,
      (e, newExprText) => flowrunChannel := FlowRun.Event.SyntaxError(e.getMessage)
    )

  private def newExprInput(nodeId: String, size: Int, value: String, placeHolder: String)(
      onSuccess: String => Unit,
      onFailure: (Throwable, String) => Unit
  ): dom.html.Input = {
    val newInput = flowRunElements.newInputText(size)
    newInput.value = value
    newInput.placeholder = placeHolder
    newInput.oninput = { (_: dom.Event) =>
      import scala.util.*
      val newExprText = newInput.value.trim
      val maybeNewExpr = Try(parseExpr(nodeId, newExprText))
      maybeNewExpr match {
        case Success(_) => onSuccess(newExprText)
        case Failure(e) => onFailure(e, newExprText)
      }
    }
    newInput
  }

  private def newTypeInput(types: Seq[Expression.Type], selectedTpe: Expression.Type)(
      onSuccess: Expression.Type => Unit
  ): dom.html.Select = {
    val newInput = flowRunElements.newInputSelect
    types.foreach { tpe =>
      val typeItem = option(value := tpe.toString)(tpe.toString).render
      newInput.add(typeItem)
    }
    newInput.value = selectedTpe.toString
    newInput.onchange = { (e: dom.Event) =>
      val thisElem = e.target.asInstanceOf[dom.html.Select]
      val newType = Expression.Type.valueOf(thisElem.value)
      onSuccess(newType)
    }
    newInput
  }

  private def getParamNameInput(
      stmtId: String,
      param: Function.Parameter
  ) = newNameInput(10, param.name, "x1") { newName =>
    val params = getParams()
    val newParams = params.map { p =>
      if p.id == param.id then p.copy(name = newName) else p
    }
    programModel.updateFunction(stmtId, parameters = Some(newParams))
  }

  private def getParamTpeInput(
      stmtId: String,
      param: Function.Parameter
  ) =
    newTypeInput(Expression.Type.VarTypes.toSeq, param.tpe) { newType =>
      val params = getParams()
      val newParams = params.map { p =>
        if p.id == param.id then p.copy(tpe = newType) else p
      }
      programModel.updateFunction(stmtId, parameters = Some(newParams))
    }

  private def getParamDeleteBtn(
      stmtId: String,
      param: Function.Parameter
  ) = {
    val paramNameInput = flowRunElements.newDeleteParamButton
    paramNameInput.onclick = _ => {
      val params = getParams()
      val newParams = params.filterNot(_.id == param.id)
      programModel.updateFunction(stmtId, parameters = Some(newParams))
      dom.window.document.getElementById(param.id).remove()
    }
    paramNameInput
  }

}
