package dev.sacode.flowrun

import org.scalajs.dom
import reactify.*
import org.getshaka.nativeconverter.NativeConverter
import org.getshaka.nativeconverter.fromJson
import dev.sacode.flowrun.codegen.Language

final case class FlowRunConfig(
    lang: Language
) derives NativeConverter

object FlowRunConfig {

  val default = FlowRunConfig(Language.scala)

  private val FlowRunConfigKey = "flowrun-config"
  private val localConfig = initLocalConfig()

  def resolve(): Var[FlowRunConfig] = localConfig

  private def initLocalConfig(): Var[FlowRunConfig] = {

    val config$ : Var[FlowRunConfig] = Var(null)
    config$.attach { newValue =>
      dom.window.localStorage.setItem(FlowRunConfigKey, newValue.toJson)
    }

    val savedConfigJson = dom.window.localStorage.getItem(FlowRunConfigKey)
    val config =
      if (savedConfigJson == null) default
      else
        try savedConfigJson.fromJson[FlowRunConfig]
        catch {
          case e => default
        }

    config$.set(config)
    config$
  }

}
