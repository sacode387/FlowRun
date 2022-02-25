package dev.sacode.flowrun

import org.scalajs.dom
import reactify.*
import org.getshaka.nativeconverter.NativeConverter
import org.getshaka.nativeconverter.fromJson
import dev.sacode.flowrun.codegen.Language

final case class FlowRunConfig(
    lang: Language,
    layout: String
) derives NativeConverter

object FlowRunConfig {

  val default = FlowRunConfig(Language.java, "")

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
      else savedConfigJson.fromJson[FlowRunConfig]

    config$.set(config)
    config$
  }

}
