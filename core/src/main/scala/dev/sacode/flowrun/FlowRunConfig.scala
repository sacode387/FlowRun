package dev.sacode.flowrun

import dev.sacode.flowrun.codegen.Language
import org.getshaka.nativeconverter.NativeConverter

final case class FlowRunConfig(
    lang: Language,
    layout: String
) derives NativeConverter