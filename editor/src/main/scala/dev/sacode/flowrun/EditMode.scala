package dev.sacode.flowrun

enum EditMode(val editable: Boolean):
  case ReadOnly extends EditMode(false)
  case Restricted extends EditMode(true)
  case Edit extends EditMode(true)
