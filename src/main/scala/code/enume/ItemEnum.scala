package org.plummtw.avalon.enume

object ItemFlagEnum extends Enumeration {
  val NONE            = Value("")
  val CRYSTAL_BALL    = Value("CB")
  val EXCALIBUR       = Value("EX")
  
  val ITEMNAME_MAP   = Map(
    NONE           -> "",
    CRYSTAL_BALL   -> "球",
    EXCALIBUR      -> "劍"
  )
  
  def item_name(flag : ItemFlagEnum.Value) = {
    ITEMNAME_MAP.get(flag)
  }
  
  def get_itemenum(item_string : String) : ItemFlagEnum.Value = {
    try {withName(item_string)}
    catch {case _ => NONE}
  }
  
  implicit def itemenum2String (en : ItemFlagEnum.Value) : String = en.toString
}

