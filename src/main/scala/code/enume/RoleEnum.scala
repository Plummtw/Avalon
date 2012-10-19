package org.plummtw.avalon.enume

import org.plummtw.avalon.data._

object RoleSideEnum extends Enumeration {
  type RoleSideEnum = Value

  val NONE     = Value("")
  val WEREWOLF = Value("W")
  val VILLAGER = Value("V")
  
  val ROLESIDE_COLOR_MAP   = scala.collection.immutable.TreeMap(
    NONE     -> "none",
    WEREWOLF -> "werewolf",
    VILLAGER -> "villager"
  )
  
  val ROLESIDE_CNAME_MAP   = scala.collection.immutable.TreeMap(
    WEREWOLF -> "人狼",
    VILLAGER -> "村民"
  )
  
  val ROLESIDE_LIST = List(
    WEREWOLF, VILLAGER
  )
  
  def get_roleside_color(roleside : RoleSideEnum.Value) : String = {
    ROLESIDE_COLOR_MAP.get(roleside).getOrElse("")
  }
  
  def get_roleside_cname(roleside : RoleSideEnum.Value) : String = {
    ROLESIDE_CNAME_MAP.get(roleside).getOrElse("")
  }
  
  def get_roleside_cname(roleside : String) : String = {
    try { get_roleside_cname(withName(roleside)) }
    catch { case e : Exception => ""}
  }
  
  implicit def rolesideenum2String (en : RoleSideEnum.Value) : String = en.toString
}

object RoleEnum extends Enumeration {
  type RoleEnum = Value

  val NONE       = Value("")
  
  val VILLAGER   = Value("VI")
  val WEREWOLF   = Value("WW")
  
  val AUGURER    = Value("AU")
  val GUARD      = Value("GU")
  val WHITEWOLF  = Value("HW")
  val SILVERWOLF = Value("SW")
  val PHANTOMWOLF = Value("PW")
  
  val ROLE_MAP = scala.collection.immutable.TreeMap(
    NONE     -> RoleNone,
    VILLAGER -> RoleVillager,
    WEREWOLF -> RoleWerewolf,
    AUGURER  -> RoleAugurer,
    GUARD    -> RoleGuard,
    WHITEWOLF -> RoleWhitewolf,
    SILVERWOLF -> RoleSilverwolf,
    PHANTOMWOLF -> RolePhantomwolf
  )
  
  val WISH_ROLE_LIST = List(NONE, VILLAGER, WEREWOLF, AUGURER)
  
  def get_role(role : RoleEnum.Value) : RoleData = {
    val result = ROLE_MAP.get(role) 
    //if (result.isEmpty)
    //  println(role.toString + "is null")
    return result.getOrElse(RoleNone)
  }
  
  def get_role(role_string : String) : RoleData = {
    try {get_role(withName(role_string)) }
    catch {case _ => RoleNone}
  }
  
  implicit def roleenum2String (en : RoleEnum.Value) : String = en.toString
}

