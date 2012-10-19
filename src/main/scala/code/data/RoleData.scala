package org.plummtw.avalon.data

import org.plummtw.avalon.enume._
//import org.plummtw.avalon.card._

class RoleData(val enum : RoleEnum.Value, val name : String, val side : RoleSideEnum.Value) {
  def role_enum = enum
  def role_name = name
  def role_side = side
  
  def role_color = RoleSideEnum.get_roleside_color(role_side) 
  
  def cfield = {
    <span class={role_color}>[{role_name}]</span>
  }
  
  def simple_cfield = {
    <span class={role_color}>[{role_name.substring(0,1)}]</span>
  }
}

object RoleVillager extends RoleData(RoleEnum.VILLAGER, "村民", RoleSideEnum.VILLAGER)

object RoleWerewolf extends RoleData(RoleEnum.WEREWOLF, "人狼",  RoleSideEnum.WEREWOLF)

object RoleAugurer extends RoleData(RoleEnum.AUGURER, "占卜師",  RoleSideEnum.VILLAGER) {
  override def role_color = "augurer"
}

object RoleGuard extends RoleData(RoleEnum.GUARD, "獵人",  RoleSideEnum.VILLAGER) {
  override def role_color = "hunter"
}

object RoleWhitewolf extends RoleData(RoleEnum.WHITEWOLF, "白狼",  RoleSideEnum.WEREWOLF)

object RoleSilverwolf extends RoleData(RoleEnum.SILVERWOLF, "銀狼",  RoleSideEnum.WEREWOLF)

object RolePhantomwolf extends RoleData(RoleEnum.PHANTOMWOLF, "幻狼",  RoleSideEnum.WEREWOLF)

object RoleNone extends RoleData(RoleEnum.NONE, "無",  RoleSideEnum.NONE ) 
