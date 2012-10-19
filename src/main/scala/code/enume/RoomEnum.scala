package org.plummtw.avalon.enume

object RoomFlagEnum extends Enumeration {
  val TEST_MODE       = Value("TM_")
  val WISH_ALIGN      = Value("WA_")
  val WISH_ROLE       = Value("WR_")
  
  val RANDOM_POSITION = Value("RP_")
  
  val NO_ROLE         = Value("R0_")
  val ROLE_GUARD      = Value("R1_")
  val ROLE_WHITEWOLF  = Value("R2_")
  val ROLE_SILVERWOLF = Value("R3_")
  val ROLE_PHANTOMWOLF = Value("R4_")
  
  val ITEM_CRYSTALBALL = Value("I1_")
  
  val FLAGNAME_MAP   = Map(
    TEST_MODE      -> "(測)",
    WISH_ALIGN     -> "(希)",
    WISH_ROLE      -> "(希職)",
    RANDOM_POSITION -> "(亂位)",
    NO_ROLE        -> "(無職)",
    ROLE_GUARD     -> "[獵]",
    ROLE_WHITEWOLF  -> "[白]",
    ROLE_SILVERWOLF  -> "[銀]",
    ROLE_PHANTOMWOLF -> "[幻]",
    ITEM_CRYSTALBALL -> "《水晶》"
  )
  
  def flag_name(flag : RoomFlagEnum.Value) = {
    FLAGNAME_MAP.get(flag)
  }
  
  implicit def roomflagenum2String (en : RoomFlagEnum.Value) : String = en.toString
}

object RoomStatusEnum extends Enumeration {
  type RoomStatusEnum = Value
  
  val WAITING  = Value("W")
  val PLAYING  = Value("P")
  val ENDED    = Value("E")
  implicit def roomstatusenum2String (en : RoomStatusEnum.Value) : String = en.toString
}

object RoomVictoryEnum extends Enumeration {
  type RoomVictoryEnum = Value
  
  val NONE         = Value("")
  
  val WEREWOLF_WIN  = Value("W1")
  val WEREWOLF_WIN2 = Value("W2")
  val VILLAGER_WIN  = Value("V")

  val DRAW         = Value("0")
  val ABANDONED    = Value("1")
  
  val VICTORY_MAP = Map(
    NONE          -> "無",
    WEREWOLF_WIN  -> "人狼",
    WEREWOLF_WIN2 -> "人狼",
    VILLAGER_WIN  -> "村民",
    DRAW          -> "平手",
    ABANDONED     -> "廢棄"
  )
  
  def victory_name(flag : RoomVictoryEnum.Value) : String = {
    VICTORY_MAP.get(flag).getOrElse("")
  }
  
  def victory_name(flag_str : String) : String = {
    val flag = try {RoomVictoryEnum.withName(flag_str)}
      catch {case e: Exception => NONE}
    victory_name(flag)
  }
  
  implicit def roomvictoryenum2String (en : RoomVictoryEnum.Value) : String = en.toString
}

object ForceUpdateEnum extends Enumeration {
  type ForceUpdateEnum = Value
  
  val NONE            = Value("")
  
  val GO_OUT_LINK     = Value("G")
  val ACTION_BAR      = Value("A")
  val USER_TABLE      = Value("U")
  val TIME_TABLE      = Value("I")
  val TALK_TABLE      = Value("T")

  implicit def forceupdateenum2String (en : ForceUpdateEnum.Value) : String = en.toString
}