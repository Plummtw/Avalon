package org.plummtw.avalon.enume

object RoomPhaseEnum extends Enumeration {
  type RoomPhaseEnum = Value
  
  val NONE            = Value("")
  
  val GAMEHALL        = Value("GH")
  val TEAM_ASSIGN     = Value("TA")
  val TEAM_VOTE       = Value("TV")
  val MISSION         = Value("MI")
  val BITE            = Value("BI")
  val ENDED           = Value("EN")
  
  val CNAME_MAP     = scala.collection.immutable.TreeMap(
    NONE              -> "無",
    GAMEHALL          -> "遊戲大廳",
    TEAM_ASSIGN       -> "成員指派",
    TEAM_VOTE         -> "成員投票",
    MISSION           -> "建置結界",
    BITE              -> "人狼行動",
    ENDED             -> "遊戲結束"
  )
  
  def get_phase(phase : String) : RoomPhaseEnum.Value =
    try {RoomPhaseEnum.withName(phase)}
      catch {case _ => NONE}
  
  def get_cname(phase : RoomPhaseEnum.Value) : String =
    CNAME_MAP.get(phase).getOrElse("無")
  
  def get_cname(phase : String ) : String =
    get_cname(get_phase(phase))
  
  implicit def gamephaseenum2String (en : RoomPhaseEnum.Value) : String = en.toString
}
