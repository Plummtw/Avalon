package org.plummtw.avalon.model

import net.liftweb._
import net.liftweb.mapper._
import net.liftweb.util.FieldError

//import scala.util.matching.Regex

import net.liftweb.http.{SessionVar, RequestVar}
import net.liftweb.common.{Empty, Box}

import org.plummtw.avalon.enume._
import org.plummtw.avalon.util.PlummUtil

object CurrentRoom extends SessionVar[Box[Room]](Empty)
object Room_E      extends SessionVar[Room](null)
object Room_R      extends SessionVar[Room](null)

class Room extends LongKeyedMapper[Room] with CreatedUpdated with IdPK {
  def getSingleton = Room // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  
  object room_name     extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      List(if (in.length <= 0)       List(FieldError(this, <b>村子名稱不得為空白</b>))
           else if (in.length > 20)  List(FieldError(this, <b>村子名稱過長＞２０</b>))
           else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>村子名稱包含控制碼</b>)) else Nil).flatten
  }

  object room_comment  extends MappedString(this, 60) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      List(if (in.length <= 0)       List(FieldError(this, <b>村子說明不得為空白</b>))
           else if (in.length > 60)  List(FieldError(this, <b>村子說明過長＞６０</b>))
           else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>村子名稱包含控制碼</b>)) else Nil).flatten
  }

  object max_user      extends MappedInt(this) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Int): List[FieldError] = 
      if (in < 5)        List(FieldError(this, <b>最大玩家數過少＜５</b>))
      else if (in > 10)  List(FieldError(this, <b>最大玩家數過多＞１０</b>))
      else Nil
  }  
  
  object team_assign_time   extends MappedInt(this) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Int): List[FieldError] = 
      if (in < 15)        List(FieldError(this, <b>成員指派時間過短＜１５</b>))
      else if (in > 999)  List(FieldError(this, <b>成員指派時間過長＞９９９</b>))
      else Nil
  }
  
  object team_vote_time extends MappedInt(this) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Int): List[FieldError] = 
      if (in < 15)        List(FieldError(this, <b>成員投票時間過短＜１５</b>))
      else if (in > 999)  List(FieldError(this, <b>成員投票時間過長＞９９９</b>))
      else Nil
  }
  
  object mission_time extends MappedInt(this) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Int): List[FieldError] = 
      if (in < 15)        List(FieldError(this, <b>製作結界時間過短＜１５</b>))
      else if (in > 999)  List(FieldError(this, <b>製作結界時間過長＞９９９</b>))
      else Nil
  }
  
  object bite_time extends MappedInt(this) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Int): List[FieldError] = 
      if (in < 15)        List(FieldError(this, <b>狼咬時間過短＜１５</b>))
      else if (in > 999)  List(FieldError(this, <b>狼咬時間過長＞９９９</b>))
      else Nil
  }
  
  object item_time extends MappedInt(this) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Int): List[FieldError] = 
      if (in < 15)        List(FieldError(this, <b>道具時間過短＜１５</b>))
      else if (in > 999)  List(FieldError(this, <b>道具時間過長＞９９９</b>))
      else Nil
  }
  
  object room_flags    extends MappedString(this, 100) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length > 100)  List(FieldError(this, <b>選項字串過長＞１００</b>))
      else Nil
  }
  
  object villager_wins extends MappedInt(this) {
    override def defaultValue = 0
  }
  
  object werewolf_wins extends MappedInt(this) {
    override def defaultValue = 0
  }
  
  //object room_options  extends MappedString(this,20)
  object status        extends MappedString(this,1)
  object victory       extends MappedString(this,2)
  
  //object victory_all   extends MappedString(this,20)
  
  object ip_address     extends MappedString(this, 20)
  
  object talk_time      extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date()
  }
  
  def option_text : String = {
    val roomflags_enums : Array[String] = room_flags.is.split(',')

    roomflags_enums.map { item =>
      try { RoomFlagEnum.flag_name(RoomFlagEnum.withName(item)).getOrElse("") }
      catch { case e: Exception => "" }
    }.mkString("")
  }

  def has_flag(flag : RoomFlagEnum.Value) : Boolean = {
    return (room_flags.is.indexOf(flag.toString) != -1)
  }
  
  def hasnt_flag(flag : RoomFlagEnum.Value) : Boolean = 
    !has_flag(flag)
  
}

object Room extends Room with LongKeyedMetaMapper[Room] {
  override def fieldOrder = List(id, room_name, room_comment, max_user,
                                 team_assign_time, team_vote_time, mission_time, bite_time, item_time,
                                 room_flags, villager_wins, werewolf_wins,
                                 status, victory, 
                                 ip_address, talk_time)
}