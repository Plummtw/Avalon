package org.plummtw.avalon.data

import net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import js.jquery._
import JqJsCmds._
import common._
import util._
import Helpers._
import actor._

//import scala.xml.NodeSeq

import org.plummtw.avalon.model._
import org.plummtw.avalon.enume._
import org.plummtw.avalon.actor._
import org.plummtw.avalon.util._
//import org.plummtw.avalon.card._

import org.plummtw.avalon.heavy.GameProcessor

trait UserEntryTargetable {
  def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is))
  }  
}

class ActionData(action: MTypeEnum.Value, str: String) {
  def action_enum     = action
  def tag_string      = str
  def command_name    = "action_" + action.toString
    
  def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, 
              currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : Boolean= true
  
  def js_command : JsCmd = Noop
  
  def js_dialog(dialog_name : String) = 
    if (CurrentUserEntry_R.get == null)
      Alert("你不在遊戲中，請重新登入")
    else
      S.runTemplate(List("dialog/" + dialog_name)).
        map(ns => ModalDialog(ns)) openOr
        Alert("Couldn't find " + dialog_name + " template")
      
  def js_action : JsCmd = {
    val roomround = RoomRound_R.get
    val currentuserentry = CurrentUserEntry_R.get
    
    if (currentuserentry == null)
      return Alert("你不在遊戲中，請重新登入")
    
    val action = Action.create.roomround_id(roomround.id.is).actioner_id(currentuserentry.id.is)
                              .mtype(action_enum.toString)
    RoomActor ! SignalAction(action)
    Noop
  }
  
  def toAjaxButton = ajaxButton(this.toString, () => S.callOnce(js_command))

  override def toString(): String = tag_string // "[" + tag_string + "]"
}

//object ActionNoAction extends ActionData(MTypeEnum.ACTION_NO_ACTION, "") {
//  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry])  = false
//}

object ActionTestAlert extends ActionData(MTypeEnum.ACTION_TEST_ALERT, "ALERT") {
  override def js_command : JsCmd =
    //Alert("Test")
    S.runTemplate(List("dialog/test_dialog")).
      map(ns => ModalDialog(ns)) openOr
      Alert("Couldn't find test_dialog template")
}

object ActionKick extends ActionData(MTypeEnum.ACTION_KICK, "踢人") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, 
                       currentuserentry : UserEntry, userentrys_rr : List[UserEntry])  = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString) &&
    (userentrys_rr.length > 1)
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase,
                                currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val roomround = RoomRound_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    val kick_actions = Action.findAll(By(Action.roomround_id, roomround.id.is),
                                      By(Action.mtype, MTypeEnum.ACTION_KICK.toString),
                                      By(Action.actioner_id, currentuserentry.id.is))
    val kick_actionees = kick_actions.map(_.actionee_id.is)
    
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is)
                         && (!kick_actionees.contains(x.id.is)))
  }

  override def js_command : JsCmd = js_dialog("kick_dialog")
}

object ActionStartGame extends ActionData(MTypeEnum.ACTION_STARTGAME, "開始遊戲") {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, 
                       currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    //(roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString) && 
    (currentuserentry.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED))
  }
  
  override def js_command : JsCmd = js_action
}

object ActionTeamAssign extends ActionData(MTypeEnum.ACTION_TEAM_ASSIGN, "指派隊員") {
  override def js_command : JsCmd = js_dialog("team_assign_dialog")
}

object ActionTeamVoteYes extends ActionData(MTypeEnum.ACTION_TEAM_VOTE_YES, "贊成") {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, 
                       currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    (currentuserentry.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED))
  }
  
  override def js_command : JsCmd = js_action
}

object ActionTeamVoteNo extends ActionData(MTypeEnum.ACTION_TEAM_VOTE_NO, "反對") {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, 
                       currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    (currentuserentry.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED))
  }
  
  override def js_command : JsCmd = js_action
}

object ActionMission extends ActionData(MTypeEnum.ACTION_MISSION, "建置結界") {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, 
                       currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    (currentuserentry.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED))
  }
  
  override def js_command : JsCmd = js_action
}

object ActionHinderMission extends ActionData(MTypeEnum.ACTION_HINDER_MISSION, "妨礙") {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, 
                       currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    (currentuserentry.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED))
  }
  
  override def js_command : JsCmd = js_action
}

object ActionBite extends ActionData(MTypeEnum.ACTION_BITE, "咬人") with UserEntryTargetable {
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase,
                                currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    userentrys_rr.filter(x=>x.get_role.role_side != RoleSideEnum.WEREWOLF)
  }

  override def js_command : JsCmd = js_dialog("bite_dialog")
}

object ActionCrystalBall extends ActionData(MTypeEnum.ITEM_CRYSTALBALL, "使用水晶球") with UserEntryTargetable {
  override def js_command : JsCmd = js_dialog("crystalball_dialog")
}


