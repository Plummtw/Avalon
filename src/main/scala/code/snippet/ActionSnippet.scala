package org.plummtw.avalon.snippet

import _root_.net.liftweb._
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


import scala.xml.NodeSeq

//import collection.mutable.{LinkedList, HashMap, SynchronizedMap}

import org.plummtw.avalon.enume._
import org.plummtw.avalon.model._
import org.plummtw.avalon.util._
import org.plummtw.avalon.data._
import org.plummtw.avalon.actor._
//import org.plummtw.avalon.card._

import org.plummtw.avalon.heavy.GameProcessor

class ActionSnippet extends Logger {
  val random = scala.util.Random
  
  /*
  def alert(in: NodeSeq) =
    ajaxButton("Test",
      () => S.runTemplate(List("dialog/test_dialog")).
      map(ns => ModalDialog(ns)) openOr
      Alert("Couldn't find _jsdialog_confirm template"))
  */
  
  def test(in: NodeSeq) =
    bind("confirm", in,
         "yes" -> ((b: NodeSeq) => ajaxButton(b, () =>
          {println("Rhode Island Destroyed")
          Unblock & Alert("Rhode Island Destroyed")})),
         "no" -> ((b: NodeSeq) => <button onclick={Unblock.toJsCmd}>{b}</button>))

  def kick(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get

    var target_str : String = ""
    val targets = ActionKick.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch { case _ => 0}
      
      println("target_str : " + target_str)
      println("target_id : "  + target_id)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_KICK.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "kick" -> ajaxSubmit("踢出", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
    //"#user-select-table" #> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)) &
    //"#kick" #> ajaxSubmit("踢出", () => { process; Unblock }) &
    //"#cancel" #> <button onclick={Unblock.toJsCmd}>取消</button>
    //ajaxForm(bind("action", in,
    //       "user_select_table" -> 
    //       <div><div>{UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x))}</div>
    //       <div>{ajaxSubmit("踢出", () => { process; Unblock })}
    //       <button onclick={Unblock.toJsCmd}>取消</button></div></div>))
  }
  
  def bite(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get

    var target_str : String = ""
    val targets = ActionBite.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch { case _ => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BITE.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "bite" -> ajaxSubmit("咬人", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
    //ajaxForm(bind("action", in,
    //     "user_select_table" -> <div>{UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x))}
    //     <div>{ajaxSubmit("咬人", () => { process; Unblock })}
    //     <button onclick={Unblock.toJsCmd}>取消</button></div></div>))
  }
  
  def crystalball(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get

    var target_str : String = ""
    val targets = ActionCrystalBall.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch { case _ => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ITEM_CRYSTALBALL.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "use" -> ajaxSubmit("使用", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def team_assign(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    val targets = userentrys_rr
    val targets_length = GameProcessor.team_assign_number(userentrys_rr.length, roomround.round_no.is)
    
    var target_ids : List[Long] = List()
    
    
    def process = {
      val action_flags = target_ids.map(userentrys_rr.map(_.id.is).indexOf(_)).foldLeft("")(_.toString + _.toString)
    
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_TEAM_ASSIGN.toString)
                         .actioner_id(currentuserentry.id.is).action_flags(action_flags)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "assign_num"        -> <span>請選擇要指派的隊員({targets_length.toString})名</span>,
         "user_choose_table" -> UserEntryHelper.user_choose_table(userentrys_rr, targets, x => (target_ids = target_ids ::: List(x))),
         "team_assign"       -> ajaxSubmit("確定", () => {
           if (target_ids.length == targets_length) {
             process; Unblock 
           } else
             Unblock & Alert("選擇之玩家數不為 " + targets_length)
         }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
    
    //ajaxForm(bind("action", in,
    //     "assign_num"        -> <div><span>請選擇要指派的隊員({targets_length.toString})名</span>
    //       <div>{UserEntryHelper.user_choose_table(userentrys_rr, targets, x => (target_ids = target_ids ::: List(x)))}</div>
    //       <div>{ajaxSubmit("確定", () => {
    //       if (target_ids.length == targets_length) {
    //         process; Unblock 
    //       } else
    //         Unblock & Alert("選擇之玩家數不為 " + targets_length)
    //     })}<button onclick={Unblock.toJsCmd}>取消</button></div></div>))
  }  
}