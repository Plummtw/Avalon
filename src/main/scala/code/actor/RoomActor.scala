package org.plummtw.avalon.actor

import net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._
import actor._
import scala.util.Random

//import scala.xml.NodeSeq

import collection.mutable.HashMap

import org.plummtw.avalon.enume._
import org.plummtw.avalon.model._
import org.plummtw.avalon.util._
import org.plummtw.avalon.comet._
import org.plummtw.avalon.data._
import org.plummtw.avalon.heavy._

case class RoomSubscribe(comet : GameComet, room_id : Long, userentry_id : Long)
case class RoomUnsubscribe(comet : GameComet, room_id : Long, userentry_id : Long)
case class RoomDeleteRoom(room_id : Long)

case class NewMessage(room_id : Long, talk : Talk)
case class SignalAction(action : Action)
case class SignalAbandon(room : Room)
case class SignalDeadline(room : Room, roomround : RoomRound, roomphase : RoomPhase)

case class RoomForceOut(room_id : Long)
case class ForceOut(userentry_id : Long)
case class ForceLogOut(userentry_id : Long)

case class SessionVarSet(room : Room = null, roomround : RoomRound = null, roomphase : RoomPhase = null, 
                         userentrys : List[UserEntry] = List())
case class RoomForceUpdate(room_id : Long, updates : List[ForceUpdateEnum.Value])
case class UserEntryForceUpdate(userentry_id : Long, updates : List[ForceUpdateEnum.Value])

//case class NewRoomRound(room_id : Long, roomround : RoomRound)

object RoomActor extends LiftActor with Logger {
  private val userentry_message_map : HashMap[Long, GameComet] = new HashMap()
  private val reverse_map : HashMap[GameComet, (Long, Long)] = new HashMap()
  
  def sendUserEntryMessage(id : Long, message : Any) = {
    val comet_list = userentry_message_map.get(id) match {
      case Some(some_userentry) => some_userentry ! message
      case _                    => ; //warn("No UserEntry Found")
    }
  }
    
  private val room_message_map : HashMap[Long, List[GameComet]] = new HashMap()
  def sendRoomMessage(id : Long, message : Any) = {
    val comet_list = room_message_map.get(id) match {
      case Some(message_list) => message_list.foreach(_ ! message)
      case _                  => ; //warn("No Room Found")
    }
    
    message match {
      case SessionVarSet(room, roomround, roomphase, userentrys) =>
        ClockActor ! message
      case x => ;
    }
  }
  
  def process_signal_action(action: Action) = {
    //val roomround_a = RoomRound.find(By(RoomRound.id, action.roomround_id.is)).get
    val actioner    = UserEntry.find(By(UserEntry.id, action.actioner_id.is)).get
    val room = Room.find(By(Room.id, actioner.room_id.is)).get
    val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                   OrderBy(RoomRound.round_no, Descending)).get
    val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
                                   OrderBy(RoomPhase.phase_no, Descending)).get
    val userentrys = UserEntry.findAllByRoom(room)
    val actioner_reload = userentrys.filter(_.id.is == actioner.id.is)(0)
      
    val enabled_action_list = ActionHelper.enabled_action_list(room, roomround, roomphase, actioner_reload, userentrys.filter(!_.revoked.is))
    action.save
      
    // 這裡加入檢核      
    if (enabled_action_list.map(_.action_enum.toString).contains(action.mtype.toString)) {
      val talk = Talk.create.roomround_id(action.roomround_id.is).mtype(action.mtype.is)
                            .actioner_id(action.actioner_id.is).actionee_id(action.actionee_id.is)
                            .message_flags(action.action_flags.is)
      talk.save
      
      sendRoomMessage(roomround.room_id.is, NewMessage(roomround.room_id.is, talk))
      ActionHelper.process_action(action, room, roomround, roomphase, actioner_reload, userentrys)
    } else {
      warn("Room : " + room)
      warn("RoomRound : " + roomround)
      warn("RoomPhase : " + roomphase)
      warn("Actioner : " + actioner_reload)
      warn("UserEntrys : " + userentrys)
        
      warn("Check Failed :  " )
      warn("Action : " + action.mtype.toString)
      warn("Enabled : " + enabled_action_list.map(_.action_enum.toString).mkString(","))
        
    }    
  }
  
  override def messageHandler = (in:Any) => in match {
    case RoomSubscribe(comet, room_id, userentry_id) =>
      //println("RoomSubscribe : " + comet.toString + " " + room_id + " " + userentry_id)
      var comet_list = room_message_map.get(room_id) match {
        case Some(message_list) => message_list
        case _                  => List()
      }
      if (userentry_message_map.contains(userentry_id)) {
        val old_comet = userentry_message_map.get(userentry_id).get
        if (old_comet != comet) {
          old_comet ! ForceLogOut(userentry_id)
          comet_list = comet_list filterNot (_ == old_comet)
        }
      }
      if (reverse_map.contains(comet)) {
        val (old_room_id, old_userentry_id) = reverse_map.get(comet).get
        if (old_room_id != room_id) {
          val old_comet_list = room_message_map.get(old_room_id) match {
            case Some(message_list) => message_list
            case _                  => List()
          }
          room_message_map.put(old_room_id, old_comet_list filterNot (_ == comet))
        }
        
        if (old_userentry_id != userentry_id) {
          userentry_message_map.remove(old_userentry_id)
        }
      }
      if (!comet_list.contains(comet))
        room_message_map.put(room_id, comet :: comet_list)
      userentry_message_map.put(userentry_id, comet)
      reverse_map.put(comet, (room_id, userentry_id))
    case RoomUnsubscribe(comet, room_id, userentry_id) =>
      val comet_list = room_message_map.get(room_id) match {
        case Some(message_list) => message_list
        case _                  => List()
      }
      room_message_map.put(room_id, comet_list filterNot (_ == comet))
      userentry_message_map.remove(userentry_id)
      reverse_map.remove(comet)
    case RoomDeleteRoom(room_id) =>
      room_message_map.remove(room_id)
      
    case NewMessage(room_id, talk) =>
      talk.save
      sendRoomMessage(room_id, in)
      RoomRound.find(By(RoomRound.room_id, room_id), OrderBy(RoomRound.round_no, Descending)) match {
        case Full(roomround) =>
          if (roomround.round_no.is == 0) {
            // 更新廢村時間
            val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is), OrderBy(RoomPhase.phase_no, Descending)).get
            roomphase.deadline(PlummUtil.dateAddMinute(new java.util.Date(), 10)).save
            ClockActor.refresh_roomphase(room_id, roomphase)
          }
        case xs => ;
      }
      
      //room.talk_time(new java.util.Date()).save
      
    case SignalAction(action) =>
      process_signal_action(action)

    case SessionVarSet(room, roomround, roomphase, userentrys) =>
      sendRoomMessage(room.id.is, in)
      
    //case NewRoomRound(room_id, roomround) =>
    //  roomround.save
    //  sendRoomMessage(room_id, ForceUpdate(room_id, (List(ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TALK_TABLE))))
    case RoomForceUpdate(room_id, updates) =>
      sendRoomMessage(room_id, in)
    case UserEntryForceUpdate(userentry_id, updates) =>
      sendUserEntryMessage(userentry_id, in)
      
    case TickPlayers(room_id, count_down) =>
      //println("RoomActor TickPlayer")  
      sendRoomMessage(room_id, in)
      
    case Timeout(room_id) => 
      //println("RoomActor Timeout")  
      //sendUserEntryMessage(userentry_id, in)
      val room = Room.find(By(Room.id, room_id)).get
      val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                     OrderBy(RoomRound.round_no, Descending)).get
      val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
                                     OrderBy(RoomPhase.phase_no, Descending)).get
      val userentrys = UserEntry.findAllByRoom(room)
      val userentrys_rr = UserEntry.rr(userentrys)
      
      // 這裡加入檢核      
      RoomPhaseEnum.get_phase(roomphase.phase_type.is) match {
        case RoomPhaseEnum.TEAM_ASSIGN =>
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
                         .actioner_id(roomphase.leader.is)
          talk.save
          talk.send(room.id.is)
        
          val targets_length = GameProcessor.team_assign_number(userentrys_rr.length, roomround.round_no.is)
          val action_flags = Random.shuffle(Range(0,userentrys_rr.length)).take(targets_length)
                                   .foldLeft("")(_.toString + _.toString)
          val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_TEAM_ASSIGN.toString)
                             .actioner_id(roomphase.leader.is).action_flags(action_flags)
          process_signal_action(action)
                           
        case RoomPhaseEnum.TEAM_VOTE =>
          userentrys_rr.foreach { userentry =>
            if (userentry.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED)) {
              val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
                         .actioner_id(userentry.id.is)
              talk.save
              talk.send(room.id.is)
            
              val random_enum = if (Random.nextInt(2) == 0) MTypeEnum.ACTION_TEAM_VOTE_YES
                                else MTypeEnum.ACTION_TEAM_VOTE_NO
              val action = Action.create.roomround_id(roomround.id.is).actioner_id(userentry.id.is)
                                 .mtype(random_enum.toString)
              process_signal_action(action)
            }
          }
        
        case RoomPhaseEnum.MISSION => 
          val player_indexes = roomphase.assigned.is.map(_.toString.toInt)
          player_indexes.foreach { player_index =>
            val userentry = userentrys_rr(player_index)
            if (userentry.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED)) {
              val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
                         .actioner_id(userentry.id.is)
              talk.save
              talk.send(room.id.is)
            
              val random_enum = if (Random.nextInt(2) == 0) {
                if (Random.nextInt(2) == 0) MTypeEnum.ACTION_MISSION
                else MTypeEnum.ACTION_HINDER_MISSION 
              } else {
                if (userentry.get_role.role_side == RoleSideEnum.VILLAGER) MTypeEnum.ACTION_MISSION
                else MTypeEnum.ACTION_HINDER_MISSION 
              }
              val action = Action.create.roomround_id(roomround.id.is).actioner_id(userentry.id.is)
                                 .mtype(random_enum.toString)
              process_signal_action(action)
            }
          }
        
        case RoomPhaseEnum.BITE => 
          val actioner = userentrys_rr.filter(_.get_role.role_side == RoleSideEnum.WEREWOLF)(0)
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
                         .actioner_id(actioner.id.is)
          talk.save
          talk.send(room.id.is)
          
          val targets = ActionBite.targetable_users(room, roomround, roomphase, actioner, userentrys_rr)
          val action  = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BITE.toString)
                              .actioner_id(actioner.id.is).actionee_id(targets(Random.nextInt(targets.length)).id.is)
          process_signal_action(action)
          
        case RoomPhaseEnum.CRYSTAL_BALL => 
          val actioner = userentrys_rr.filter(_.has_item_flag(ItemFlagEnum.CRYSTAL_BALL))(0)
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
                         .actioner_id(actioner.id.is)
          talk.save
          talk.send(room.id.is)
          
          val targets = ActionCrystalBall.targetable_users(room, roomround, roomphase, actioner, userentrys_rr)
          val action  = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ITEM_CRYSTALBALL.toString)
                              .actioner_id(actioner.id.is).actionee_id(targets(Random.nextInt(targets.length)).id.is)
          process_signal_action(action)
        
        
        case _ => ;
      }
      
  }
}
