package org.plummtw.avalon.heavy

import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import common._
import S._
import SHtml._
import Helpers._


import org.plummtw.avalon.model._
import org.plummtw.avalon.enume._
import org.plummtw.avalon.actor._
import org.plummtw.avalon.snippet._
import org.plummtw.avalon.data._
import org.plummtw.avalon.util.PlummUtil

object ActionHelper extends Logger {
  def action_list(roomphase: RoomPhase, currentuserentry : UserEntry, 
                  userentrys_rr:List[UserEntry]) : List[ActionData] = {
    RoomPhaseEnum.get_phase(roomphase.phase_type.is) match {
      case RoomPhaseEnum.GAMEHALL => List(ActionKick, ActionStartGame)
      case RoomPhaseEnum.TEAM_ASSIGN =>
        if (roomphase.leader.is == currentuserentry.id.is)
          List(ActionTeamAssign)
        else List()
      case RoomPhaseEnum.TEAM_VOTE => 
        if (currentuserentry.has_room_flag(UserEntryRoomFlagEnum.VOTED))
          List()
        else List(ActionTeamVoteYes, ActionTeamVoteNo)
      case RoomPhaseEnum.MISSION => 
        if (currentuserentry.has_room_flag(UserEntryRoomFlagEnum.VOTED))
          List()
        else {
          val useractionee_list2 = roomphase.assigned.is.map(x => userentrys_rr(x.toString.toInt).id.is)
          if (useractionee_list2.contains(currentuserentry.id.is))
            List(ActionMission, ActionHinderMission)
          else List()
        }
      case RoomPhaseEnum.BITE => 
        if (currentuserentry.get_role.role_side == RoleSideEnum.WEREWOLF)
          List(ActionBite)
        else List()
      case _ => List()
    }
  }
  
  def enabled_action_list(room:Room, roomround:RoomRound, roomphase:RoomPhase, 
                          currentuserentry:UserEntry, userentrys_rr:List[UserEntry]) : List[ActionData] = {
    action_list(roomphase, currentuserentry, userentrys_rr).filter {x => 
      if (!x.enabled(room, roomround, roomphase, currentuserentry, userentrys_rr)) false
      else true
    }
  }
  
  def process_action(action: Action, room: Room, roomround: RoomRound,
                   roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    val actioner_id = action.actioner_id.is
    val action_enum = MTypeEnum.get_action(action.mtype.is)
    val userentrys_rr = UserEntry.rr(userentrys)

    action_enum match {
      case MTypeEnum.ACTION_KICK =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actionee.damaged(actionee.damaged.is + 1)
        actionee.save

        // 如果被踢到 3 次
        val room = Room.find(By(Room.id, actionee.room_id.is)).get

        if (actionee.damaged.is >= 3) {
          UserEntrySnippet.revoke(room, actionee)
          RoomActor.sendUserEntryMessage(actionee.id.is, ForceOut(actionee.id.is))
        }
        val userentrys_reload = UserEntry.findAllByRoom(room)
        RoomActor.sendRoomMessage(actionee.room_id.is, SessionVarSet(room = room, userentrys = userentrys_reload))
        RoomActor.sendRoomMessage(actionee.room_id.is, RoomForceUpdate(actionee.room_id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.ACTION_BAR)))

      case MTypeEnum.ACTION_STARTGAME =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        actioner.add_room_flag(UserEntryRoomFlagEnum.VOTED)
        actioner.save

        // 如果全員都開始遊戲
        GameProcessLock.get_lock(actioner.room_id.is).synchronized {
          //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
          val userentrys_rr = UserEntry.rr(userentrys)
          val userentrys_notready = userentrys_rr.filter(x => (x.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED)))

          if ((userentrys_rr.length >= 5) && (userentrys_notready.length == 0)) {
            val room = Room.find(By(Room.id, actioner.room_id.is)).get
            room.status(RoomStatusEnum.PLAYING.toString)
            room.save
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room))
          
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is,
                                                                           List(ForceUpdateEnum.GO_OUT_LINK)))

            GameProcessor.process_start_game(room)
            // New Round
            //val room_reload       = Room.find(By(Room.id, actioner.room_id.is)).get
            val roomround_reload  = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                                   OrderBy(RoomRound.round_no, Descending)).get
            val roomphase_reload  = RoomPhase.find(By(RoomPhase.roomround_id, roomround_reload.id.is),
                                                   OrderBy(RoomPhase.phase_no, Descending)).get
            val userentrys_reload = UserEntry.findAllByRoom(room)

            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomround = roomround_reload,
                                                                          roomphase = roomphase_reload,
                                                                        userentrys = userentrys_reload))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is,
                                                                          List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TIME_TABLE, 
                                                                               ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.ACTION_BAR)))
          } else {
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
            RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }
        }

      case MTypeEnum.ACTION_TEAM_ASSIGN =>
        val new_phase_no = roomphase.phase_no.is + 1
        val new_phase = RoomPhase.create.roomround_id(roomphase.roomround_id.is)
                                 .phase_no(new_phase_no).phase_round(roomphase.phase_round.is)
                                 .phase_type(RoomPhaseEnum.TEAM_VOTE.toString)
                                 .leader(roomphase.leader.is)
                                 .assigned(action.action_flags.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.team_vote_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase,
          userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is,
          List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
          
      case MTypeEnum.ACTION_TEAM_VOTE_YES =>
        actioner.add_room_flag(UserEntryRoomFlagEnum.VOTED).save
        val vote = Vote.create.actioner_id(actioner.id.is).roomphase_id(roomphase.id.is)
                       .vote_yes(true).save
        if (!userentrys_rr.exists(_.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED))) {
          GameProcessor.check_team_vote(room, roomround, roomphase, userentrys_rr)
        } else {
          RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendUserEntryMessage(actioner.id.is, RoomForceUpdate(room.id.is,
            List(ForceUpdateEnum.ACTION_BAR)))
        }
        
      case MTypeEnum.ACTION_TEAM_VOTE_NO =>
        actioner.add_room_flag(UserEntryRoomFlagEnum.VOTED).save
        val vote = Vote.create.actioner_id(actioner.id.is).roomphase_id(roomphase.id.is)
                       .vote_yes(false).save
        if (!userentrys_rr.exists(_.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED))) {
          GameProcessor.check_team_vote(room, roomround, roomphase, userentrys_rr)
        } else {
          RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendUserEntryMessage(actioner.id.is, RoomForceUpdate(room.id.is,
            List(ForceUpdateEnum.ACTION_BAR)))
        }
        
      case MTypeEnum.ACTION_MISSION =>
        actioner.add_room_flag(UserEntryRoomFlagEnum.VOTED).save
        val vote = Vote.create.actioner_id(actioner.id.is).roomphase_id(roomphase.id.is)
                       .vote_yes(true).save
        if (userentrys_rr.filter(_.has_room_flag(UserEntryRoomFlagEnum.VOTED)).length == roomphase.assigned.is.length) {
          GameProcessor.check_mission_vote(room, roomround, roomphase, userentrys_rr)
        } else {
          RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendUserEntryMessage(actioner.id.is, RoomForceUpdate(room.id.is,
            List(ForceUpdateEnum.ACTION_BAR)))
        }
        
      case MTypeEnum.ACTION_HINDER_MISSION =>
        actioner.add_room_flag(UserEntryRoomFlagEnum.VOTED).save
        val vote = Vote.create.actioner_id(actioner.id.is).roomphase_id(roomphase.id.is)
                       .vote_yes(false).save
        if (userentrys_rr.filter(_.has_room_flag(UserEntryRoomFlagEnum.VOTED)).length == roomphase.assigned.is.length) {
          GameProcessor.check_mission_vote(room, roomround, roomphase, userentrys_rr)
        } else {
          RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendUserEntryMessage(actioner.id.is, RoomForceUpdate(room.id.is,
            List(ForceUpdateEnum.ACTION_BAR)))
        }
        
      case MTypeEnum.ACTION_BITE =>
        val actionee : UserEntry = UserEntry.get(action.actionee_id.is, userentrys)
        if (actionee.get_role == RoleAugurer)
          GameProcessor.process_victory(room, roomround, userentrys_rr, RoomVictoryEnum.WEREWOLF_WIN2)
        else
          GameProcessor.process_victory(room, roomround, userentrys_rr, RoomVictoryEnum.VILLAGER_WIN)
          
      case xs =>
        warn("Unprocessed Action : " + action.toString)
    }
  }
}
