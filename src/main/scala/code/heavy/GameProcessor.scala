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

//import scala.collection.JavaConversions
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer 

import org.plummtw.avalon.model._
import org.plummtw.avalon.enume._
import org.plummtw.avalon.actor._
import org.plummtw.avalon.data._
//import org.plummtw.avalon.card._
import org.plummtw.avalon.util.PlummUtil

object GameProcessor extends Logger{
  val random = scala.util.Random
  
  val team_assign_number_hash = Map(
    5 -> List(2,3,2,3,3),
    6 -> List(2,3,4,3,4),
    7 -> List(2,3,3,4,4),
    8 -> List(3,4,4,5,5),
    9 -> List(3,4,4,5,5),
   10 -> List(3,4,4,5,5)
  )
  
  def team_assign_number(members : Int, day : Int) = {
    team_assign_number_hash(members)(day-1)
  }
  
  // 分配職業
  def dispatch_role(room : Room, userentrys : List[UserEntry]) {
    val random = new scala.util.Random
        
    val werewolf_number = (userentrys.length + 2) / 3
    val villager_number = userentrys.length - werewolf_number
    
    var java_werewolf_list: java.util.List[RoleEnum.Value] = new java.util.LinkedList()
    var java_villager_list: java.util.List[RoleEnum.Value] = new java.util.LinkedList()
    
    // 先產生職業清單
   
    if (room.hasnt_flag(RoomFlagEnum.NO_ROLE)) {
      java_villager_list.add(RoleEnum.AUGURER)
      
      if (room.has_flag(RoomFlagEnum.ROLE_GUARD))
        java_villager_list.add(RoleEnum.GUARD)
        
      if (room.has_flag(RoomFlagEnum.ROLE_WHITEWOLF))
        java_werewolf_list.add(RoleEnum.WHITEWOLF)
        
      if (room.has_flag(RoomFlagEnum.ROLE_SILVERWOLF))
        java_werewolf_list.add(RoleEnum.SILVERWOLF)
        
      if (room.has_flag(RoomFlagEnum.ROLE_PHANTOMWOLF))
        java_werewolf_list.add(RoleEnum.PHANTOMWOLF)
    }
    
    val werewolf_filler_num = werewolf_number - java_werewolf_list.length
    for (i <- 1 to werewolf_filler_num)
      java_werewolf_list.add(RoleEnum.WEREWOLF)

    val villager_filler_num = villager_number - java_villager_list.length
    for (i <- 1 to villager_filler_num)
      java_villager_list.add(RoleEnum.VILLAGER)
      
    java.util.Collections.shuffle(java_werewolf_list)
    java.util.Collections.shuffle(java_villager_list)
    
    java_werewolf_list = java_werewolf_list.subList(0, werewolf_number)
    java_villager_list = java_villager_list.subList(0, villager_number)
    
    //println("shadow : " + java_shadow_list.size())
    //println("hunter : " + java_hunter_list.size())
    //println("neutral : " + java_neutral_list.size())
    
    // 設定玩家優先順位
    var java_user_no_array : java.util.LinkedList[Int] = new java.util.LinkedList()
    for (i <- 1 to userentrys.length)
      java_user_no_array.add(i)
      
    java.util.Collections.shuffle(java_user_no_array)
    
    userentrys.foreach { userentry =>
      userentry.user_no(java_user_no_array.removeFirst()).room_flags(userentry.role.is).role("")
      //println(user_entry.user_no.is + " " + user_entry.user_flags.is)
    }

    val userentrys_ordered = userentrys.sortBy(_.user_no.is)

    // 第一次先看看有沒有希望陣營
    userentrys_ordered.foreach { userentry =>
      val role  = try { RoleEnum.withName(userentry.role_flags.is) }
        catch { case _ => RoleEnum.NONE }
      val align = try { RoleSideEnum.withName(userentry.subrole.is) }
        catch { case _ => RoleSideEnum.NONE }
      if ((role != RoleEnum.NONE) && (role != null) && (random.nextInt(6) != 0)) {
        //error("role : " + role.toString)
        //error("get_role : " + RoleEnum.get_role(role).toString)
        //error("role_side : " + RoleEnum.get_role(role).role_side.toString)
        RoleEnum.get_role(role).role_side match {
          case RoleSideEnum.WEREWOLF => 
            if (java_werewolf_list.contains(role)) {
              try { 
                java_werewolf_list.remove(role)
                userentry.role(role.toString)
              } catch { case _ => 
                warn("Java Werewolf List : " + java_werewolf_list.toString)
                warn("Role : " + role.toString)
              }
            }
            
          case RoleSideEnum.VILLAGER => java_villager_list
            if (java_villager_list.contains(role)) {
              try { 
                java_villager_list.remove(role)
                userentry.role(role.toString)
              } catch { case _ => 
                warn("Java VILLAGER List : " + java_villager_list.toString)
                warn("Role : " + role.toString)
              }
            }
        }
        
      } else if ((align != RoleSideEnum.NONE) && (align != null) && (random.nextInt(6) != 0)) {
        align match {
          case RoleSideEnum.WEREWOLF => 
            if (java_werewolf_list.size() != 0) {
              userentry.role(java_werewolf_list.get(0).toString)
              java_werewolf_list.remove(0)
            }
             
          case RoleSideEnum.VILLAGER => 
            if (java_villager_list.size() != 0) {
              userentry.role(java_villager_list.get(0).toString)
              java_villager_list.remove(0)
            }
        }
      }
    }

    // 然後設定剩下的職業
    var java_role_array : java.util.LinkedList[RoleEnum.Value] = new java.util.LinkedList()
    java_role_array.addAll(java_werewolf_list)
    java_role_array.addAll(java_villager_list)
    
    java.util.Collections.shuffle(java_role_array)

    userentrys_ordered.foreach { userentry =>
      if (userentry.role.is == "") {
        userentry.role(java_role_array.removeFirst().toString)
      }
    }
    
    //java_user_no_array : java.util.LinkedList[Int] = new java.util.LinkedList()
    for (i <- 1 to userentrys.length)
      java_user_no_array.add(i)
    
    if (room.has_flag(RoomFlagEnum.RANDOM_POSITION))
      java.util.Collections.shuffle(java_user_no_array)
    
    userentrys.foreach { userentry =>
      userentry.user_no(java_user_no_array.removeFirst()).room_flags("").role_flags("").damaged(0).subrole("")
      if (room.has_flag(RoomFlagEnum.ITEM_CRYSTALBALL) && (userentry.user_no.is == 1))
        userentry.add_item_flag(ItemFlagEnum.CRYSTAL_BALL)
      userentry.save
    }
  }
  
  def process_start_game(room : Room) = {
    // val room = Room.find(By(Room.id, room_id)).get
    var userentrys_rr = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                        By(UserEntry.revoked, false))
    
    dispatch_role(room, userentrys_rr)
    
    if (room.has_flag(RoomFlagEnum.RANDOM_POSITION))
      userentrys_rr = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                        By(UserEntry.revoked, false),
                                        OrderBy(UserEntry.user_no, Ascending))
    
    // 加入第一回合
    val new_round = RoomRound.create.room_id(room.id.is).round_no(1)
    new_round.save
    
    val new_phase = RoomPhase.create.roomround_id(new_round.id.is)
                    .phase_no(1).phase_round(1)
                    .phase_type(RoomPhaseEnum.TEAM_ASSIGN.toString)
                    .leader(userentrys_rr(0).id.is)
                    .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.team_assign_time.is))
    new_phase.save

    // 產生人數字串
    val align_text = new StringBuffer("陣營分布：")
    align_text.append("　村民側：")
    align_text.append(userentrys_rr.filter(x => 
      RoleEnum.get_role(x.role.is).role_side == RoleSideEnum.VILLAGER).length)
    align_text.append("　人狼側：")
    align_text.append(userentrys_rr.filter(x => 
      RoleEnum.get_role(x.role.is).role_side == RoleSideEnum.WEREWOLF).length)

    val talk = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message("第 " + (new_round.round_no.is.toString) + " 日 " + 
                                   " 第 " + (new_phase.phase_round.is.toString) + " 回 " +
                                   " (成員：" + team_assign_number(userentrys_rr.length, new_round.round_no.is) +
                                   " ,領隊：" + userentrys_rr(0).handle_name.is + ") " +
                                   (new java.util.Date).toString)
    talk.save
    
    val talk2 = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                           .message(align_text.toString).cssclass("normal")
    talk2.save
    
    val talk3 = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                           .message("成員數：" + team_assign_number_hash(userentrys_rr.length)
                           .map(_.toString).reduceLeft(_ + " , " + _)).cssclass("normal")
    talk3.save
    //RoomActor ! NewMessageNoSend(talk)
    
    room.status(RoomStatusEnum.PLAYING.toString)
    room.save
    
    //RoomActor ! NewRoomRound(room_i, new_round)
  }
  
  def process_victory(room : Room, roomround: RoomRound, userentrys: List[UserEntry], 
                      victory : RoomVictoryEnum.Value) = {
    val new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                        .last_round(roomround.id.is)
    new_roomround.save
    val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("遊戲結束 "+ (new java.util.Date).toString)
    talk.save
    talk.send(room.id.is)
    
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(0)
                                    .phase_type(RoomPhaseEnum.ENDED.toString)
    new_phase.save
    
    room.status(RoomStatusEnum.ENDED.toString).victory(victory.toString)
    room.save

    RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, roomround = new_roomround, 
      roomphase = new_phase, userentrys = userentrys))
    RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is ,List(ForceUpdateEnum.USER_TABLE,
      ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR))) // ForceUpdateEnum.TALK_TABLE, 
  }
  
  def check_team_vote(room: Room, roomround: RoomRound, roomphase: RoomPhase, userentrys: List[UserEntry]) : Unit = {
    val new_phase_no = roomphase.phase_no.is + 1
    
    val vote_result = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_TEAM_VOTE.toString)
                                 .message(roomphase.id.is.toString)
    vote_result.save
    vote_result.send(room.id.is)
  
    val votes = Vote.findAll(By(Vote.roomphase_id, roomphase.id.is))
    val votes_yes = votes.filter(_.vote_yes.is)
    val votes_no  = votes.filter(!_.vote_yes.is)
    
    var new_phase : RoomPhase = null
    
    userentrys.foreach { userentry =>
      if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED))
        userentry.remove_room_flag(UserEntryRoomFlagEnum.VOTED).save
    }
    
    if (votes_yes.length > votes_no.length) {
      val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                            .message("成員投票成功：" + votes_yes.length + " 比 " + votes_no.length)
      talk.save
      talk.send(room.id.is)
      
      // 進入 MISSION 階段
      new_phase = RoomPhase.create.roomround_id(roomphase.roomround_id.is)
                           .phase_no(new_phase_no).phase_round(roomphase.phase_round.is)
                           .phase_type(RoomPhaseEnum.MISSION.toString)
                           .leader(roomphase.leader.is)
                           .assigned(roomphase.assigned.is)
                           .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.mission_time.is))
      new_phase.save
    } else {
      val talk0 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                            .message("成員投票失敗：" + votes_yes.length + " 比 " + votes_no.length)
      talk0.save
      talk0.send(room.id.is)
    
    
      // 到下一個 TEAM_ASSIGNED 階段
      var player_index = userentrys.map(_.id.is).indexOf(roomphase.leader.is) + 1
      if (player_index >= userentrys.length)
        player_index = 0
        
      //val vote_result = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_TEAM_VOTE.toString)
      //                      .message(roomphase.id.is.toString)
      //vote_result.save
      
      new_phase = RoomPhase.create.roomround_id(roomphase.roomround_id.is)
                           .phase_no(new_phase_no).phase_round(roomphase.phase_round.is + 1)
                           .phase_type(RoomPhaseEnum.TEAM_ASSIGN.toString)
                           .leader(userentrys(player_index).id.is)
                           .assigned(roomphase.assigned.is)
                           .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.team_assign_time.is))
      new_phase.save
      
      val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                            .message("第 " + (roomround.round_no.is.toString) + " 日 " + 
                                     " 第 " + (new_phase.phase_round.is.toString) + " 回 " +
                                     " (成員：" + team_assign_number(userentrys.length, roomround.round_no.is) +
                                     " ,領隊：" + userentrys(player_index).handle_name.is + ") " +
                                     (new java.util.Date).toString)
      talk.save
      talk.send(room.id.is)
    }
    
    RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, roomphase = new_phase,
      userentrys = userentrys))
    RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is,
      List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.USER_TABLE)))
      // ForceUpdateEnum.TALK_TABLE, 
  }
  
  def check_mission_vote(room: Room, roomround: RoomRound, roomphase: RoomPhase, 
                         userentrys: List[UserEntry]) : Unit = {
    val new_phase_no = roomphase.phase_no.is + 1
    
    val vote_result = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_MISSION.toString)
                                 .message(roomphase.id.is.toString)
    vote_result.save
    vote_result.send(room.id.is)
  
    val votes = Vote.findAll(By(Vote.roomphase_id, roomphase.id.is))
    val votes_no  = votes.filter(!_.vote_yes.is)
    
    val mission_fail = (votes_no.length >= 2) ||
                       ((votes_no.length == 1) && ((roomround.round_no.is != 4) || 
                                                   (userentrys.length < 7)))
    
    userentrys.foreach { userentry =>
      if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED))
        userentry.remove_room_flag(UserEntryRoomFlagEnum.VOTED).save
    }
    
    if (!mission_fail) {
      room.villager_wins(room.villager_wins.is + 1).save
      val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                            .message("建置結界成功，成功數：" + room.villager_wins.is.toString)
      talk.save
      talk.send(room.id.is)
    } else {
      room.werewolf_wins(room.werewolf_wins.is + 1).save
      val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                            .message("建置結界失敗，失敗數：" + room.werewolf_wins.is.toString)
      talk.save
      talk.send(room.id.is)
    }
    
    if (room.villager_wins.is >= 3) {
    
      if (room.has_flag(RoomFlagEnum.NO_ROLE))
        GameProcessor.process_victory(room, roomround, userentrys, RoomVictoryEnum.VILLAGER_WIN)
      else {
        // 進入 BITE 階段
        val new_phase = RoomPhase.create.roomround_id(roomphase.roomround_id.is)
                                 .phase_no(new_phase_no).phase_round(roomphase.phase_round.is)
                                 .phase_type(RoomPhaseEnum.BITE.toString)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.bite_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, roomphase = new_phase,
          userentrys = userentrys))
        RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is,
          List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
      }
    } else if (room.werewolf_wins.is >= 3) {
      GameProcessor.process_victory(room, roomround, userentrys, RoomVictoryEnum.WEREWOLF_WIN)
    } else {
      // 檢查是否進入水晶球階段
      if ((room.has_flag(RoomFlagEnum.ITEM_CRYSTALBALL)) &&
          (roomround.round_no.is >= 2) && (roomround.round_no.is <= 4)) {
        val new_phase = RoomPhase.create.roomround_id(roomphase.roomround_id.is)
                                 .phase_no(new_phase_no).phase_round(roomphase.phase_round.is)
                                 .phase_type(RoomPhaseEnum.CRYSTAL_BALL.toString)
                                 .leader(roomphase.leader.is)
                                 .assigned(roomphase.assigned.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.item_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, roomphase = new_phase,
          userentrys = userentrys))
        RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is,
          List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
      } else
        // 進入下一日
        next_day(room, roomround, roomphase, userentrys)
    }
  }
  
  def next_day(room: Room, roomround: RoomRound, roomphase: RoomPhase, 
                         userentrys: List[UserEntry]) : Unit = {
    val new_phase_no = roomphase.phase_no.is + 1
    
    val new_round = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                             .last_round(roomround.id.is)
    new_round.save
    
    var player_index = userentrys.map(_.id.is).indexOf(roomphase.leader.is) + 1
    if (player_index >= userentrys.length)
      player_index = 0
    
    val new_phase = RoomPhase.create.roomround_id(new_round.id.is)
                    .phase_no(new_phase_no).phase_round(1)
                    .phase_type(RoomPhaseEnum.TEAM_ASSIGN.toString)
                    .leader(userentrys(player_index).id.is)
                    .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.team_assign_time.is))
    new_phase.save
      
    val talk = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message("第 " + (new_round.round_no.is.toString) + " 日 " + 
                                   " 第 " + (new_phase.phase_round.is.toString) + " 回 " +
                                   " (成員：" + team_assign_number(userentrys.length, new_round.round_no.is) +
                                   " ,領隊：" + userentrys(player_index).handle_name.is + ") " +
                                   (new java.util.Date).toString)
    talk.save
    talk.send(room.id.is)
    
    if ((new_round.round_no.is == 4) && (userentrys.length >= 7)) {
      val talk1 = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                            .message("提醒：本日須 2 個妨礙才會失敗")
      talk1.save
      talk1.send(room.id.is)
    }
      
    RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, roomround=new_round, roomphase = new_phase,
      userentrys = userentrys))
    RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is,
      List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.USER_TABLE)))
        // ForceUpdateEnum.TALK_TABLE, 
  }
  
  def abandon(room : Room) = {
    val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is), OrderBy(RoomRound.round_no, Descending)).get
    val new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                        .last_round(roomround.id.is)
    new_roomround.save
    val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("遊戲結束 "+ (new java.util.Date).toString)
    talk.save
     
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(0)
                            .phase_type(RoomPhaseEnum.ENDED.toString)
    new_phase.save
             
    room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
    room.save
            
    RoomActor ! SessionVarSet(room = room, roomround = new_roomround, roomphase = new_phase)
    //RoomActor.sendRoomMessage(room_id, RoomForceUpdate(room_id ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
    RoomActor.sendRoomMessage(room.id.is, RoomForceOut(room.id.is))
  }
}
