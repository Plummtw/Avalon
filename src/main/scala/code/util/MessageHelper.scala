package org.plummtw.avalon.util

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._

import scala.xml.NodeSeq
import scala.xml.Unparsed

import org.plummtw.avalon.enume._
import org.plummtw.avalon.model._
//import org.plummtw.avalon.data._
import org.plummtw.avalon.util._
import org.plummtw.avalon.actor._

import org.plummtw.avalon.heavy.GameProcessor

object MessageHelper {
  // 一般言論
  def simple_talk_tag(talk:Talk, userentrys: List[UserEntry]) : NodeSeq= {
    val mtype : MTypeEnum.Value = 
      try { MTypeEnum.withName(talk.mtype.is) }
      catch { case e: Exception => MTypeEnum.TALK }
    var userentry = UserEntry.get(talk.actioner_id.is, userentrys) //UserEntrys_R.get
    val usericon  = userentry.get_user_icon

    mtype match {
      case MTypeEnum.TALK_DAY    =>
        Seq(<tr class="user-talk"><td class="user-name">
          <font color={usericon.color.is}>◆</font>{userentry.handle_name.is} </td>
          <td class={"say " + talk.cssclass.is}> {Unparsed(talk.message.is)} </td></tr>)
       case MTypeEnum.TALK_ADMIN    =>
        Seq(<tr class="admin-talk"><td class="user-name">
          <font color={usericon.color.is}>◆</font><span class="parrot">管理員</span> </td>
          <td class={"say " + talk.cssclass.is}> {Unparsed(talk.message.is)} </td></tr>)
       case MTypeEnum.TALK_ADMIN_PRIVATE    =>
        val useractionee = UserEntry.get(talk.actionee_id.is, userentrys)
        
        Seq(<tr class="admin-talk"><td class="user-name">
          <font color={usericon.color.is}>◆</font><span class="parrot">管理員</span>的悄悄話({useractionee.handle_name.is}) </td>
          <td class={"say " + talk.cssclass.is}> {Unparsed(talk.message.is)} </td></tr>)
        
      case _ => NodeSeq.Empty 
    }
  }
  
  def simple_message_tag(message: String) : NodeSeq= {
    Seq(<tr><td class="system-user" colspan="2">　　{message}</td></tr>)
  }
  
  def simple_message_tag(node: scala.xml.Node) : NodeSeq= {
    Seq(<tr><td class="system-user" colspan="2">　　{node}</td></tr>)
  }
  
  def simple_message_tag(message: String, reveal_mode: Boolean, cssclass : String) : NodeSeq= {
    val css_str = if (cssclass == "") "system-user" else  cssclass 
  
    if (reveal_mode)
      //Seq(<tr><td width="1000" colspan="3" align="left" style={style_str}>　　　　　　　　　　　　{message} </td></tr>)
      Seq(<tr><td class={css_str} colspan="2">　　{message}</td></tr>)
    else
      NodeSeq.Empty  
  }
  
  def talk_tag(talk: Talk, userentrys: List[UserEntry], reveal_mode: Boolean): NodeSeq = {
    val currentuserentry_id = CurrentUserEntry.get match {
      case Full(x) => x.id.is
      case x       => 0
    }

    val mtype : MTypeEnum.Value = 
      try { MTypeEnum.withName(talk.mtype.is) }
      catch { case e: Exception => MTypeEnum.TALK }
    
    val useractioner = UserEntry.get(talk.actioner_id.is, userentrys)
    val useractionee = UserEntry.get(talk.actionee_id.is, userentrys)
    
    mtype match {
      case MTypeEnum.TALK_ADMIN          => simple_talk_tag(talk, userentrys)
      case MTypeEnum.TALK_ADMIN_PRIVATE  => //simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
        if ((reveal_mode) || (currentuserentry_id == useractionee.id.is))
          simple_talk_tag(talk, userentrys)
        else
          Seq()
      case MTypeEnum.TALK_DAY            => simple_talk_tag(talk, userentrys)
      
      case MTypeEnum.MESSAGE_GENERAL     => simple_message_tag(talk.message.is)
      case MTypeEnum.MESSAGE_COME        => simple_message_tag(useractioner.handle_name.is + " 來到村莊大廳")
      case MTypeEnum.MESSAGE_LEAVE       => simple_message_tag(useractioner.handle_name.is +" 離開這個村莊了")
      case MTypeEnum.MESSAGE_KICKED      => simple_message_tag(useractioner.handle_name.is +" 人間蒸發、被轉學了")
      case MTypeEnum.MESSAGE_REVOTE0     => simple_message_tag("＜投票重新開始 請儘速重新投票＞")
      case MTypeEnum.MESSAGE_REVOTE      => simple_message_tag("＜投票結果有問題 請重新投票＞")
      case MTypeEnum.MESSAGE_TIMEOUT     => simple_message_tag(useractioner.handle_name.is +" 超過時限，自動行動")
      case MTypeEnum.ACTION_KICK         => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 投票踢出", true, "kick-do")
      
      case MTypeEnum.ACTION_TEAM_ASSIGN  => 
        val userentrys_rr = UserEntry.rr(userentrys)
        val useractionee_list2 = talk.message_flags.is.map(x => userentrys_rr(x.toString.toInt).handle_name.is)
        val useractionees2 = useractionee_list2.reduceLeft(_ + " , " + _)
          
        simple_message_tag(useractioner.handle_name.is +  " 選擇成員為 " + useractionees2 , true, "team-assign")  
      case MTypeEnum.ACTION_TEAM_VOTE_YES    => 
        simple_message_tag(useractioner.handle_name.is + " 投下贊成票", reveal_mode, "team-vote")
      case MTypeEnum.ACTION_TEAM_VOTE_NO    => 
        simple_message_tag(useractioner.handle_name.is + " 投下反對票", reveal_mode, "team-vote")
      case MTypeEnum.ACTION_MISSION      => 
        simple_message_tag(useractioner.handle_name.is + " 建置結界", reveal_mode, "mission-vote")
      case MTypeEnum.ACTION_HINDER_MISSION      => 
        simple_message_tag(useractioner.handle_name.is + " 妨礙建置結界", reveal_mode, "mission-hinder")  
      case MTypeEnum.ACTION_BITE         => 
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 為鎖定目標", true, "bite")
        
      case MTypeEnum.ITEM_CRYSTALBALL   => 
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用水晶球", true, "item")   
        
      case MTypeEnum.RESULT_TEAM_VOTE      =>
        val roomphase_id = talk.message.is.toLong
        val votes = Vote.findAll(By(Vote.roomphase_id, roomphase_id))
        simple_message_tag(VoteHelper.team_vote_table(RoomPhase.find(By(RoomPhase.id, roomphase_id)).get, userentrys, votes))
      case MTypeEnum.RESULT_MISSION        =>
        val roomphase_id = talk.message.is.toLong
        val votes = Vote.findAll(By(Vote.roomphase_id, roomphase_id))
        if (reveal_mode)
          simple_message_tag(VoteHelper.mission_vote_table(userentrys, votes))
        else {
          val votes_yes = votes.filter(_.vote_yes.is)
          val votes_no  = votes.filter(!_.vote_yes.is)
          simple_message_tag("建置結界：" + votes_yes.length + " 人，妨礙：" + votes_no.length + " 人", true, "bite")
        }
        
      case MTypeEnum.RESULT_CRYSTALBALL   => 
        simple_message_tag(" 你發現 " + useractionee.handle_name.is + " 是 " + 
          RoleSideEnum.get_roleside_cname(useractionee.get_role.role_side) + " 側" , 
          (reveal_mode || (useractioner.id.is == currentuserentry_id)), "item")   
        
        
      case xs => NodeSeq.Empty
    }
  }
  
  // Message Table
  def messages_normal(room: Room, roomround : RoomRound, userentrys: List[UserEntry], reveal:Boolean) : NodeSeq = {
    //val roomround = RoomRound_R.get
    var talks =  Talk.findAll(By(Talk.roomround_id, roomround.id.is), OrderBy(Talk.id, Descending))
    
    var lastround_id = roomround.last_round.is
    while(lastround_id != 0) {
      talks = talks ++ Talk.findAll(By(Talk.roomround_id, lastround_id), OrderBy(Talk.id, Descending))
      val lastround = RoomRound.find(By(RoomRound.id, lastround_id)).get
      lastround_id = lastround.last_round.is
    }
    
    if (roomround.round_no.is == 0) {
      val revotes = talks.filter(_.mtype.is == MTypeEnum.MESSAGE_REVOTE0.toString)

      // 新增 投票重新開始 50 次時廢村
      if ((revotes.length >= 50) || (talks.length >= 1000)) {
        //val room = Room_R.get
        /*
        room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
        room.save
            
        RoomActor ! SessionVarSet(room = room)
        RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
        */
       GameProcessor.abandon(room)
      }
    }
    
    // border="0" cellpadding="0" cellspacing="0"
    Seq(<table class="talk"> <tbody id="talk-tbody">{
        for (talk <- talks) yield talk_tag(talk, userentrys, reveal)
      } </tbody></table>)
  }
  
  // Message Table
  def messages_leftright(room: Room, roomround : RoomRound, userentrys: List[UserEntry], reveal:Boolean) 
    : (NodeSeq, NodeSeq) = {
    //val roomround = RoomRound_R.get
    var talks =  Talk.findAll(By(Talk.roomround_id, roomround.id.is), OrderBy(Talk.id, Descending))
    
    var lastround_id = roomround.last_round.is
    while(lastround_id != 0) {
      talks = talks ++ Talk.findAll(By(Talk.roomround_id, lastround_id), OrderBy(Talk.id, Descending))
      val lastround = RoomRound.find(By(RoomRound.id, lastround_id)).get
      lastround_id = lastround.last_round.is
    }
    
    if (roomround.round_no.is == 0) {
      val revotes = talks.filter(_.mtype.is == MTypeEnum.MESSAGE_REVOTE0.toString)

      // 新增 投票重新開始 50 次時廢村
      if ((revotes.length >= 50) || (talks.length >= 1000)) {
        //val room = Room_R.get
        /*
        room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
        room.save
            
        RoomActor ! SessionVarSet(room = room)
        RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
        */
       GameProcessor.abandon(room)
      }
    }
    
    val talks_left  = talks.filter(_.mtype.is != MTypeEnum.RESULT_TEAM_VOTE.toString)
    val talks_right = talks.filter(t => (t.mtype.is == MTypeEnum.RESULT_TEAM_VOTE.toString) ||
                                        (t.mtype.is == MTypeEnum.RESULT_MISSION.toString))
    
    val result_left = 
      Seq(<table class="talk"> <tbody id="talk-tbody-left">{
          for (talk <- talks_left) yield talk_tag(talk, userentrys, reveal)
        } </tbody></table>)
    val result_right = 
      Seq(<table class="talk"> <tbody id="talk-tbody-right">{
          for (talk <- talks_right) yield {
            if (talk.mtype.is == MTypeEnum.RESULT_MISSION.toString) {
              val roomphase_id = talk.message.is.toLong
              val votes = Vote.findAll(By(Vote.roomphase_id, roomphase_id))
              VoteHelper.mission_vote_table_right(userentrys, votes)
            } else
              talk_tag(talk, userentrys, reveal)
          }
        } </tbody></table>)
        
    (result_left, result_right)
  }
  
  //val amountRange = Expense.findAll(   BySql("amount between ? and ?", lowVal, highVal))
  
  def messages_all(room_id : Long, userentrys: List[UserEntry], reveal:Boolean) : NodeSeq = {
    val talks        =  Talk.findAllByPreparedStatement({ superconn =>
      val statement = superconn.connection.prepareStatement(
      "select * from Talk join RoomRound on Talk.roomround_id = RoomRound.id where RoomRound.room_id = ? order by Talk.id desc")
      statement.setString(1, room_id.toString)
      statement
    })
    
    // border="0" cellpadding="0" cellspacing="0"
    Seq(<table class="talk"> <tbody id="talk-tbody">{
        for (talk <- talks) yield talk_tag(talk, userentrys, reveal)
      } </tbody></table>)
  }
}


/*
<tr>
<td class="system-user" colspan="2"><img src="img/icon/hum.gif"> clojure 來到了幻想鄉</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/msg.gif"> ＜投票結果重置，請重新投票＞</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/spy.gif"> 映 被踢出村莊了</td>
</tr>
<tr class="system-message">
<td class="kick-do" colspan="2">映 對 映 投票踢出</td>
</tr>
<tr class="system-message">
<td class="kick-do" colspan="2">映 對 右代宮  戰人 投票踢出</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/msg.gif"> ＜投票結果重置，請重新投票＞</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/spy.gif"> 天晴 被踢出村莊了</td>
</tr>
<tr class="system-message">
<td class="kick-do" colspan="2">天晴 對 天晴 投票踢出</td>
</tr>
<tr class="user-talk">
<td class="user-name"><font style="color:#00DD77">◆</font>天晴</td>
<td class="say normal">「鑽石開始了 我先自刪 掰」</td>
</tr> */