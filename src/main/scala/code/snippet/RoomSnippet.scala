package org.plummtw.avalon.snippet

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

import org.plummtw.avalon.enume._
import org.plummtw.avalon.model._
import org.plummtw.avalon.util._

class RoomListSnippet extends Logger {
  def render(in : NodeSeq) = {
    val room_list  = Room.findAll(By(Room.status, RoomStatusEnum.WAITING.toString), OrderBy(Room.id,Descending)) :::
                     Room.findAll(By(Room.status, RoomStatusEnum.PLAYING.toString), OrderBy(Room.id,Descending))
                     
  
    room_list.flatMap { room => Seq(
      <a href={"login.html?room_no=" + room.id}>
      {
        if (room.status.is == RoomStatusEnum.WAITING.toString)
          <img class="option" src="images/waiting.gif" alt="等待中" title="等待中"/>
        else
          <img class="option" src="images/playing.gif" alt="進行中" title="進行中"/>
      }
      <span> [{room.id}號村] </span>{room.room_name}村<br/><div class="room_comment">～ {room.room_comment} ～
      [指派:{room.team_assign_time}][投票:{room.team_vote_time}][建置:{room.mission_time}][狼咬:{room.bite_time}]
      [ {room.max_user}人用 ] </div></a>, 
      <div class="room_option">村莊選項：{room.option_text}</div>, <br/>) }
  }
}

// 創新村莊的 Lock，以免村莊數超過村莊上限
object RoomCreateLock {}

class RoomCreateSnippet extends StatefulSnippet with Logger{
  private var room_name        = ""
  private var room_comment     = ""
  private var max_user         = 10
  private var team_assign_time = 90
  private var team_vote_time   = 120
  private var mission_time     = 45
  private var bite_time        = 120
  private var item_time        = 60
  
  def dispatch = {
    case _ => render
  }

  def render =
  {
    var option_list : List[RoomFlagEnum.Value] = List()

    def process() {
      debug("In Process")
      
      val room_flags : String= option_list.distinct.map(_.toString).mkString("",",","")
      
      val room = Room.create.room_name(room_name.replace('　',' ').trim()).room_comment(room_comment.replace('　',' ').trim()).max_user(max_user)
                     .team_assign_time(team_assign_time).team_vote_time(team_vote_time)
                     .mission_time(mission_time).bite_time(bite_time).item_time(item_time)
                     .room_flags(room_flags).status(RoomStatusEnum.WAITING.toString).victory("")
                     
      room.validate match {
        case Nil => ;
        case xs  => S.error(xs) //; return redirectTo("main.html")
      }
      
      // 加入大廳
      val game_hall = RoomRound.create.round_no(0)
      
      val room_params = AdminManage.findAll(Like(AdminManage.param_name, "room%"))

      val current_time =  new java.util.GregorianCalendar
      val current_hour =  current_time.get(java.util.Calendar.HOUR_OF_DAY)

      val room_start =
          try { room_params.filter(_.param_name.is == "room_start")(0).param_value.is.toInt }
          catch { case _ => AdminHelper.DEFAULT_ROOM_START}
      val room_end =
          try { room_params.filter(_.param_name.is == "room_end")(0).param_value.is.toInt }
          catch { case _ => AdminHelper.DEFAULT_ROOM_END}

      if ((current_hour >= room_end) || (current_hour< room_start)) {
        S.error((room_end.toString) + ":00 - " + (room_start.toString) +":00 請不要開村")
        // return redirectTo("main.html")
      }
      
      RoomCreateLock.synchronized {
      
        val room_count  = Room.count(By(Room.status, RoomStatusEnum.WAITING.toString)) +
                          Room.count(By(Room.status, RoomStatusEnum.PLAYING.toString))
        val room_count_limit =
          try { room_params.filter(_.param_name.is == "room_count")(0).param_value.is.toInt }
          catch { case _ => AdminHelper.DEFAULT_ROOM_COUNT}

        if (room_count >= room_count_limit) {
          S.error("超過村數上限") //; return redirectTo("main.html")
        }
                          
        room.save

        game_hall.room_id(room.id.is)
        game_hall.save
        
        val room_phase = RoomPhase.create.roomround_id(game_hall.id.is).phase_no(0).phase_type(RoomPhaseEnum.GAMEHALL.toString)
        room_phase.deadline(PlummUtil.dateAddMinute(new java.util.Date(), 10))
        
        room_phase.save
        
        val talk = Talk.create.roomround_id(game_hall.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                       .message("村莊建立 " + (new java.util.Date).toString)
        talk.save
      }

      /*
      try {
        val plurk_client = new PlurkClient(plurk_apiKey)     // 建立 SPlurk 物件
        plurk_client.Users.login (plurk_username, plurk_password)  // 登入噗浪

        // 發噗
        plurk_client.Timeline.plurkAdd (
          qualifier = Qualifier.Says,  // 設定噗文前的修飾詞（說、喜歡、正在……等）
          content   = "第" + room.id.is.toString + "號村已建立",  // 噗文的內容
          language  = Some(Language.tr_ch)  // 修飾詞的語言（tr_ch 為中文）
        )
      } catch { case _ =>  S.notice("Plurk 發佈失敗") }
      */

      
      S.notice(room.id.toString() + "號村已建立") 
    }
    
    "name=room_name"      #> SHtml.text(room_name, x => room_name = x) & 
    "name=room_comment"   #> SHtml.text(room_comment, x => room_comment = x) & 
    "name=max_user"       #> SHtml.select(Seq(("5","5"),("6","6"),("7","7"),("8","8"),("9","9"),("10","10")),
                             Full(max_user.toString),  x => asInt(x).foreach(y => (max_user = y))) &
    "name=team_assign_time" #> SHtml.text(team_assign_time.toString,     s => asInt(s).foreach(x => team_assign_time = x)) &
    "name=team_vote_time"   #> SHtml.text(team_vote_time.toString,   s => asInt(s).foreach(x => team_vote_time = x)) &
    "name=mission_time"     #> SHtml.text(mission_time.toString, s => asInt(s).foreach(x => mission_time = x)) &
    "name=bite_time"        #> SHtml.text(bite_time.toString, s => asInt(s).foreach(x => bite_time = x)) &
    "name=item_time"        #> SHtml.text(item_time.toString, s => asInt(s).foreach(x => item_time = x)) &
    "name=test_mode"      #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.TEST_MODE)) &
    "name=wish_align"     #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WISH_ALIGN)) &
    "name=wish_role"      #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WISH_ROLE)) &
    "name=random_position" #> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.RANDOM_POSITION)) &
    "name=no_role"         #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_ROLE)) &
    "name=role_guard"      #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_GUARD)) &
    "name=role_whitewolf"   #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_WHITEWOLF)) &
    "name=role_silverwolf"  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_SILVERWOLF)) &
    "name=role_phantomwolf" #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_PHANTOMWOLF)) &
    //"type=submit"         #> SHtml.onSubmitUnit(() => debug("TEST")) 
     "type=submit"         #> SHtml.onSubmitUnit(S.callOnce(process))
  }
}
