package org.plummtw.avalon.util

import net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._

import scala.xml.NodeSeq

import org.plummtw.avalon.model._
import org.plummtw.avalon.enume._
import org.plummtw.avalon.data._
//import org.plummtw.avalon.card._


object UserEntryHelper {
  // http://74.82.5.143/
  // http://identicon.relucks.org/
  def user_cell(room : Room, roomphase: RoomPhase, currentuserentry: UserEntry, userentry: UserEntry, reveal: Boolean ) : NodeSeq = {
    //val room = Room_R.get
    //val roomround = RoomRound_R.get
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = userentry.get_user_icon
    val id_icon : NodeSeq =
      if ((room.status.is != RoomStatusEnum.PLAYING.toString) || (userentry.ip_address.is != userentry.ip_address0.is))
        Seq(<img src={AdminHelper.identicon_link + userentry.ip_address_md5.is} />)
      else
        NodeSeq.Empty

    result ++= <td valign="top" class={if ((roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString) &&
                                           (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED))) "voted" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /><br/></td>
    

    // <img src={"http://identicon.relucks.org/" + user_entry.ip_address_md5.is} />
    val result2 = <td class={if ((roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString) &&
                                 (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED))) "voted" 
                             else if (userentry.id.is == roomphase.leader.is) "leader-mark" 
                             else ""}>
          <font color={user_icon.color.is}>◆</font><span>{userentry.handle_name.is}</span><br/>
          { if (userentry.trip.is != "")
              Seq(<span>◆</span>, <a target="_blank" class="decor_none" href={AdminHelper.trip_link + userentry.trip.is}>{userentry.trip.is}</a>, <br/>)
            else
              NodeSeq.Empty }
          { if (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString) <strong>[？？]</strong>
            else if (reveal || (currentuserentry == userentry)) <strong>{userentry.get_role_field}</strong>
            else if ((currentuserentry.get_role.role_side == RoleSideEnum.WEREWOLF) &&
                     (userentry.get_role.role_side == RoleSideEnum.WEREWOLF) &&
                     (userentry.get_role != RoleSilverwolf)) <strong>{RoleWerewolf.cfield}</strong>
            else if ((currentuserentry.get_role == RoleAugurer) &&
                     (userentry.get_role.role_side == RoleSideEnum.WEREWOLF) &&
                     (userentry.get_role != RoleWhitewolf))
              <strong>{RoleWerewolf.cfield}</strong>
            else if ((currentuserentry.get_role == RoleGuard) &&
                     ((userentry.get_role == RoleAugurer) ||
                      (userentry.get_role == RolePhantomwolf)))
              <strong>{RoleAugurer.cfield}</strong>
            else <strong>[？？]</strong>}
          <br/>{id_icon}</td>
    
    result ++ result2
  }


  def user_admin_cell(room: Room, roomphase: RoomPhase, userentry: UserEntry ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = userentry.get_user_icon
    val id_icon : NodeSeq =
      if ((room.status.is != RoomStatusEnum.PLAYING.toString) || (userentry.ip_address.is != userentry.ip_address0.is))
        Seq(<img src={AdminHelper.identicon_link + userentry.ip_address_md5.is} />)
      else
        NodeSeq.Empty

    result ++= <td valign="top" class={if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /><br/></td>

    // <img src={"http://identicon.relucks.org/" + user_entry.ip_address_md5.is} />
    val result2 = <td class={if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" 
                             else if (userentry.id.is == roomphase.leader.is) "leader-mark" 
                             else ""}>
          <font color={user_icon.color.is}>◆</font>{userentry.handle_name.is}<br/>
          { if (userentry.trip.is != "")
              Seq(<span>◆</span>, <a target="_blank" class="decor_none" href={AdminHelper.trip_link + userentry.trip.is}>{userentry.trip.is}</a>, <br/>)
            else
              NodeSeq.Empty }
           <strong>{userentry.get_role_field}</strong>
          <br/>
          {Seq(<input type="checkbox" id={"id" + userentry.user_no.is} name={"id" + userentry.user_no.is} />)}
          {id_icon}</td>
    
    result ++ result2
  }

  def user_select_cell(userentry : UserEntry, node : NodeSeq ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = userentry.get_user_icon
    
    result ++= <td valign="top">
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString}
           border="2" style={"border-color:" + user_icon.color.is} /></td>

    result ++= <td width="150px" >
      {userentry.handle_name.is}<br/><font color={user_icon.color.is}>◆</font>
      {node}</td>
    result
  }
  
  // User Table
  def user_table(room:Room, roomphase:RoomPhase, currentuserentry:UserEntry, 
                 userentrys:List[UserEntry], reveal: Boolean) : NodeSeq = {
    //val userentrys = UserEntrys_RR.get
    val user_groups = userentrys.grouped(5).toList
    //room:Room, current_user:UserEntry, user_entrys: List[UserEntry], 

    <table border="0" cellpadding="0" cellspacing="5" class="userentry_table"> <tbody>   
    { for (user_group <- user_groups) yield <tr> { 
       for (userentry <- user_group) yield user_cell(room, roomphase, currentuserentry, userentry, reveal)
    } </tr> } </tbody></table> 
  }

  def user_select_table(userentrys: List[UserEntry], targettable_list: List[UserEntry], callback : String => Unit) : NodeSeq = {
    assert(! targettable_list.isEmpty)
        
    //val userentrys = UserEntrys_RR.get
    val user_groups = userentrys.grouped(5).toList
        
    val targettable_id_list = targettable_list.map(_.id.is.toString)
    val targettable_radios = SHtml.radio(targettable_id_list, Full(targettable_id_list(0)), callback(_))
    
        
    <table border="0" cellpadding="0" cellspacing="5" class="userentry_table"><tbody>
    { for (user_group <- user_groups) yield <tr> { 
       for (userentry <- user_group) yield {
         val index = targettable_list.indexOf(userentry)
         if (index != -1)
           user_select_cell(userentry, targettable_radios(index))
         else
           user_select_cell(userentry, NodeSeq.Empty)
       }
    } </tr> } </tbody></table> }
      
  def user_choose_table(userentrys: List[UserEntry], targettable_list: List[UserEntry], callback : Long => Unit) : NodeSeq = {
    assert(! targettable_list.isEmpty)
        
    //val userentrys = UserEntrys_RR.get
    val user_groups = userentrys.grouped(5).toList
        
    val targettable_id_list = targettable_list.map(_.id.is.toString)
    //val targettable_radios = SHtml.radio(targettable_id_list, Full(targettable_id_list(0)), callback(_))
    
        
    <table border="0" cellpadding="0" cellspacing="5" class="userentry_table"><tbody>
    { for (user_group <- user_groups) yield <tr> { 
       for (userentry <- user_group) yield {
         val index = targettable_list.indexOf(userentry)
         if (index != -1) {
           val targettable_checkbox = SHtml.checkbox(false, if (_) callback(userentry.id.is)) 
           user_select_cell(userentry, targettable_checkbox)
         } else
           user_select_cell(userentry, NodeSeq.Empty)
       }
    } </tr> } </tbody></table> }
          
          
  // User Admin Table
  def user_admin_table(room:Room, roomphase:RoomPhase, userentrys: List[UserEntry]) : NodeSeq = {
    val user_groups = userentrys.grouped(5).toList

    return <table border="0" cellpadding="0" cellspacing="5" style="userentry_table">
    { for (user_group <- user_groups) yield <tr> {
       for (userentry <- user_group) yield user_admin_cell(room, roomphase, userentry)
    } </tr> } </table> }
}


