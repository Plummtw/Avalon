package org.plummtw.avalon.model

import net.liftweb._
import net.liftweb.mapper._
import net.liftweb.util.FieldError
import net.liftweb.http.{SHtml, SessionVar, RequestVar}
import net.liftweb.common.{Empty, Box, Full}

import scala.xml.NodeSeq
import scala.util.matching.Regex

import org.plummtw.avalon.enume._
import org.plummtw.avalon.data._
import org.plummtw.avalon.util.PlummUtil
//import org.plummtw.avalon.heavy.GameProcessor

object CurrentUserEntry   extends SessionVar[Box[UserEntry]](Empty)
object CurrentUserEntry_E extends SessionVar[UserEntry](GlobalUserEntry.NoUserEntry)
object CurrentUserEntry_R extends SessionVar[UserEntry](GlobalUserEntry.NoUserEntry)

object UserEntrys_E        extends SessionVar[List[UserEntry]](List())
object UserEntrys_ER       extends SessionVar[List[UserEntry]](List())
object UserEntrys_R        extends SessionVar[List[UserEntry]](List())
object UserEntrys_RR       extends SessionVar[List[UserEntry]](List())


class UserEntry extends LongKeyedMapper[UserEntry] with CreatedUpdated with IdPK {
  def getSingleton = UserEntry // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  //object id            extends MappedLongIndex(this)
  object room_id       extends MappedLongForeignKey(this, Room)
  object user_id       extends MappedLongForeignKey(this, User)
  
  object user_icon_id  extends MappedLongForeignKey(this, UserIcon) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Long): List[FieldError] = 
      if (in == 0)  List(FieldError(this, <b>尚未選擇圖像</b>)) 
      else Nil
  }
  
  object user_no       extends MappedInt(this)
  // Login 用 id
  object uname         extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() <  6)          List(FieldError(this, <b>帳號過短＜６</b>))
             else if (in.length() > 20)   List(FieldError(this, <b>帳號過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>帳號包含控制碼</b>)) else Nil).flatten
  }

  // 顯示用暱名
  object handle_name   extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() <  1)          List(FieldError(this, <b>暱稱過短＜１</b>))
             else if (in.length() > 20)   List(FieldError(this, <b>暱稱過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>暱稱包含控制碼</b>)) else Nil).flatten
  }
  
  // trip
  object trip         extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() > 20)          List(FieldError(this, <b>ｔｒｉｐ過長＞２０</b>))
           else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>ｔｒｉｐ包含控制碼</b>)) else Nil).flatten
  }
  
  object password      extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() < 6)           List(FieldError(this, <b>密碼過短＜６</b>))
             else if (in.length() > 20)   List(FieldError(this, <b>密碼過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>密碼包含控制碼</b>)) else Nil).flatten
  }
    
  object sex           extends MappedString(this, 1)  {
    override def validations = validPriority _ :: super.validations
    override def defaultValue = "M"

    val sex_map: Map[String, String] = Map("M"->"男", "F"->"女")
    def sex_radios =
      SHtml.radio(sex_map.keys.toList, Full(UserEntry.this.sex.toString), UserEntry.this.sex(_))

    def generateHtml = sex_radios.flatMap(PlummUtil.htmlize(_, sex_map))

    def validPriority(in: String): List[FieldError] =
      if (!sex_map.contains(in)) List(FieldError(this, <b>性別錯誤</b>))
      else Nil
  }
  
  object role          extends MappedString(this, 2)
  object subrole       extends MappedString(this, 4)
  
  object damaged     extends MappedInt(this)
  
  object revoked      extends MappedBoolean(this) {
    override def defaultValue = false
  }
  
  object last_words    extends MappedString(this, 600) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length() > 600)  List(FieldError(this, <b>遺言過長＞６００</b>))
      else Nil
  }
  
  object ip_address0     extends MappedString(this, 20) with LifecycleCallbacks {
    override def beforeCreate = {
      this(ip_address.is)
    }
  }
  object ip_address     extends MappedString(this, 20)
  object ip_address_md5 extends MappedString(this, 34) with LifecycleCallbacks {
    override def beforeCreate = {
      this(PlummUtil.generateMD5(ip_address.is))
    }
  }

  object last_round_no extends MappedInt(this)
  object last_talk      extends MappedString(this, 600)

  object room_flags    extends MappedString(this, 20)
  object role_flags    extends MappedString(this, 20)
  object user_flags    extends MappedString(this, 20)
  object item_flags    extends MappedString(this, 20)
  
  def get_user_icon : UserIcon = {
    UserIconCache.getOr(user_icon_id.is) { () => 
      UserIcon.find(By(UserIcon.id, user_icon_id.is)) match {
        case Full(x) => x
        case x       => UserIcon.find(By(UserIcon.id, 1)).get
    }}
  }
  
  def get_role = {
    val role_str =
      if (role.is.length > 2) role.is.substring(0,2)
      else role.is
        
    RoleEnum.get_role(role_str)
  }
  
  def get_role_field = {
    var result : NodeSeq = 
      if (role.is.length > 2) Seq(RoleEnum.get_role(role.is.substring(0,2)).cfield)
      else Seq(RoleEnum.get_role(role.is).cfield)
      
    for (i <- 1 until (role.is.length / 2))
      result = result ++ RoleEnum.get_role(role.is.substring(i*2,i*2+2)).simple_cfield
      
    result
  }
  
  def has_room_flag(flag : UserEntryRoomFlagEnum.Value) : Boolean = 
    return (room_flags.is.indexOf(flag.toString) != -1)
  def hasnt_room_flag(flag : UserEntryRoomFlagEnum.Value) : Boolean = 
    !has_room_flag(flag)
  def add_room_flag(flag : UserEntryRoomFlagEnum.Value) : UserEntry = 
    room_flags(room_flags.is + flag.toString)
  def remove_room_flag(flag : UserEntryRoomFlagEnum.Value) : UserEntry = 
    room_flags(room_flags.is.replace(flag.toString, ""))
  
  def has_role_flag(flag : UserEntryRoleFlagEnum.Value) : Boolean = 
    return (role_flags.is.indexOf(flag.toString) != -1)
  def hasnt_role_flag(flag : UserEntryRoleFlagEnum.Value) : Boolean = 
    !has_role_flag(flag)
  def add_role_flag(flag : UserEntryRoleFlagEnum.Value) : UserEntry = 
    role_flags(role_flags.is + flag.toString)
  def remove_role_flag(flag : UserEntryRoleFlagEnum.Value) : UserEntry = 
    role_flags(role_flags.is.replace(flag.toString, ""))
  
  def has_user_flag(flag : UserEntryFlagEnum.Value) : Boolean = 
    return (user_flags.is.indexOf(flag.toString) != -1)
  def hasnt_user_flag(flag : UserEntryFlagEnum.Value) : Boolean = 
    !has_user_flag(flag)
  def add_user_flag(flag : UserEntryFlagEnum.Value) : UserEntry = 
    user_flags(user_flags.is + flag.toString)
  def remove_user_flag(flag : UserEntryFlagEnum.Value) : UserEntry = 
    user_flags(user_flags.is.replace(flag.toString, ""))

  def has_item_flag(flag : ItemFlagEnum.Value) : Boolean = 
    return (item_flags.is.indexOf(flag.toString) != -1)
  def hasnt_item_flag(flag : ItemFlagEnum.Value) : Boolean = 
    !has_item_flag(flag)
  def add_item_flag(flag : ItemFlagEnum.Value) : UserEntry = 
    item_flags(item_flags.is + flag.toString)
  def remove_item_flag(flag : ItemFlagEnum.Value) : UserEntry = 
    item_flags(item_flags.is.replace(flag.toString, ""))

    
  //def items =
  //  item_flags.is.grouped(3).toList.map(x => CardEnum.get_card(x))

}

object UserEntry extends UserEntry with LongKeyedMetaMapper[UserEntry] {
  override def fieldOrder = List(id, room_id, user_id, user_icon_id, user_no, uname, handle_name, 
                                 trip,  password, sex, role, subrole, damaged, revoked,
                                 ip_address0, ip_address, ip_address_md5,
                                 last_round_no, last_talk, 
                                 room_flags, role_flags, user_flags, item_flags)

  def get (get_id : Long, userentrys : List[UserEntry]) : UserEntry= {
    if (get_id <= 0) {
      get_id match {
        case -1 => GlobalUserEntry.AdminUserEntry
        case  _ => GlobalUserEntry.NoUserEntry
      }
    } else
      userentrys.find(_.id.is == get_id).getOrElse(GlobalUserEntry.NoUserEntry)
  }
  
  def rrnc(currentuserentry : UserEntry, userentrys : List[UserEntry]) =
    userentrys.filter(x =>  (!x.revoked.is) && (x != currentuserentry))
  
  def rr(userentrys : List[UserEntry]) =
    userentrys.filter(!_.revoked.is)
  
  def findAllByRoom (room : Room) : List[UserEntry] = {
    if ((room.status.is == RoomStatusEnum.WAITING.toString)) // ||
        // (room.hasnt_flag(RoomFlagEnum.RANDOM_POSITION)))
      UserEntry.findAll(By(UserEntry.room_id, room.id.is))
    else
      UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                        OrderBy(UserEntry.user_no, Ascending))
  }
}

object GlobalUserEntry{
  val NoUserEntry    = UserEntry.create.handle_name("無").user_icon_id(1)
  val AdminUserEntry = UserEntry.create.handle_name("管理員").user_icon_id(1)
  
  val GLOBAL_USERENTRY_LIST = List(NoUserEntry, AdminUserEntry)
}
