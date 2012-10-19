package org.plummtw.avalon.model

import net.liftweb._
import net.liftweb.mapper._
//import net.liftweb.util.FieldError

import net.liftweb.http.RequestVar
import net.liftweb.http.SessionVar

//import scala.util.matching.Regex

//import org.plummtw.avalon.enume._
//import org.plummtw.avalon.util.PlummUtil

object RoomRound_E extends SessionVar[RoomRound](null)
object RoomRound_R extends SessionVar[RoomRound](null)

class RoomRound extends LongKeyedMapper[RoomRound] with CreatedUpdated with IdPK {
  def getSingleton = RoomRound // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object room_id       extends MappedLongForeignKey(this, Room)
  object last_round    extends MappedLongForeignKey(this, RoomRound) 
  
  object round_no      extends MappedInt(this)
}

object RoomRound extends RoomRound with LongKeyedMetaMapper[RoomRound] {
  override def fieldOrder = List(id, room_id, last_round, round_no) //, player_no, round_step, reaction_player, reaction_type, deadline
}

