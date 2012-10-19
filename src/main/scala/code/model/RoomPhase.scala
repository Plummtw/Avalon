package org.plummtw.avalon.model

import net.liftweb._
import net.liftweb.mapper._
//import net.liftweb.util.FieldError

import net.liftweb.http.RequestVar
import net.liftweb.http.SessionVar

//import scala.util.matching.Regex

//import org.plummtw.avalon.enume._
import org.plummtw.avalon.util.PlummUtil

object RoomPhase_E extends SessionVar[RoomPhase](null)
object RoomPhase_R extends SessionVar[RoomPhase](null)

class RoomPhase extends LongKeyedMapper[RoomPhase] with CreatedUpdated with IdPK {
  def getSingleton = RoomPhase // what's the "meta" object

  object roomround_id  extends MappedLongForeignKey(this, RoomRound)
  
  object phase_no       extends MappedInt(this)
  object phase_round    extends MappedInt(this)  // 最多 5 round
  object phase_type     extends MappedString(this, 2)
  //object phase_subtype  extends MappedString(this, 4)
  object leader         extends MappedLongForeignKey(this, UserEntry) 
  object assigned       extends MappedString(this, 20)
  
  object deadline       extends MappedDateTime(this) {
    override def defaultValue = PlummUtil.dateAddSecond(new java.util.Date(), 15)
  }
  
  object additional     extends MappedInt(this)
  object phase_flags    extends MappedString(this, 20)
}

object RoomPhase extends RoomPhase with LongKeyedMetaMapper[RoomPhase] {
  override def fieldOrder = List(id, roomround_id, phase_no, phase_round,
                                 phase_type, leader, assigned, 
                                 deadline, additional, phase_flags)
}
