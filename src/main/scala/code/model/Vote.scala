package org.plummtw.avalon.model 
 
import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._

class Vote extends LongKeyedMapper[Vote] with CreatedUpdated with IdPK {
  def getSingleton = Vote // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object roomphase_id  extends MappedLongForeignKey(this, RoomPhase) 
  object actioner_id   extends MappedLongForeignKey(this, UserEntry)
  object actionee_id   extends MappedLongForeignKey(this, UserEntry)

  object vote_yes      extends MappedBoolean(this)  
  object vote_flags    extends MappedString(this, 20)

}

object Vote extends Vote with LongKeyedMetaMapper[Vote] {
  override def fieldOrder = List(id, roomphase_id, actioner_id, actionee_id, vote_yes, vote_flags)
}

/*
class Vote {
  //static  belongsTo = [room_day: RoomDay, voter:UserEntry, target:UserEntry]
  
  //RoomDay   room_day
  UserEntry voter
  UserEntry target
  
  Integer vote_number
  Integer vote_time
  String  situation
  String  vote_flag
  
  static constraints = {
    voter(nullable:false)
    target(nullable:true)
    
    vote_number(nullable:true,  min:1, max:999)
    vote_time(nullable:false,  min:1, max:99)
    
    situation(nullable:false, maxSize:2)
    vote_flag(nullable:true,  maxSize:1)
    
    created(nullable:true)
    updated(nullable:true)
  }
  
  Date created
  Date updated
  def beforeInsert = {
    created = new Date()
  }
  def beforeUpdate = {
    updated = new Date()
  }}
*/

/*
      	`room_no` smallint(5) unsigned NOT NULL default '0',
				`date` tinyint(3) unsigned default NULL,
				`uname` varchar(20) default NULL,
				`target_uname` varchar(20) default NULL,
				`vote_number` int(11) default NULL,
				`vote_times` int(11) default NULL,
				`situation` text,
				KEY `room_no` (`room_no`,`date`)
*/