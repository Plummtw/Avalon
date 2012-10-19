package org.plummtw.avalon.enume

import org.plummtw.avalon.data._

object MTypeEnum extends Enumeration {
  type MTypeEnum = Value
  
  val NONE                = Value("")
  
  val VOTE                = Value("V")
  
  val TALK                = Value("T")
  val TALK_DAY            = Value("TD")
  val TALK_ADMIN          = Value("TA")
  val TALK_ADMIN_PRIVATE  = Value("TAP")
  val TALK_END            = Value("TE")
  
  val MESSAGE              = Value("S")
  //val MESSAGE_HIDDEN       = Value("SH")
  val MESSAGE_GENERAL      = Value("S0")
  val MESSAGE_COME         = Value("S1")
  val MESSAGE_LEAVE        = Value("S2")
  val MESSAGE_KICKED       = Value("S3")
  val MESSAGE_REVOTE0      = Value("S4")
  val MESSAGE_REVOTE       = Value("S5")
  val MESSAGE_TIMEOUT      = Value("S6")
  
  val RESULT_TEAM_ASSIGN   = Value("RTA")
  val RESULT_TEAM_VOTE     = Value("RTV")
  val RESULT_MISSION       = Value("RMI")
  val RESULT_BITE          = Value("RBI")

  val OBJECTION_MALE       = Value("OM")
  val OBJECTION_FEMALE     = Value("OF")
  
  val ACTION_TEAM_ASSIGN   = Value("ATA")
  val ACTION_TEAM_VOTE_YES = Value("ATY")
  val ACTION_TEAM_VOTE_NO  = Value("ATN")
  val ACTION_MISSION       = Value("AMI")
  val ACTION_HINDER_MISSION = Value("AHM")
  val ACTION_BITE          = Value("ABI")
  
  val ACTION_TEST_ALERT    = Value("A0")
  val ACTION_KICK          = Value("AK")
  val ACTION_STARTGAME     = Value("AS")
  
  def get_action(string : String) : MTypeEnum.Value = {
    try {MTypeEnum.withName(string)}
    catch {case e: Exception => NONE}
  }
  
  implicit def mtypeenum2String (en : MTypeEnum.Value) : String = en.toString
}