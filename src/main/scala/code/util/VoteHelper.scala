package org.plummtw.avalon.util

import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import S._
import SHtml._
import Helpers._

import collection.mutable.{HashMap, SynchronizedMap}

import org.plummtw.avalon.model._
import org.plummtw.avalon.enume._
import org.plummtw.avalon.actor._

object VoteHelper {
  def team_vote_table(roomphase : RoomPhase, userentrys : List[UserEntry], votes : List[Vote]) = {
    val votes_rr = votes.sortWith((x,y) => UserEntry.get(x.actioner_id.is, userentrys).user_no.is < 
                                           UserEntry.get(y.actioner_id.is, userentrys).user_no.is)
    val userentrys_rr = UserEntry.rr(userentrys)
    val useractionee_list2 = roomphase.assigned.is.map(x => userentrys_rr(x.toString.toInt).handle_name.is)
    val useractionees2 = useractionee_list2.reduceLeft(_ + " , " + _)
          
    val table_head  = UserEntry.get(roomphase.leader.is, userentrys).handle_name.is +  " 選擇成員：" + useractionees2 
                                           
    <table class="team-vote-table">
      <th colspan="2">{table_head}</th>
    {votes_rr.map { vote =>
      val actioner = UserEntry.get(vote.actioner_id.is, userentrys)
      <tr><td>{actioner.handle_name.is}</td><td>{if (vote.vote_yes.is) "贊成" else "反對" }</td></tr>
    }}</table>
  }
  
  def mission_vote_table(userentrys : List[UserEntry], votes : List[Vote]) = {
    val votes_rr = votes.sortWith((x,y) => UserEntry.get(x.actioner_id.is, userentrys).user_no.is < 
                                           UserEntry.get(y.actioner_id.is, userentrys).user_no.is)
    <table class="mission-table">{votes_rr.map { vote =>
      val actioner = UserEntry.get(vote.actioner_id.is, userentrys)
      <tr><td>{actioner.handle_name.is}</td><td>{if (vote.vote_yes.is) "建置結界" else "妨礙建置結界" }</td></tr>
    }}</table>
  }
}