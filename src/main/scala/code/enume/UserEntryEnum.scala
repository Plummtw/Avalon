package org.plummtw.avalon.enume

object UserEntryRoomFlagEnum extends Enumeration {
  type UserEntryRoomFlagEnum = Value
  
  val  VOTED     = Value("V")
  val  AUTOVOTED = Value("A")
  val  SKIPPED    = Value("S")
  //val  SUDDENDEATH = Value("D")
}

object UserEntryRoleFlagEnum extends Enumeration {
  type UserEntryRoleFlagEnum = Value
  
}

object UserEntryFlagEnum extends Enumeration {
  type UserEntryFlagEnum = Value
}


