package com.us.dsb.wm

import java.time.LocalTime

case class SchedulingResults(nextOrder: Option[Order],
                              ordersSoFar: List[Order],  // TODO: Purge; no longer needed
                              schedule: List[ScheduleStep[LocalTime]],
                              npsPct: Float)
