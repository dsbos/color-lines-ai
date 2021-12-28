package com.us.dsb.explore.algs.walmart

import java.time.LocalTime

case class SchedulingResults(nextOrder: Option[Order],
                              ordersSoFar: List[Order],  // TODO: Purge; no longer needed
                              schedule: List[ScheduleStep[LocalTime]],
                              npsPct: Float)
