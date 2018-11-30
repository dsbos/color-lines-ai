package com.us.dsb.wm

import org.scalatest.FunSuite

import java.time.LocalTime

class OrdersSchedulerTest extends FunSuite {

  implicit class NameNotUsed(localTimeString: String) {
    def toTime: LocalTime = LocalTime.parse(localTimeString)
    def t: LocalTime = toTime

  }

  test("set of zero orders should yield zero-length schedule with no NPS") {
    val actual = OrdersScheduler.scheduleAllOrders(Nil)
    val expected = SchedulingResults(None, Nil, Nil, Float.NaN)
    assertResult(expected.nextOrder  )(actual.nextOrder)
    assertResult(expected.ordersSoFar)(actual.ordersSoFar)
    assertResult(expected.schedule   )(actual.schedule)
    assertResult(true)(actual.npsPct.isNaN)
  }

  test("single, simple, zero-distance order ...") {
    val simpleOrder = Order("Test1", "06:00:00".t, 0, 0)
    val ordersIn = List(simpleOrder)
    val actual = OrdersScheduler.scheduleAllOrders(ordersIn)
    val expectedSchedule =
      List(ScheduleStep("06:00".t, "Test1", "06:00".t, Some("06:00".t), 1, 0, 0, "06:00".t))

    assertResult(Some(simpleOrder))(actual.nextOrder)
    assertResult(Nil)(actual.ordersSoFar)  //?? broken (off by one); purge that field anyway?
    assertResult(expectedSchedule)(actual.schedule)
    assertResult(100)(actual.npsPct)
  }

  test("order just in promoter-time range should not drop to neutral") {
    assertResult(1.5, "- parameter changed?")(SatisfactionParameters.P_VS_N_BOUNDARY_HOURS)

    val simpleOrder = Order("Test1", "06:00:00".t, (1.5 * 60 - 1).toInt, 0)
    val ordersIn = List(simpleOrder)
    val actual = OrdersScheduler.scheduleAllOrders(ordersIn)
    val expectedSchedule =
      List(ScheduleStep("06:00".t, "Test1", "06:00".t, Some("07:29".t), 1, 0, 0, "08:58".t))

    assertResult(Some(simpleOrder))(actual.nextOrder)
    assertResult(Nil)(actual.ordersSoFar)
    assertResult(expectedSchedule)(actual.schedule)
    assertResult(100)(actual.npsPct)
  }

  test("order far enough away should drop from promoter to neutral") {
    assertResult(1.5, "- parameter changed?")(SatisfactionParameters.P_VS_N_BOUNDARY_HOURS)

    val simpleOrder = Order("Test1", "06:00:00".t, (1.5 * 60 + 1).toInt, 0)
    val ordersIn = List(simpleOrder)
    val actual = OrdersScheduler.scheduleAllOrders(ordersIn)
    val expectedSchedule =
      List(ScheduleStep("06:00".t, "Test1", "06:00".t, Some("07:31".t), 0, 1, 0, "09:02".t))

    assertResult(Some(simpleOrder))(actual.nextOrder)
    assertResult(Nil)(actual.ordersSoFar)
    assertResult(expectedSchedule)(actual.schedule)
    assertResult(0)(actual.npsPct)
  }

  test("order far enough away should drop from to detractor") {
    assertResult(3.5, "- parameter changed?")(SatisfactionParameters.N_VS_D_BOUNDARY_HOURS)

    val simpleOrder = Order("Test1", "06:00:00".t, (3.5 * 60 + 1).toInt, 0)
    val ordersIn = List(simpleOrder)
    val actual = OrdersScheduler.scheduleAllOrders(ordersIn)
    val expectedSchedule =
      List(ScheduleStep("06:00".t, "Test1", "06:00".t, Some("09:31".t), 0, 0, 1, "13:02".t))

    assertResult(Some(simpleOrder))(actual.nextOrder)
    assertResult(Nil)(actual.ordersSoFar)
    assertResult(expectedSchedule)(actual.schedule)
    assertResult(-100)(actual.npsPct)
  }

  test("???single, simple, zero-distance order ...") {
    /*
      two orders
      one almost far away enough to drop - 1.5 hr * 60 s/hr = 90,
      second closer, bu...

     */

    val order1 = Order.decode("WM0001 N0E85 06:00:00")
    val order2 = Order.decode("WM0002 N0E1 06:00:00")

    val ordersIn = List(order1, order2)
    val actual = OrdersScheduler.scheduleAllOrders(ordersIn)
    val expectedSchedule: List[ScheduleStep[LocalTime]] =
      List(
        ScheduleStep("06:00".t, "WM0002", "06:00".t, Some("06:01".t), 1, 0, 0, "06:02".t),
        ScheduleStep("06:02".t, "WM0001", "06:02".t, Some("07:27".t), 2, 0, 0, "08:52".t)
      )

    assertResult(Some(order2))(actual.nextOrder)  //????
    assertResult(Nil)(actual.ordersSoFar)  //?? broken (off by one); purge that field anyway?
    assertResult(expectedSchedule)(actual.schedule)
    assertResult(100)(actual.npsPct)
  }

  test("order coming in at T2 after drone availability at T1 does _not_ start delivery at T1") {
    val simpleOrder = Order("Test1", "07:00:00".t, 11, 0)
    val ordersIn = List(simpleOrder)
    val actual = OrdersScheduler.scheduleAllOrders(ordersIn)
    val expectedSchedule =
      List(ScheduleStep("06:00".t, "Test1", "07:00".t, Some("07:11".t), 1, 0, 0, "07:22".t))

    assertResult(Some(simpleOrder))(actual.nextOrder)
    assertResult(Nil)(actual.ordersSoFar)  //?? broken (off by one); purge that field anyway?
    assertResult(expectedSchedule)(actual.schedule)
    assertResult(100)(actual.npsPct)
  }

  test("unpreventable-detractor order should be deferred until after other orders") {

    // 3.5 hr * 60 = 210 min
    val ordersIn =
      List(Order.decode("WM0001 N222E222 06:00:00"),
           Order.decode("WM0002 N0E2 07:00:00"),
           Order.decode("WM0003 N0E3 07:01:00"),
           Order.decode("WM0004 N0E4 07:02:00"),
           Order.decode("WM0005 N0E5 07:03:00"),
           Order.decode("WM0006 N0E6 07:04:00"),
           Order.decode("WM0007 N0E7 07:05:00")
      )

    val actual = OrdersScheduler.scheduleAllOrders(ordersIn)
    val expectedSchedule =
      List(ScheduleStep("06:00".t, "xxxTest1", "07:00".t, Some("07:11".t), 1, 0, 0, "07:22".t))

    assertResult(
      List(
        ("WM0002", "07:00:00".t),
        ("WM0003", "07:04:00".t),
        ("WM0004", "07:10:00".t),
        ("WM0005", "07:18:00".t),
        ("WM0006", "07:28:00".t),
        ("WM0007", "07:40:00".t),
        ("WM0001", "07:54:00".t)
      )
    ) {
      actual.schedule.map(s => (s.orderId, s.departureTime))
    }

    assertResult(Nil)(actual.ordersSoFar)
    // 6 promotors, 1 detractor, 7 orders, (6 - 1) / 7 * 100%  = 5/7 ~= 71.42857
    assertResult(100f * 5 / 7)(actual.npsPct)  // (Float, not Double)
  }

}
