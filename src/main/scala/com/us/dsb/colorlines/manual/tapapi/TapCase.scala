package com.us.dsb.colorlines.manual.tapapi

/** Interpreted meaning/command of a (virtual) tap on a board cell. */
private[manual] sealed trait TapCase derives CanEqual

private[manual] object TapCase {

  /** Tap selects cell that has ball. */
  private[manual] case object SelectBallTap  extends TapCase

  /** Tap selects cell that is empty. */
  private[manual] case object SelectEmptyTap extends TapCase

  /** Tap cancels selection. */
  private[manual] case object DeselectTap    extends TapCase

  /** Tap makes game move to move previously selected ball (if open path) */
  private[manual] case object TryMoveBallTap extends TapCase

  /** Tap makes game move to pass (move nothing and get more balls placed). */
  private[manual] case object PassTap        extends TapCase
}
